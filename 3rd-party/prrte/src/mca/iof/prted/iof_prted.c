/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2017-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"
#include "src/util/pmix_output.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <string.h>

#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#else
#    ifdef HAVE_SYS_FCNTL_H
#        include <sys/fcntl.h>
#    endif
#endif

#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_os_dirpath.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/odls/odls_types.h"
#include "src/rml/rml.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"
#include "src/util/name_fns.h"

#include "src/mca/iof/base/base.h"
#include "src/mca/iof/base/iof_base_setup.h"
#include "src/mca/iof/iof.h"

#include "iof_prted.h"

/* LOCAL FUNCTIONS */
static void stdin_write_handler(int fd, short event, void *cbdata);

/* API FUNCTIONS */
static int init(void);

static int prted_push(const pmix_proc_t *dst_name, prte_iof_tag_t src_tag, int fd);

static int prted_pull(const pmix_proc_t *src_name, prte_iof_tag_t src_tag, int fd);

static int prted_close(const pmix_proc_t *peer, prte_iof_tag_t source_tag);


static void prted_complete(const prte_job_t *jdata);

static int finalize(void);

/* The API's in this module are solely used to support LOCAL
 * procs - i.e., procs that are co-located to the daemon. Output
 * from local procs is automatically sent to the HNP for output
 * and possible forwarding to other requestors.
 */

prte_iof_base_module_t prte_iof_prted_module = {
    .init = init,
    .push = prted_push,
    .pull = prted_pull,
    .close = prted_close,
    .complete = prted_complete,
    .finalize = finalize,
};

static int init(void)
{
    /* post a non-blocking RML receive to get messages
     from the HNP IOF component */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_IOF_PROXY,
                  PRTE_RML_PERSISTENT, prte_iof_prted_recv, NULL);

    /* setup the local global variables */
    PMIX_CONSTRUCT(&prte_mca_iof_prted_component.procs, pmix_list_t);
    prte_mca_iof_prted_component.xoff = false;

    return PRTE_SUCCESS;
}

/**
 * Push data from the specified file descriptor
 * to the HNP
 */

static int prted_push(const pmix_proc_t *dst_name, prte_iof_tag_t src_tag, int fd)
{
    int flags;
    prte_iof_proc_t *proct;
    prte_job_t *jobdat = NULL;

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                         "%s iof:prted pushing fd %d for process %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), fd, PRTE_NAME_PRINT(dst_name)));

    /* set the file descriptor to non-blocking - do this before we setup
     * and activate the read event in case it fires right away
     */
    if ((flags = fcntl(fd, F_GETFL, 0)) < 0) {
        pmix_output(prte_iof_base_framework.framework_output,
                    "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", __FILE__, __LINE__, errno);
    } else {
        flags |= O_NONBLOCK;
        fcntl(fd, F_SETFL, flags);
    }

    /* do we already have this process in our list? */
    PMIX_LIST_FOREACH(proct, &prte_mca_iof_prted_component.procs, prte_iof_proc_t)
    {
        if (PMIX_CHECK_PROCID(&proct->name, dst_name)) {
            /* found it */
            goto SETUP;
        }
    }
    /* if we get here, then we don't yet have this proc in our list */
    proct = PMIX_NEW(prte_iof_proc_t);
    PMIX_XFER_PROCID(&proct->name, dst_name);
    pmix_list_append(&prte_mca_iof_prted_component.procs, &proct->super);

SETUP:
    /* get the local jobdata for this proc */
    if (NULL == (jobdat = prte_get_job_data_object(proct->name.nspace))) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        return PRTE_ERR_NOT_FOUND;
    }
    /* define a read event and activate it */
    if (src_tag & PRTE_IOF_STDOUT) {
        PRTE_IOF_READ_EVENT(&proct->revstdout, proct, fd, PRTE_IOF_STDOUT,
                            prte_iof_prted_read_handler, false);
    } else if (src_tag & PRTE_IOF_STDERR) {
        PRTE_IOF_READ_EVENT(&proct->revstderr, proct, fd, PRTE_IOF_STDERR,
                            prte_iof_prted_read_handler, false);
    }

    /* if -all- of the readevents for this proc have been defined, then
     * activate them. Otherwise, we can think that the proc is complete
     * because one of the readevents fires -prior- to all of them having
     * been defined!
     */
    if (NULL != proct->revstdout &&
        NULL != proct->revstderr) {
        if (!proct->revstdout->activated) {
            PRTE_IOF_READ_ACTIVATE(proct->revstdout);
            proct->revstdout->activated = true;
        }
        if (!proct->revstderr->activated) {
            PRTE_IOF_READ_ACTIVATE(proct->revstderr);
            proct->revstderr->activated = true;
        }
    }
    return PRTE_SUCCESS;
}

/**
 * Pull for a daemon tells
 * us that any info we receive from the HNP that is targeted
 * for stdin of the specified process should be fed down the
 * indicated file descriptor. Thus, all we need to do here
 * is define a local endpoint so we know where to feed anything
 * that comes to us
 */

static int prted_pull(const pmix_proc_t *dst_name, prte_iof_tag_t src_tag, int fd)
{
    prte_iof_proc_t *proct;
    prte_ns_cmp_bitmask_t mask = PRTE_NS_CMP_ALL;
    int flags;

    /* this is a local call - only stdin is suppprted */
    if (PRTE_IOF_STDIN != src_tag) {
        return PRTE_ERR_NOT_SUPPORTED;
    }

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                         "%s iof:prted pulling fd %d for process %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), fd, PRTE_NAME_PRINT(dst_name)));

    /* set the file descriptor to non-blocking - do this before we setup
     * the sink in case it fires right away
     */
    if ((flags = fcntl(fd, F_GETFL, 0)) < 0) {
        pmix_output(prte_iof_base_framework.framework_output,
                    "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", __FILE__, __LINE__, errno);
    } else {
        flags |= O_NONBLOCK;
        fcntl(fd, F_SETFL, flags);
    }

    /* do we already have this process in our list? */
    PMIX_LIST_FOREACH(proct, &prte_mca_iof_prted_component.procs, prte_iof_proc_t)
    {
        if (PRTE_EQUAL == prte_util_compare_name_fields(mask, &proct->name, dst_name)) {
            /* found it */
            goto SETUP;
        }
    }
    /* if we get here, then we don't yet have this proc in our list */
    proct = PMIX_NEW(prte_iof_proc_t);
    PMIX_XFER_PROCID(&proct->name, dst_name);
    pmix_list_append(&prte_mca_iof_prted_component.procs, &proct->super);

SETUP:
    PRTE_IOF_SINK_DEFINE(&proct->stdinev, dst_name, fd, PRTE_IOF_STDIN, stdin_write_handler);

    return PRTE_SUCCESS;
}

/*
 * One of our local procs wants us to close the specifed
 * stream(s), thus terminating any potential io to/from it.
 * For the prted, this just means closing the local fd
 */
static int prted_close(const pmix_proc_t *peer, prte_iof_tag_t source_tag)
{
    prte_iof_proc_t *proct;

    PMIX_LIST_FOREACH(proct, &prte_mca_iof_prted_component.procs, prte_iof_proc_t)
    {
        if (PMIX_CHECK_PROCID(&proct->name, peer)) {
            if (PRTE_IOF_STDIN & source_tag) {
                if (NULL != proct->stdinev) {
                    PMIX_RELEASE(proct->stdinev);
                }
                proct->stdinev = NULL;
            }
            if ((PRTE_IOF_STDOUT & source_tag) || (PRTE_IOF_STDMERGE & source_tag)) {
                if (NULL != proct->revstdout) {
                    PMIX_RELEASE(proct->revstdout);
                }
                proct->revstdout = NULL;
            }
            if (PRTE_IOF_STDERR & source_tag) {
                if (NULL != proct->revstderr) {
                    PMIX_RELEASE(proct->revstderr);
                }
                proct->revstderr = NULL;
            }
            /* if we closed them all, then remove this proc */
            if (NULL == proct->stdinev && NULL == proct->revstdout && NULL == proct->revstderr) {
                pmix_list_remove_item(&prte_mca_iof_prted_component.procs, &proct->super);
                PMIX_RELEASE(proct);
            }
            break;
        }
    }

    return PRTE_SUCCESS;
}

static void prted_complete(const prte_job_t *jdata)
{
    prte_iof_proc_t *proct, *next;

    /* cleanout any lingering sinks */
    PMIX_LIST_FOREACH_SAFE(proct, next, &prte_mca_iof_prted_component.procs, prte_iof_proc_t)
    {
        if (PMIX_CHECK_NSPACE(jdata->nspace, proct->name.nspace)) {
            pmix_list_remove_item(&prte_mca_iof_prted_component.procs, &proct->super);
            PMIX_RELEASE(proct);
        }
    }
}

static int finalize(void)
{
    PMIX_LIST_DESTRUCT(&prte_mca_iof_prted_component.procs);

    /* Cancel the RML receive */
    PRTE_RML_CANCEL(PRTE_NAME_WILDCARD, PRTE_RML_TAG_IOF_PROXY);
    return PRTE_SUCCESS;
}

static void stdin_write_handler(int _fd, short event, void *cbdata)
{
    prte_iof_sink_t *sink = (prte_iof_sink_t *) cbdata;
    prte_iof_write_event_t *wev = sink->wev;
    pmix_list_item_t *item;
    prte_iof_write_output_t *output;
    int num_written;
    PRTE_HIDE_UNUSED_PARAMS(_fd, event);

    PMIX_ACQUIRE_OBJECT(sink);

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                         "%s prted:stdin:write:handler writing data to %d",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), wev->fd));

    wev->pending = false;

    while (NULL != (item = pmix_list_remove_first(&wev->outputs))) {
        output = (prte_iof_write_output_t *) item;
        if (0 == output->numbytes) {
            /* this indicates we are to close the fd - there is
             * nothing to write
             */
            PMIX_OUTPUT_VERBOSE(
                (20, prte_iof_base_framework.framework_output,
                 "%s iof:prted closing fd %d on write event due to zero bytes output",
                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), wev->fd));
            PMIX_RELEASE(wev);
            sink->wev = NULL;
            return;
        }
        num_written = write(wev->fd, output->data, output->numbytes);
        PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                             "%s prted:stdin:write:handler wrote %d bytes",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), num_written));
        if (num_written < 0) {
            if (EAGAIN == errno || EINTR == errno) {
                /* push this item back on the front of the list */
                pmix_list_prepend(&wev->outputs, item);
                /* leave the write event running so it will call us again
                 * when the fd is ready.
                 */
                PRTE_IOF_SINK_ACTIVATE(wev);
                goto CHECK;
            }
            /* otherwise, something bad happened so all we can do is declare an
             * error and abort
             */
            PMIX_RELEASE(output);
            PMIX_OUTPUT_VERBOSE(
                (20, prte_iof_base_framework.framework_output,
                 "%s iof:prted closing fd %d on write event due to negative bytes written",
                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), wev->fd));
            PMIX_RELEASE(wev);
            sink->wev = NULL;
            /* tell the HNP to stop sending us stuff */
            if (!prte_mca_iof_prted_component.xoff) {
                prte_mca_iof_prted_component.xoff = true;
                prte_iof_prted_send_xonxoff(PRTE_IOF_XOFF);
            }
            return;
        } else if (num_written < output->numbytes) {
            PMIX_OUTPUT_VERBOSE(
                (1, prte_iof_base_framework.framework_output,
                 "%s prted:stdin:write:handler incomplete write %d - adjusting data",
                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), num_written));
            /* incomplete write - adjust data to avoid duplicate output */
            memmove(output->data, &output->data[num_written], output->numbytes - num_written);
            /* push this item back on the front of the list */
            pmix_list_prepend(&wev->outputs, item);
            /* leave the write event running so it will call us again
             * when the fd is ready.
             */
            PRTE_IOF_SINK_ACTIVATE(wev);
            goto CHECK;
        }
        PMIX_RELEASE(output);
    }

CHECK:
    if (prte_mca_iof_prted_component.xoff) {
        /* if we have told the HNP to stop reading stdin, see if
         * the proc has absorbed enough to justify restart
         *
         * RHC: Note that when multiple procs want stdin, we
         * can get into a fight between a proc turnin stdin
         * back "on" and other procs turning it "off". There
         * is no clear way to resolve this as different procs
         * may take input at different rates.
         */
        if (pmix_list_get_size(&wev->outputs) < PRTE_IOF_MAX_INPUT_BUFFERS) {
            /* restart the read */
            prte_mca_iof_prted_component.xoff = false;
            prte_iof_prted_send_xonxoff(PRTE_IOF_XON);
        }
    }
}
