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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
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

#include "src/event/event-internal.h"
#include "src/pmix/pmix-internal.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/ess.h"
#include "src/mca/odls/odls_types.h"
#include "src/rml/rml.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"
#include "src/util/name_fns.h"

#include "iof_hnp.h"
#include "src/mca/iof/base/base.h"
#include "src/mca/iof/base/iof_base_setup.h"

/* LOCAL FUNCTIONS */
static void stdin_write_handler(int fd, short event, void *cbdata);

/* API FUNCTIONS */
static int init(void);

static int hnp_push(const pmix_proc_t *dst_name, prte_iof_tag_t src_tag, int fd);

static int hnp_pull(const pmix_proc_t *src_name, prte_iof_tag_t src_tag, int fd);

static int hnp_close(const pmix_proc_t *peer, prte_iof_tag_t source_tag);

static void hnp_complete(const prte_job_t *jdata);

static int finalize(void);

static int push_stdin(const pmix_proc_t *dst_name, uint8_t *data, size_t sz);

/* The API's in this module are solely used to support LOCAL
 * procs - i.e., procs that are co-located to the HNP. Remote
 * procs interact with the HNP's IOF via the HNP's receive function,
 * which operates independently and is in the iof_hnp_receive.c file
 */

prte_iof_base_module_t prte_iof_hnp_module = {
    .init = init,
    .push = hnp_push,
    .pull = hnp_pull,
    .close = hnp_close,
    .complete = hnp_complete,
    .finalize = finalize,
    .push_stdin = push_stdin
};

/* Initialize the module */
static int init(void)
{
    /* post non-blocking recv to catch forwarded IO from
     * the orteds
     */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_IOF_HNP,
                  PRTE_RML_PERSISTENT, prte_iof_hnp_recv, NULL);

    PMIX_CONSTRUCT(&prte_mca_iof_hnp_component.procs, pmix_list_t);

    return PRTE_SUCCESS;
}

/* Setup to read local data.
 */
static int hnp_push(const pmix_proc_t *dst_name, prte_iof_tag_t src_tag, int fd)
{
    prte_iof_proc_t *proct;
    int flags;

    /* don't do this if the dst vpid is invalid or the fd is negative! */
    if (PMIX_RANK_INVALID == dst_name->rank || fd < 0) {
        return PRTE_SUCCESS;
    }

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                         "%s iof:hnp pushing fd %d for process %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), fd, PRTE_NAME_PRINT(dst_name)));

    /* do we already have this process in our list? */
    PMIX_LIST_FOREACH(proct, &prte_mca_iof_hnp_component.procs, prte_iof_proc_t)
    {
        if (PMIX_CHECK_PROCID(&proct->name, dst_name)) {
            /* found it */
            goto SETUP;
        }
    }
    /* if we get here, then we don't yet have this proc in our list */
    proct = PMIX_NEW(prte_iof_proc_t);
    PMIX_XFER_PROCID(&proct->name, dst_name);
    pmix_list_append(&prte_mca_iof_hnp_component.procs, &proct->super);

SETUP:
    /* for stdout/stderr, set the file descriptor to non-blocking - do this before we setup
     * and activate the read event in case it fires right away
     */
    if ((flags = fcntl(fd, F_GETFL, 0)) < 0) {
        pmix_output(prte_iof_base_framework.framework_output,
                    "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", __FILE__, __LINE__, errno);
    } else {
        flags |= O_NONBLOCK;
        fcntl(fd, F_SETFL, flags);
    }

    /* define a read event and activate it */
    if (src_tag & PRTE_IOF_STDOUT) {
        PRTE_IOF_READ_EVENT(&proct->revstdout, proct, fd, PRTE_IOF_STDOUT,
                            prte_iof_hnp_read_local_handler, false);
    } else if (src_tag & PRTE_IOF_STDERR) {
        PRTE_IOF_READ_EVENT(&proct->revstderr, proct, fd, PRTE_IOF_STDERR,
                            prte_iof_hnp_read_local_handler, false);
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

/* Push data to stdin of a client process
 *
 * (a) a specific name, usually vpid=0; or
 *
 * (b) all procs, specified by vpid=PRTE_VPID_WILDCARD
 *
 */
static int push_stdin(const pmix_proc_t *dst_name, uint8_t *data, size_t sz)
{
    pmix_proc_t p;
    prte_iof_proc_t *proct;
    int rc;

    /* don't do this if the dst vpid is invalid */
    if (PMIX_RANK_INVALID == dst_name->rank) {
        return PRTE_SUCCESS;
    }

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                         "%s iof:hnp pushing stdin to process %s (size %zu)",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(dst_name), sz));

    /* if I am the DVM master and this is a wildcard name, then we have to
     * broadcast this to all daemons */
    if (PMIX_RANK_WILDCARD == dst_name->rank) {
        PMIX_LOAD_PROCID(&p, PRTE_PROC_MY_NAME->nspace, PMIX_RANK_WILDCARD);
        rc = prte_iof_hnp_send_data_to_endpoint(&p, dst_name,
                                                PRTE_IOF_STDIN,
                                                data, sz);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* lookup the daemon hosting the target proc */
    PMIX_LOAD_PROCID(&p, PRTE_PROC_MY_NAME->nspace, PMIX_RANK_WILDCARD);
    p.rank = prte_get_proc_daemon_vpid(dst_name);
    if (PMIX_RANK_INVALID == p.rank) {
        PRTE_ERROR_LOG(PRTE_ERR_ADDRESSEE_UNKNOWN);
        return PRTE_ERR_ADDRESSEE_UNKNOWN;
    }

    /* if the host isn't me, send it to them */
    if (p.rank != PRTE_PROC_MY_NAME->rank) {
        /* send the data to the daemon so it can
         * write it to the proc's fd - in this case,
         * we pass dst_name to indicate who is to
         * receive the data. If the connection closed,
         * numbytes will be zero so zero bytes will be
         * sent - this will tell the daemon to close
         * the fd for stdin to that proc
         */
        rc = prte_iof_hnp_send_data_to_endpoint(&p, dst_name,
                                                PRTE_IOF_STDIN,
                                                data, sz);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
        }
        return rc;
    }

    /* local proc - see if we have this process in our list */
    PMIX_LIST_FOREACH(proct, &prte_mca_iof_hnp_component.procs, prte_iof_proc_t)
    {
        if (PMIX_CHECK_PROCID(&proct->name, dst_name)) {
            /* did they direct that the data go to this proc? */
            if (NULL == proct->stdinev) {
                /* nope - ignore it */
                continue;
            }

            /* send the bytes down the pipe - we even send 0 byte events
             * down the pipe so it forces out any preceding data before
             * closing the output stream
             */
            if (NULL != proct->stdinev->wev) {
                if (PRTE_IOF_MAX_INPUT_BUFFERS < prte_iof_base_write_output(&proct->name,
                                                                            PRTE_IOF_STDIN, data, sz,
                                                                            proct->stdinev->wev)) {
                    /* getting too backed up - stop the read event for now if it is still active */

                    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                                         "buffer backed up - holding"));
                    return PRTE_ERR_OUT_OF_RESOURCE;
                }
            }
        }
    }

    return PRTE_SUCCESS;
}

/*
 * Since we are the HNP, the only "pull" call comes from a local
 * process so we can record the file descriptor for its stdin.
 */

static int hnp_pull(const pmix_proc_t *dst_name, prte_iof_tag_t src_tag, int fd)
{
    prte_iof_proc_t *proct;
    int flags;

    /* this is a local call - only stdin is supported */
    if (PRTE_IOF_STDIN != src_tag) {
        return PRTE_ERR_NOT_SUPPORTED;
    }

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                         "%s iof:hnp pulling fd %d for process %s",
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
    PMIX_LIST_FOREACH(proct, &prte_mca_iof_hnp_component.procs, prte_iof_proc_t)
    {
        if (PMIX_CHECK_PROCID(&proct->name, dst_name)) {
            /* found it */
            goto SETUP;
        }
    }
    /* if we get here, then we don't yet have this proc in our list */
    proct = PMIX_NEW(prte_iof_proc_t);
    PMIX_XFER_PROCID(&proct->name, dst_name);
    pmix_list_append(&prte_mca_iof_hnp_component.procs, &proct->super);

SETUP:
    PRTE_IOF_SINK_DEFINE(&proct->stdinev, dst_name, fd, PRTE_IOF_STDIN, stdin_write_handler);
    PMIX_XFER_PROCID(&proct->stdinev->daemon, PRTE_PROC_MY_NAME);
    PRTE_IOF_SINK_ACTIVATE(proct->stdinev->wev);

    return PRTE_SUCCESS;
}

/*
 * One of our local procs wants us to close the specifed
 * stream(s), thus terminating any potential io to/from it.
 */
static int hnp_close(const pmix_proc_t *peer, prte_iof_tag_t source_tag)
{
    prte_iof_proc_t *proct;

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                         "%s iof:hnp closing connection to process %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(peer)));

    PMIX_LIST_FOREACH(proct, &prte_mca_iof_hnp_component.procs, prte_iof_proc_t)
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
                pmix_list_remove_item(&prte_mca_iof_hnp_component.procs, &proct->super);
                PMIX_RELEASE(proct);
            }
            break;
        }
    }
    return PRTE_SUCCESS;
}

static void hnp_complete(const prte_job_t *jdata)
{
    prte_iof_proc_t *proct, *next;

    /* cleanout any lingering sinks */
    PMIX_LIST_FOREACH_SAFE(proct, next, &prte_mca_iof_hnp_component.procs, prte_iof_proc_t)
    {
        if (PMIX_CHECK_NSPACE(jdata->nspace, proct->name.nspace)) {
            pmix_list_remove_item(&prte_mca_iof_hnp_component.procs, &proct->super);
            if (NULL != proct->revstdout) {
                PMIX_RELEASE(proct->revstdout);
            }
            proct->revstdout = NULL;
            if (NULL != proct->revstderr) {
                PMIX_RELEASE(proct->revstderr);
            }
            proct->revstderr = NULL;
            PMIX_RELEASE(proct);
        }
    }
}

static int finalize(void)
{
    PMIX_DESTRUCT(&prte_mca_iof_hnp_component.procs);
    return PRTE_SUCCESS;
}

/* this function is called by the event library and thus
 * can access information global to the state machine
 */
static void stdin_write_handler(int fd, short event, void *cbdata)
{
    prte_iof_sink_t *sink = (prte_iof_sink_t *) cbdata;
    prte_iof_write_event_t *wev = sink->wev;
    pmix_list_item_t *item;
    prte_iof_write_output_t *output;
    int num_written, total_written = 0;
    PRTE_HIDE_UNUSED_PARAMS(fd, event);

    PMIX_ACQUIRE_OBJECT(sink);

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                         "%s hnp:stdin:write:handler writing %d data to %d",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         (int)pmix_list_get_size(&wev->outputs), wev->fd));

    wev->pending = false;

    while (NULL != (item = pmix_list_remove_first(&wev->outputs))) {
        output = (prte_iof_write_output_t *) item;
        /* if an abnormal termination has occurred, just dump
         * this data as we are aborting
         */
        if (prte_abnormal_term_ordered) {
            PMIX_RELEASE(output);
            continue;
        }
        if (0 == output->numbytes) {
            /* this indicates we are to close the fd - there is
             * nothing to write
             */
            PMIX_OUTPUT_VERBOSE((20, prte_iof_base_framework.framework_output,
                                 "%s iof:hnp closing fd %d on write event due to zero bytes output",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), wev->fd));
            goto finish;
        }
        num_written = write(wev->fd, output->data, output->numbytes);
        PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                             "%s hnp:stdin:write:handler wrote %d bytes",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), num_written));
        if (num_written < 0) {
            if (EAGAIN == errno || EINTR == errno) {
                /* push this item back on the front of the list */
                pmix_list_prepend(&wev->outputs, item);
                /* leave the write event running so it will call us again
                 * when the fd is ready.
                 */
                goto re_enter;
            }
            /* otherwise, something bad happened so all we can do is declare an
             * error and abort
             */
            PMIX_RELEASE(output);
            PMIX_OUTPUT_VERBOSE(
                (20, prte_iof_base_framework.framework_output,
                 "%s iof:hnp closing fd %d on write event due to negative bytes written",
                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), wev->fd));
            goto finish;
        } else if (num_written < output->numbytes) {
            PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                                 "%s hnp:stdin:write:handler incomplete write %d - adjusting data",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), num_written));
            /* incomplete write - adjust data to avoid duplicate output */
            memmove(output->data, &output->data[num_written], output->numbytes - num_written);
            /* push this item back on the front of the list */
            pmix_list_prepend(&wev->outputs, item);
            /* leave the write event running so it will call us again
             * when the fd is ready.
             */
            goto re_enter;
        }
        PMIX_RELEASE(output);

        total_written += num_written;
        if ((PRTE_IOF_SINK_BLOCKSIZE <= total_written) && wev->always_writable) {
            goto re_enter;
        }
    }
    goto check;

re_enter:
    PRTE_IOF_SINK_ACTIVATE(wev);

check:
    if (sink->closed && 0 == pmix_list_get_size(&wev->outputs)) {
        /* the sink has already been closed and everything was written, time to release it */
        PMIX_RELEASE(sink);
    }
    return;

finish:
    PMIX_RELEASE(wev);
    sink->wev = NULL;
    return;
}
