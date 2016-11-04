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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2016      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "opal/util/output.h"
#include "orte/constants.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <string.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#else
#ifdef HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif
#endif

#include "opal/util/os_dirpath.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/iof/base/iof_base_setup.h"

#include "iof_orted.h"


/* LOCAL FUNCTIONS */
static void stdin_write_handler(int fd, short event, void *cbdata);


/* API FUNCTIONS */
static int init(void);

static int orted_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd);

static int orted_pull(const orte_process_name_t* src_name,
                      orte_iof_tag_t src_tag,
                      int fd);

static int orted_close(const orte_process_name_t* peer,
                       orte_iof_tag_t source_tag);

static int finalize(void);

static int orted_ft_event(int state);

/* The API's in this module are solely used to support LOCAL
 * procs - i.e., procs that are co-located to the daemon. Output
 * from local procs is automatically sent to the HNP for output
 * and possible forwarding to other requestors. The HNP automatically
 * determines and wires up the stdin configuration, so we don't
 * have to do anything here.
 */

orte_iof_base_module_t orte_iof_orted_module = {
    init,
    orted_push,
    orted_pull,
    orted_close,
    NULL,
    finalize,
    orted_ft_event
};

static int init(void)
{
    /* post a non-blocking RML receive to get messages
     from the HNP IOF component */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                            ORTE_RML_TAG_IOF_PROXY,
                            ORTE_RML_PERSISTENT,
                            orte_iof_orted_recv,
                            NULL);

    /* setup the local global variables */
    OBJ_CONSTRUCT(&mca_iof_orted_component.procs, opal_list_t);
    mca_iof_orted_component.xoff = false;

    return ORTE_SUCCESS;
}

/**
 * Push data from the specified file descriptor
 * to the HNP
 */

static int orted_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd)
{
    int flags;
    orte_iof_proc_t *proct;
    orte_iof_sink_t *stdoutsink=NULL, *stderrsink=NULL, *stddiagsink=NULL;
    int rc;
    orte_job_t *jobdat=NULL;
    orte_ns_cmp_bitmask_t mask;

    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                         "%s iof:orted pushing fd %d for process %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         fd, ORTE_NAME_PRINT(dst_name)));

    /* set the file descriptor to non-blocking - do this before we setup
     * and activate the read event in case it fires right away
     */
    if((flags = fcntl(fd, F_GETFL, 0)) < 0) {
        opal_output(orte_iof_base_framework.framework_output, "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n",
                    __FILE__, __LINE__, errno);
    } else {
        flags |= O_NONBLOCK;
        fcntl(fd, F_SETFL, flags);
    }

    /* do we already have this process in our list? */
    OPAL_LIST_FOREACH(proct, &mca_iof_orted_component.procs, orte_iof_proc_t) {
        mask = ORTE_NS_CMP_ALL;

        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &proct->name, dst_name)) {
            /* found it */
            goto SETUP;
        }
    }
    /* if we get here, then we don't yet have this proc in our list */
    proct = OBJ_NEW(orte_iof_proc_t);
    proct->name.jobid = dst_name->jobid;
    proct->name.vpid = dst_name->vpid;
    opal_list_append(&mca_iof_orted_component.procs, &proct->super);
    /* get the local jobdata for this proc */
    if (NULL == (jobdat = orte_get_job_data_object(proct->name.jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    /* setup any requested output files */
    if (ORTE_SUCCESS != (rc = orte_iof_base_setup_output_files(dst_name, jobdat, proct, &stdoutsink, &stderrsink, &stddiagsink))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

SETUP:
    /* define a read event and activate it */
    if (src_tag & ORTE_IOF_STDOUT) {
        ORTE_IOF_READ_EVENT(&proct->revstdout, proct, fd, ORTE_IOF_STDOUT,
                            orte_iof_orted_read_handler, false);
        proct->revstdout->sink = stdoutsink;
    } else if (src_tag & ORTE_IOF_STDERR) {
        ORTE_IOF_READ_EVENT(&proct->revstderr, proct, fd, ORTE_IOF_STDERR,
                            orte_iof_orted_read_handler, false);
        proct->revstderr->sink = stderrsink;
    } else if (src_tag & ORTE_IOF_STDDIAG) {
        ORTE_IOF_READ_EVENT(&proct->revstddiag, proct, fd, ORTE_IOF_STDDIAG,
                            orte_iof_orted_read_handler, false);
        proct->revstddiag->sink = stddiagsink;
    }

    /* if -all- of the readevents for this proc have been defined, then
     * activate them. Otherwise, we can think that the proc is complete
     * because one of the readevents fires -prior- to all of them having
     * been defined!
     */
    if (NULL != proct->revstdout && NULL != proct->revstderr && NULL != proct->revstddiag) {
        proct->revstdout->active = true;
        opal_event_add(proct->revstdout->ev, 0);
        proct->revstderr->active = true;
        opal_event_add(proct->revstderr->ev, 0);
        proct->revstddiag->active = true;
        opal_event_add(proct->revstddiag->ev, 0);
    }
    return ORTE_SUCCESS;
}


/**
 * Pull for a daemon tells
 * us that any info we receive from the HNP that is targeted
 * for stdin of the specified process should be fed down the
 * indicated file descriptor. Thus, all we need to do here
 * is define a local endpoint so we know where to feed anything
 * that comes to us
 */

static int orted_pull(const orte_process_name_t* dst_name,
                      orte_iof_tag_t src_tag,
                      int fd)
{
    orte_iof_proc_t *proct;
    orte_ns_cmp_bitmask_t mask = ORTE_NS_CMP_ALL;
    int flags;

    /* this is a local call - only stdin is supported */
    if (ORTE_IOF_STDIN != src_tag) {
        return ORTE_ERR_NOT_SUPPORTED;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                         "%s iof:orted pulling fd %d for process %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         fd, ORTE_NAME_PRINT(dst_name)));

    /* set the file descriptor to non-blocking - do this before we setup
     * the sink in case it fires right away
     */
    if((flags = fcntl(fd, F_GETFL, 0)) < 0) {
        opal_output(orte_iof_base_framework.framework_output, "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n",
                    __FILE__, __LINE__, errno);
    } else {
        flags |= O_NONBLOCK;
        fcntl(fd, F_SETFL, flags);
    }

    /* do we already have this process in our list? */
    OPAL_LIST_FOREACH(proct, &mca_iof_orted_component.procs, orte_iof_proc_t) {
        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &proct->name, dst_name)) {
            /* found it */
            goto SETUP;
        }
    }
    /* if we get here, then we don't yet have this proc in our list */
    proct = OBJ_NEW(orte_iof_proc_t);
    proct->name.jobid = dst_name->jobid;
    proct->name.vpid = dst_name->vpid;
    opal_list_append(&mca_iof_orted_component.procs, &proct->super);

  SETUP:
    ORTE_IOF_SINK_DEFINE(&proct->stdinev, dst_name, fd, ORTE_IOF_STDIN,
                         stdin_write_handler);

    return ORTE_SUCCESS;
}


/*
 * One of our local procs wants us to close the specifed
 * stream(s), thus terminating any potential io to/from it.
 * For the orted, this just means closing the local fd
 */
static int orted_close(const orte_process_name_t* peer,
                       orte_iof_tag_t source_tag)
{
    orte_iof_proc_t* proct;
    orte_ns_cmp_bitmask_t mask = ORTE_NS_CMP_ALL;
    int cnt = 0;

    OPAL_LIST_FOREACH(proct, &mca_iof_orted_component.procs, orte_iof_proc_t) {
        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &proct->name, peer)) {
            if (ORTE_IOF_STDIN & source_tag) {
                if (NULL != proct->stdinev) {
                    OBJ_RELEASE(proct->stdinev);
                }
                ++cnt;
            }
            if (ORTE_IOF_STDOUT & source_tag) {
                if (NULL != proct->revstdout) {
                    orte_iof_base_static_dump_output(proct->revstdout);
                    OBJ_RELEASE(proct->revstdout);
                }
                ++cnt;
            }
            if (ORTE_IOF_STDERR & source_tag) {
                if (NULL != proct->revstderr) {
                    orte_iof_base_static_dump_output(proct->revstderr);
                    OBJ_RELEASE(proct->revstderr);
                }
                ++cnt;
            }
            if (ORTE_IOF_STDDIAG & source_tag) {
                if (NULL != proct->revstddiag) {
                    orte_iof_base_static_dump_output(proct->revstddiag);
                    OBJ_RELEASE(proct->revstddiag);
                }
                ++cnt;
            }
            /* if we closed them all, then remove this proc */
            if (4 == cnt) {
                opal_list_remove_item(&mca_iof_orted_component.procs, &proct->super);
                OBJ_RELEASE(proct);
            }
            break;
        }
    }

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    orte_iof_proc_t *proct;

    /* cycle thru the procs and ensure all their output was delivered
     * if they were writing to files */
    while (NULL != (proct = (orte_iof_proc_t*)opal_list_remove_first(&mca_iof_orted_component.procs))) {
        if (NULL != proct->revstdout) {
            orte_iof_base_static_dump_output(proct->revstdout);
        }
        if (NULL != proct->revstderr) {
            orte_iof_base_static_dump_output(proct->revstderr);
        }
        if (NULL != proct->revstddiag) {
            orte_iof_base_static_dump_output(proct->revstddiag);
        }
        OBJ_RELEASE(proct);
    }
    OBJ_DESTRUCT(&mca_iof_orted_component.procs);

    /* Cancel the RML receive */
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_IOF_PROXY);
    return ORTE_SUCCESS;
}

/*
 * FT event
 */

static int orted_ft_event(int state)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static void stdin_write_handler(int fd, short event, void *cbdata)
{
    orte_iof_sink_t *sink = (orte_iof_sink_t*)cbdata;
    orte_iof_write_event_t *wev = sink->wev;
    opal_list_item_t *item;
    orte_iof_write_output_t *output;
    int num_written;

    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                         "%s orted:stdin:write:handler writing data to %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         wev->fd));

    wev->pending = false;

    while (NULL != (item = opal_list_remove_first(&wev->outputs))) {
        output = (orte_iof_write_output_t*)item;
        if (0 == output->numbytes) {
            /* this indicates we are to close the fd - there is
             * nothing to write
             */
            OPAL_OUTPUT_VERBOSE((20, orte_iof_base_framework.framework_output,
                                 "%s iof:orted closing fd %d on write event due to zero bytes output",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), wev->fd));
            OBJ_RELEASE(wev);
            sink->wev = NULL;
            return;
        }
        num_written = write(wev->fd, output->data, output->numbytes);
        OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                             "%s orted:stdin:write:handler wrote %d bytes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             num_written));
        if (num_written < 0) {
            if (EAGAIN == errno || EINTR == errno) {
                /* push this item back on the front of the list */
                opal_list_prepend(&wev->outputs, item);
                /* leave the write event running so it will call us again
                 * when the fd is ready.
                 */
                wev->pending = true;
                opal_event_add(wev->ev, 0);
                goto CHECK;
            }
            /* otherwise, something bad happened so all we can do is declare an
             * error and abort
             */
            OBJ_RELEASE(output);
            OPAL_OUTPUT_VERBOSE((20, orte_iof_base_framework.framework_output,
                                 "%s iof:orted closing fd %d on write event due to negative bytes written",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), wev->fd));
            OBJ_RELEASE(wev);
            sink->wev = NULL;
            /* tell the HNP to stop sending us stuff */
            if (!mca_iof_orted_component.xoff) {
                mca_iof_orted_component.xoff = true;
                orte_iof_orted_send_xonxoff(ORTE_IOF_XOFF);
            }
            return;
        } else if (num_written < output->numbytes) {
            OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                                 "%s orted:stdin:write:handler incomplete write %d - adjusting data",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_written));
            /* incomplete write - adjust data to avoid duplicate output */
            memmove(output->data, &output->data[num_written], output->numbytes - num_written);
            /* push this item back on the front of the list */
            opal_list_prepend(&wev->outputs, item);
            /* leave the write event running so it will call us again
             * when the fd is ready.
             */
            wev->pending = true;
            opal_event_add(wev->ev, 0);
            goto CHECK;
        }
        OBJ_RELEASE(output);
    }

CHECK:
    if (mca_iof_orted_component.xoff) {
        /* if we have told the HNP to stop reading stdin, see if
         * the proc has absorbed enough to justify restart
         *
         * RHC: Note that when multiple procs want stdin, we
         * can get into a fight between a proc turnin stdin
         * back "on" and other procs turning it "off". There
         * is no clear way to resolve this as different procs
         * may take input at different rates.
         */
        if (opal_list_get_size(&wev->outputs) < ORTE_IOF_MAX_INPUT_BUFFERS) {
            /* restart the read */
            mca_iof_orted_component.xoff = false;
            orte_iof_orted_send_xonxoff(ORTE_IOF_XON);
        }
    }
}
