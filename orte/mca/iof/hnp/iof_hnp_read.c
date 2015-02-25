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
 * Copyright (c) 2007-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2014-2015 Intel Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/dss/dss.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/util/name_fns.h"
#include "orte/mca/state/state.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"

#include "iof_hnp.h"

static void restart_stdin(int fd, short event, void *cbdata)
{
    orte_timer_t *tm = (orte_timer_t*)cbdata;

    if (NULL != mca_iof_hnp_component.stdinev &&
        !orte_job_term_ordered &&
        !mca_iof_hnp_component.stdinev->active) {
        mca_iof_hnp_component.stdinev->active = true;
        opal_event_add(mca_iof_hnp_component.stdinev->ev, 0);
    }

    /* if this was a timer callback, then release the timer */
    if (NULL != tm) {
        OBJ_RELEASE(tm);
    }
}

/* return true if we should read stdin from fd, false otherwise */
bool orte_iof_hnp_stdin_check(int fd)
{
#if defined(HAVE_TCGETPGRP)
    if( isatty(fd) && (getpgrp() != tcgetpgrp(fd)) ) {
        return false;
    }
#endif
    return true;
}

void orte_iof_hnp_stdin_cb(int fd, short event, void *cbdata)
{
    bool should_process = orte_iof_hnp_stdin_check(0);
    
    if (should_process) {
        mca_iof_hnp_component.stdinev->active = true;
        opal_event_add(mca_iof_hnp_component.stdinev->ev, 0);
    } else {
        opal_event_del(mca_iof_hnp_component.stdinev->ev);
        mca_iof_hnp_component.stdinev->active = false;
    }
}

/* this is the read handler for my own child procs. In this case,
 * the data is going nowhere - I just output it myself
 */
void orte_iof_hnp_read_local_handler(int fd, short event, void *cbdata)
{
    orte_iof_read_event_t *rev = (orte_iof_read_event_t*)cbdata;
    unsigned char data[ORTE_IOF_BASE_MSG_MAX];
    int32_t numbytes;
    opal_list_item_t *item, *prev_item;
    orte_iof_proc_t *proct;
    int rc;
    orte_ns_cmp_bitmask_t mask;
    bool exclusive;
    
    /* read up to the fragment size */
    numbytes = read(fd, data, sizeof(data));
    
    if (numbytes < 0) {
        /* either we have a connection error or it was a non-blocking read */
        
        /* non-blocking, retry */
        if (EAGAIN == errno || EINTR == errno) {
            opal_event_add(rev->ev, 0);
            return;
        } 

        OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                             "%s iof:hnp:read handler %s Error on connection:%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&rev->name), fd));
        /* Un-recoverable error. Allow the code to flow as usual in order to
         * to send the zero bytes message up the stream, and then close the
         * file descriptor and delete the event.
         */
        numbytes = 0;
    }
    
    /* is this read from our stdin? */
    if (ORTE_IOF_STDIN & rev->tag) {
        /* The event has fired, so it's no longer active until we
           re-add it */
        mca_iof_hnp_component.stdinev->active = false;
    
        /* if job termination has been ordered, just ignore the
         * data and delete the read event
         */
        if (orte_job_term_ordered) {
            OBJ_RELEASE(mca_iof_hnp_component.stdinev);
            return;
        }
        /* cycle through our list of sinks */
        for (item = opal_list_get_first(&mca_iof_hnp_component.sinks);
             item != opal_list_get_end(&mca_iof_hnp_component.sinks);
             item = opal_list_get_next(item)) {
            orte_iof_sink_t* sink = (orte_iof_sink_t*)item;
            
            /* only look at stdin sinks */
            if (!(ORTE_IOF_STDIN & sink->tag)) {
                continue;
            }
            
            mask = ORTE_NS_CMP_ALL;

            /* if the daemon is me, then this is a local sink */
            if (OPAL_EQUAL == orte_util_compare_name_fields(mask, ORTE_PROC_MY_NAME, &sink->daemon)) {
                OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                                     "%s read %d bytes from stdin - writing to %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numbytes,
                                     ORTE_NAME_PRINT(&rev->name)));
                /* send the bytes down the pipe - we even send 0 byte events
                 * down the pipe so it forces out any preceding data before
                 * closing the output stream
                 */
                if (NULL != sink->wev) {
                    if (ORTE_IOF_MAX_INPUT_BUFFERS < orte_iof_base_write_output(&rev->name, rev->tag, data, numbytes, sink->wev)) {
                        /* getting too backed up - stop the read event for now if it is still active */

                        OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                                             "buffer backed up - holding"));
                        return;
                    }
                }
            } else {
                OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                                     "%s sending %d bytes from stdin to daemon %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numbytes,
                                     ORTE_NAME_PRINT(&sink->daemon)));
                
                /* send the data to the daemon so it can
                 * write it to the proc's fd - in this case,
                 * we pass sink->name to indicate who is to
                 * receive the data. If the connection closed,
                 * numbytes will be zero so zero bytes will be
                 * sent - this will tell the daemon to close
                 * the fd for stdin to that proc
                 */
                if( ORTE_SUCCESS != (rc = orte_iof_hnp_send_data_to_endpoint(&sink->daemon, &sink->name, ORTE_IOF_STDIN, data, numbytes))) {
                    /* if the addressee is unknown, remove the sink from the list */
                    if( ORTE_ERR_ADDRESSEE_UNKNOWN == rc ) {
                        prev_item = opal_list_get_prev(item);
                        opal_list_remove_item(&mca_iof_hnp_component.sinks, item);
                        OBJ_RELEASE(item);
                        item = prev_item;
                    }
                }
            }
        }
        /* if num_bytes was zero, or we read the last piece of the file, then we need to terminate the event */
        if (0 == numbytes) {
            /* this will also close our stdin file descriptor */
            OBJ_RELEASE(mca_iof_hnp_component.stdinev);
        } else {
            /* if we are looking at a tty, then we just go ahead and restart the
             * read event assuming we are not backgrounded
             */
            if (orte_iof_hnp_stdin_check(fd)) {
                restart_stdin(fd, 0, NULL);
            } else {
                /* delay for awhile and then restart */
                ORTE_TIMER_EVENT(0, 10000, restart_stdin, ORTE_INFO_PRI);
            }
        }
        /* nothing more to do */
        return;
    }
    
    /* this must be output from one of my local procs - see
     * if anyone else has requested a copy of this info
     */
    exclusive = false;
    for (item = opal_list_get_first(&mca_iof_hnp_component.sinks);
         item != opal_list_get_end(&mca_iof_hnp_component.sinks);
         item = opal_list_get_next(item)) {
        orte_iof_sink_t *sink = (orte_iof_sink_t*)item;
        /* if the target isn't set, then this sink is for another purpose - ignore it */
        if (ORTE_JOBID_INVALID == sink->daemon.jobid) {
            continue;
        }
        if ((sink->tag & rev->tag) &&
            sink->name.jobid == rev->name.jobid &&
            (ORTE_VPID_WILDCARD == sink->name.vpid || sink->name.vpid == rev->name.vpid)) {
            /* need to send the data to the remote endpoint - if
             * the connection closed, numbytes will be zero, so
             * the remote endpoint will know to close its local fd.
             * In this case, we pass rev->name to indicate who the
             * data came from.
             */
            OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                                 "%s sending data to tool %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&sink->daemon)));
            orte_iof_hnp_send_data_to_endpoint(&sink->daemon, &rev->name, rev->tag, data, numbytes);
            if (sink->exclusive) {
                exclusive = true;
            }
        }
    }

    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                         "%s read %d bytes from %s of %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numbytes,
                         (ORTE_IOF_STDOUT & rev->tag) ? "stdout" : ((ORTE_IOF_STDERR & rev->tag) ? "stderr" : "stddiag"),
                         ORTE_NAME_PRINT(&rev->name)));
    
    if (0 == numbytes) {
        /* if we read 0 bytes from the stdout/err/diag, there is
         * nothing to output - find this proc on our list and
         * release the appropriate event. This will delete the
         * read event and close the file descriptor
         */
        for (item = opal_list_get_first(&mca_iof_hnp_component.procs);
             item != opal_list_get_end(&mca_iof_hnp_component.procs);
             item = opal_list_get_next(item)) {
            proct = (orte_iof_proc_t*)item;
            mask = ORTE_NS_CMP_ALL;
            if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &proct->name, &rev->name)) {
                /* found it - release corresponding event. This deletes
                 * the read event and closes the file descriptor
                 */
                if (rev->tag & ORTE_IOF_STDOUT) {
                    OBJ_RELEASE(proct->revstdout);
                } else if (rev->tag & ORTE_IOF_STDERR) {
                    OBJ_RELEASE(proct->revstderr);
                } else if (rev->tag & ORTE_IOF_STDDIAG) {
                    OBJ_RELEASE(proct->revstddiag);
                }
                /* check to see if they are all done */
                if (NULL == proct->revstdout &&
                    NULL == proct->revstderr &&
                    NULL == proct->revstddiag) {
                    /* this proc's iof is complete */
                    opal_list_remove_item(&mca_iof_hnp_component.procs, item);
                    ORTE_ACTIVATE_PROC_STATE(&proct->name, ORTE_PROC_STATE_IOF_COMPLETE);
                    OBJ_RELEASE(proct);
                }
                break;
            }
        }
        return;
    }

    if (!exclusive) {
        /* see if the user wanted the output directed to files */
        if (NULL != orte_output_filename) {
            /* find the sink for this rank */
            for (item = opal_list_get_first(&mca_iof_hnp_component.sinks);
                 item != opal_list_get_end(&mca_iof_hnp_component.sinks);
                 item = opal_list_get_next(item)) {
                orte_iof_sink_t *sink = (orte_iof_sink_t*)item;
                /* if the target is set, then this sink is for another purpose - ignore it */
                if (ORTE_JOBID_INVALID != sink->daemon.jobid) {
                    continue;
                }
                /* if this sink isn't for output, ignore it */
                if (ORTE_IOF_STDIN & sink->tag) {
                    continue;
                }
                /* is this the desired proc? */
                mask = ORTE_NS_CMP_ALL;

                if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &sink->name, &rev->name)) {
                    /* output to the corresponding file */
                    orte_iof_base_write_output(&rev->name, rev->tag, data, numbytes, sink->wev);
                    /* done */
                    break;
                }
            }
        } else {
            /* output this to our local output */
            if (ORTE_IOF_STDOUT & rev->tag || orte_xml_output) {
                orte_iof_base_write_output(&rev->name, rev->tag, data, numbytes, orte_iof_base.iof_write_stdout->wev);
            } else {
                orte_iof_base_write_output(&rev->name, rev->tag, data, numbytes, orte_iof_base.iof_write_stderr->wev);
            }
        }
    }
    
    /* re-add the event */
    opal_event_add(rev->ev, 0);

    return;
}
