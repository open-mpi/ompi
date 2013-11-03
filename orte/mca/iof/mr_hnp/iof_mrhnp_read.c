/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
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

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"

#include "iof_mrhnp.h"

static void send_data(orte_process_name_t *name, orte_iof_tag_t tag,
                      orte_jobid_t jobid,
                      unsigned char *data, int32_t nbytes);

static void restart_stdin(int fd, short event, void *cbdata)
{
    orte_timer_t *tm = (orte_timer_t*)cbdata;

    opal_output(0, "RESTART STDIN");
    if (NULL != mca_iof_mr_hnp_component.stdinev &&
        !orte_job_term_ordered &&
        !mca_iof_mr_hnp_component.stdinev->active) {
        mca_iof_mr_hnp_component.stdinev->active = true;
        opal_event_add(mca_iof_mr_hnp_component.stdinev->ev, 0);
    }

    /* if this was a timer callback, then release the timer */
    if (NULL != tm) {
        OBJ_RELEASE(tm);
    }
}

/* return true if we should read stdin from fd, false otherwise */
bool orte_iof_mrhnp_stdin_check(int fd)
{
#if defined(HAVE_TCGETPGRP)
    if( isatty(fd) && (getpgrp() != tcgetpgrp(fd)) ) {
        return false;
    }
#endif
    return true;
}

void orte_iof_mrhnp_stdin_cb(int fd, short event, void *cbdata)
{
    bool should_process = orte_iof_mrhnp_stdin_check(0);
    
    if (should_process) {
        mca_iof_mr_hnp_component.stdinev->active = true;
        opal_event_add(mca_iof_mr_hnp_component.stdinev->ev, 0);
    } else {
        opal_event_del(mca_iof_mr_hnp_component.stdinev->ev);
        mca_iof_mr_hnp_component.stdinev->active = false;
    }
}

/* this is the read handler for my own child procs and stdin
 */
void orte_iof_mrhnp_read_local_handler(int fd, short event, void *cbdata)
{
    orte_iof_read_event_t *rev = (orte_iof_read_event_t*)cbdata;
    unsigned char data[ORTE_IOF_BASE_MSG_MAX];
    int32_t numbytes;
    opal_list_item_t *item;
    orte_iof_proc_t *proct;
    int i, j;
    orte_ns_cmp_bitmask_t mask;
    orte_job_t *jdata;
    orte_iof_job_t *iofjob;
    orte_node_t *node;
    orte_proc_t *daemon;
    orte_job_map_t *map;
    bool write_out=false;

    /* read up to the fragment size */
    numbytes = read(fd, data, sizeof(data));

    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                         "%s iof:mrhnp:read handler read %d bytes from %s:%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numbytes,
                         ORTE_NAME_PRINT(&rev->name), fd));

    if (numbytes < 0) {
        /* either we have a connection error or it was a non-blocking read */
        
        /* non-blocking, retry */
        if (EAGAIN == errno || EINTR == errno) {
            opal_event_add(rev->ev, 0);
            return;
        } 

        OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                             "%s iof:mrhnp:read handler %s Error on connection:%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&rev->name), fd));
        /* Un-recoverable error. Allow the code to flow as usual in order to
         * to send the zero bytes message up the stream, and then close the
         * file descriptor and delete the event.
         */
        numbytes = 0;
    }
    
    /* if job termination has been ordered, just ignore the
     * data and delete the stdin read event, if that is what fired
     */
    if (orte_job_term_ordered) {
        if (ORTE_IOF_STDIN & rev->tag) {
            OBJ_RELEASE(mca_iof_mr_hnp_component.stdinev);
        }
        return;
    }

    if (ORTE_IOF_STDIN & rev->tag) {
        /* The event has fired, so it's no longer active until we
         * re-add it
         */
        mca_iof_mr_hnp_component.stdinev->active = false;    
        /* if this was read from my stdin, I need to send this input to all
         * daemons who host mapper procs
         */
        for (j=0; j < mca_iof_mr_hnp_component.stdin_jobs.size; j++) {
            if (NULL == (iofjob = (orte_iof_job_t*)opal_pointer_array_get_item(&mca_iof_mr_hnp_component.stdin_jobs, j))) {
                continue;
            }
            jdata = iofjob->jdata;
            OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                                 "%s read %d bytes from stdin - writing to job %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numbytes,
                                 ORTE_JOBID_PRINT(jdata->jobid)));
            map = jdata->map;
            for (i=0; i < map->nodes->size; i++) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
                    continue;
                }
                daemon = node->daemon;

                if (daemon->name.vpid == ORTE_PROC_MY_NAME->vpid) {
                    /* if it is me, then send the bytes down the stdin pipe
                     * for every local proc (they are all on my proct list) - we even send 0 byte events
                     * down the pipe so it forces out any preceding data before
                     * closing the output stream. We add a 0 byte message if
                     * numbytes < sizeof(data) as this means the chunk we read
                     * was the end of the file.
                     */
                    for (item = opal_list_get_first(&mca_iof_mr_hnp_component.procs);
                         item != opal_list_get_end(&mca_iof_mr_hnp_component.procs);
                         item = opal_list_get_next(item)) {
                        proct = (orte_iof_proc_t*)item;
                        if (proct->name.jobid == jdata->jobid) {
                            if (NULL == proct->sink) {
                                opal_output(0, "NULL SINK FOR PROC %s", ORTE_NAME_PRINT(&proct->name));
                                continue;
                            }
                            if (ORTE_IOF_MAX_INPUT_BUFFERS < orte_iof_base_write_output(&proct->name, ORTE_IOF_STDIN, data, numbytes, proct->sink->wev)) {
                                /* getting too backed up - stop the read event for now if it is still active */
                                if (mca_iof_mr_hnp_component.stdinev->active) {
                                    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                                                         "buffer backed up - holding"));
                                    mca_iof_mr_hnp_component.stdinev->active = false;
                                }
                                return;
                            }
                            if (0 < numbytes && numbytes < (int)sizeof(data)) {
                                /* need to write a 0-byte event to clear the stream and close it */
                                orte_iof_base_write_output(&proct->name, ORTE_IOF_STDIN, data, 0, proct->sink->wev);
                                proct->sink = NULL;
                            }
                        }
                    }
                } else {
                    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                                         "%s sending %d bytes from stdin to daemon %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numbytes,
                                         ORTE_NAME_PRINT(&daemon->name)));
                
                    /* send the data to the daemon so it can
                     * write it to all local procs from this job.
                     * If the connection closed,
                     * numbytes will be zero so zero bytes will be
                     * sent - this will tell the daemon to close
                     * the fd for stdin to that proc
                     */
                    send_data(&daemon->name, ORTE_IOF_STDIN, jdata->jobid, data, numbytes);
                    if (0 < numbytes && numbytes < (int)sizeof(data)) {
                        /* need to send a 0-byte message to clear the stream and close it */
                        send_data(&daemon->name, ORTE_IOF_STDIN, jdata->jobid, data, 0);
                    }
                }
            }
        }
        /* if num_bytes was zero, then we need to terminate the event */
        if (0 == numbytes || numbytes < (int)sizeof(data)) {
            /* this will also close our stdin file descriptor */
            if (NULL != mca_iof_mr_hnp_component.stdinev) {
                OBJ_RELEASE(mca_iof_mr_hnp_component.stdinev);
            }
        } else {
            /* if we are looking at a tty, then we just go ahead and restart the
             * read event assuming we are not backgrounded
             */
            if (orte_iof_mrhnp_stdin_check(fd)) {
                restart_stdin(fd, 0, NULL);
            } else {
                /* delay for awhile and then restart */
                ORTE_TIMER_EVENT(0, 10000, restart_stdin, ORTE_INFO_PRI);
            }
        }
        return;
    }

    if (ORTE_IOF_STDOUT & rev->tag && 0 < numbytes) {
        /* see if we need to forward this output */
        jdata = orte_get_job_data_object(rev->name.jobid);
        if (ORTE_JOBID_INVALID == jdata->stdout_target) {
            /* end of the chain - just output the info */
            write_out = true;
            goto PROCESS;
        }
        /* it goes to the next job in the chain */
        jdata = orte_get_job_data_object(jdata->stdout_target);
        map = jdata->map;
        for (i=0; i < map->nodes->size; i++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
                continue;
            }
            daemon = node->daemon;

            if (daemon->name.vpid == ORTE_PROC_MY_NAME->vpid) {
                /* if it is me, then send the bytes down the stdin pipe
                 * for every local proc (they are all on my proct list)
                 */
                for (item = opal_list_get_first(&mca_iof_mr_hnp_component.procs);
                     item != opal_list_get_end(&mca_iof_mr_hnp_component.procs);
                     item = opal_list_get_next(item)) {
                    proct = (orte_iof_proc_t*)item;
                    if (proct->name.jobid == jdata->jobid) {
                        if (NULL == proct->sink) {
                            opal_output(0, "NULL SINK FOR PROC %s", ORTE_NAME_PRINT(&proct->name));
                            continue;
                        }
                        orte_iof_base_write_output(&proct->name, ORTE_IOF_STDIN, data, numbytes, proct->sink->wev);
                    }
                }
            } else {
                OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                                     "%s sending %d bytes from stdout of %s to daemon %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numbytes,
                                     ORTE_NAME_PRINT(&rev->name),
                                     ORTE_NAME_PRINT(&daemon->name)));
                
                /* send the data to the daemon so it can
                 * write it to all local procs from this job
                 */
                send_data(&daemon->name, ORTE_IOF_STDIN, jdata->jobid, data, numbytes);
            }
        }
    }
    
 PROCESS:
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                         "%s read %d bytes from %s of %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numbytes,
                         (ORTE_IOF_STDOUT & rev->tag) ? "stdout" : ((ORTE_IOF_STDERR & rev->tag) ? "stderr" : "stddiag"),
                         ORTE_NAME_PRINT(&rev->name)));
    
    if (0 == numbytes) {
        /* if we read 0 bytes from the stdout/err/diag, find this proc
         * on our list and
         * release the appropriate event. This will delete the
         * read event and close the file descriptor
         */
        for (item = opal_list_get_first(&mca_iof_mr_hnp_component.procs);
             item != opal_list_get_end(&mca_iof_mr_hnp_component.procs);
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
                    opal_list_remove_item(&mca_iof_mr_hnp_component.procs, item);
                    ORTE_ACTIVATE_PROC_STATE(&proct->name, ORTE_PROC_STATE_IOF_COMPLETE);
                    OBJ_RELEASE(proct);
                }
                break;
            }
        }
        return;
    } else {
        /* output this to our local output */
        if (ORTE_IOF_STDOUT & rev->tag) {
            if (write_out) {
                orte_iof_base_write_output(&rev->name, rev->tag, data, numbytes, orte_iof_base.iof_write_stdout->wev);
            }
        } else {
            orte_iof_base_write_output(&rev->name, rev->tag, data, numbytes, orte_iof_base.iof_write_stderr->wev);
        }
    }
    
    /* re-add the event */
    opal_event_add(rev->ev, 0);

    return;
}

static void send_data(orte_process_name_t *name, orte_iof_tag_t tag,
                      orte_jobid_t jobid,
                      unsigned char *data, int32_t nbytes)
{
    opal_buffer_t *buf;
    int rc;

    buf = OBJ_NEW(opal_buffer_t);

    if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &tag, 1, ORTE_IOF_TAG))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, data, nbytes, OPAL_BYTE))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    if (0 > (rc = orte_rml.send_buffer_nb(name, buf, ORTE_RML_TAG_IOF_PROXY,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
    }
}
