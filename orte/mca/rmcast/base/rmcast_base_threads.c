/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/fd.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/threads/threads.h"

#include "orte/mca/rmcast/base/base.h"
#include "orte/mca/rmcast/base/private.h"

static void* rcv_processing_thread(opal_object_t *obj);
static int extract_hdr(opal_buffer_t *buf,
                       orte_process_name_t *name,
                       orte_rmcast_channel_t *channel,
                       orte_rmcast_tag_t *tag,
                       bool *restart,
                       orte_rmcast_seq_t *seq_num);

int orte_rmcast_base_start_threads(void)
{
    int rc;

    if (!orte_rmcast_base.recv_process_ctl.running) {
        OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base: starting recv processing thread",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* setup a pipe that we will use to signal the thread that a message
         * is waiting to be processed - don't define an event for it
         */
        if (pipe(orte_rmcast_base.recv_pipe) < 0) {
            opal_output(0, "%s Cannot open recv processing thread ctl pipe",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* start the thread - we will send it a NULL msg pointer when
         * we want it to stop
         */
        orte_rmcast_base.recv_process.t_run = rcv_processing_thread;
        if (ORTE_SUCCESS != (rc = opal_thread_start(&orte_rmcast_base.recv_process))) {
            ORTE_ERROR_LOG(rc);
            orte_rmcast_base.recv_process_ctl.running = false;
            return rc;
        }

        OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base: recv processing thread started",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    }

    return ORTE_SUCCESS;
}

void orte_rmcast_base_stop_threads(void)
{
    opal_buffer_t *msg=NULL;

    OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base: stopping recv processing thread",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    ORTE_ACQUIRE_THREAD(&orte_rmcast_base.recv_process_ctl);
    if (orte_rmcast_base.recv_process_ctl.running) {
        ORTE_RELEASE_THREAD(&orte_rmcast_base.recv_process_ctl);
        opal_fd_write(orte_rmcast_base.recv_pipe[1], sizeof(opal_buffer_t*), &msg);
        opal_thread_join(&orte_rmcast_base.recv_process, NULL);
        ORTE_ACQUIRE_THREAD(&orte_rmcast_base.recv_process_ctl);
    }
    ORTE_RELEASE_THREAD(&orte_rmcast_base.recv_process_ctl);

    OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base: all threads stopped",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
}

void orte_rmcast_base_process_msg(orte_rmcast_msg_t *msg)
{
    orte_rmcast_channel_t channel;
    rmcast_base_recv_t *ptr, *recv=NULL;
    orte_process_name_t name;
    orte_rmcast_tag_t tag;
    int8_t flag;
    struct iovec *iovec_array=NULL;
    int32_t iovec_count=0, i, n, isz;
    int rc=ORTE_SUCCESS;
    orte_rmcast_seq_t recvd_seq_num;
    opal_list_item_t *item;
    rmcast_seq_tracker_t *trkr, *tptr;
    rmcast_recv_log_t *log, *logptr;
    bool restart;
    opal_buffer_t alert;

    /* extract the header */
    if (ORTE_SUCCESS != (rc = extract_hdr(msg->buf, &name, &channel, &tag, &restart, &recvd_seq_num))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* if this message is from myself, ignore it */
    if (name.jobid == ORTE_PROC_MY_NAME->jobid && name.vpid == ORTE_PROC_MY_NAME->vpid) {
        OPAL_OUTPUT_VERBOSE((10, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base:process_recv sent from myself: %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name)));
        goto cleanup;
    }
    
    /* if this is a heartbeat and I am not a daemon or a scheduler, then ignore it
     * to avoid swamping tools
     */
    if (!(ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_SCHEDULER) && ORTE_RMCAST_TAG_HEARTBEAT == tag) {
        OPAL_OUTPUT_VERBOSE((10, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base:process_recv ignoring heartbeat",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto cleanup;
    }

    /* if this message is from a different job family, ignore it unless
     * it is on the system channel. We ignore these messages to avoid
     * confusion between different jobs since we all may be sharing
     * multicast channels. The system channel is left open to support
     * cross-job communications for detecting multiple conflicting DVMs.
     */
    if (ORTE_JOB_FAMILY(name.jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid) &&
        (ORTE_RMCAST_SYS_CHANNEL != channel)) {
        /* if we are not the HNP or a daemon, then we ignore this */
        if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
            OPAL_OUTPUT_VERBOSE((10, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:base:process_recv from a different job family: %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&name)));
        } else {
            goto cleanup;
        }
    }
    
    if (orte_rmcast_base.unreliable_xport) {
        /* if the message is not on a system-specified channel, then check to see if we
         * are missing any messages and need a resend
         */
        if (ORTE_RMCAST_DYNAMIC_CHANNELS <= channel) {
            log = NULL;
            for (item = opal_list_get_first(&orte_rmcast_base.msg_logs);
                 item != opal_list_get_end(&orte_rmcast_base.msg_logs);
                 item = opal_list_get_next(item)) {
                logptr = (rmcast_recv_log_t*)item;
                /* look for this source */
                if (name.jobid == logptr->name.jobid &&
                    name.vpid == logptr->name.vpid) {
                    log = logptr;
                    break;
                }
            }
            if (NULL == log) {
                /* new source */
                log = OBJ_NEW(rmcast_recv_log_t);
                log->name.jobid = name.jobid;
                log->name.vpid = name.vpid;
                opal_list_append(&orte_rmcast_base.msg_logs, &log->super);
            }
            /* look for the channel */
            trkr = NULL;
            for (item = opal_list_get_first(&log->last_msg);
                 item != opal_list_get_end(&log->last_msg);
                 item = opal_list_get_next(item)) {
                tptr = (rmcast_seq_tracker_t*)item;
                if (channel == tptr->channel) {
                    trkr = tptr;
                    break;
                }
            }
            if (NULL == trkr) {
                /* new channel */
                trkr = OBJ_NEW(rmcast_seq_tracker_t);
                trkr->channel = channel;
                opal_list_append(&log->last_msg, &trkr->super);
                OPAL_OUTPUT_VERBOSE((10, orte_rmcast_base.rmcast_output,
                                     "%s NEW CHANNEL: %d SENDER: %s SEQ %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     channel, ORTE_NAME_PRINT(&log->name), recvd_seq_num));
            } else if (ORTE_RMCAST_SEQ_INVALID != trkr->seq_num && !restart) {
                /* if this is a repeat msg, ignore it */
                if (recvd_seq_num <= trkr->seq_num) {
                    OPAL_OUTPUT_VERBOSE((1, orte_rmcast_base.rmcast_output,
                                         "%s Repeat msg %d on channel %d from source %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recvd_seq_num, channel,
                                         ORTE_NAME_PRINT(&name)));
                }
                if (1 != (recvd_seq_num - trkr->seq_num) ||
                    (ORTE_RMCAST_SEQ_MAX == trkr->seq_num && 0 != recvd_seq_num)) {
                    /* missing a message - request it */
                    OPAL_OUTPUT_VERBOSE((1, orte_rmcast_base.rmcast_output,
                                         "%s Missed msg %d (%d) on channel %d from source %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recvd_seq_num,
                                         trkr->seq_num, channel, ORTE_NAME_PRINT(&name)));
                    OBJ_CONSTRUCT(&alert, opal_buffer_t);
                    if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &channel, 1, ORTE_RMCAST_CHANNEL_T))) {
                        ORTE_ERROR_LOG(rc);
                        exit(1);
                    }
                    if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &trkr->seq_num, 1, ORTE_RMCAST_SEQ_T))) {
                        ORTE_ERROR_LOG(rc);
                        exit(1);
                    }
                    if (0 > (rc = orte_rml.send_buffer(&name, &alert, ORTE_RML_TAG_MISSED_MSG, 0))) {
                        ORTE_ERROR_LOG(rc);
                        exit(1);
                    }
                    OBJ_DESTRUCT(&alert);
                    goto cleanup;
                }
                OPAL_OUTPUT_VERBOSE((10, orte_rmcast_base.rmcast_output,
                                     "%s CHANNEL: %d SENDER: %s SEQ: %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     channel, ORTE_NAME_PRINT(&log->name), recvd_seq_num));
            }
            trkr->seq_num = recvd_seq_num;
        }
    }

    /* unpack the iovec vs buf flag */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(msg->buf, &flag, &n, OPAL_INT8))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base:process_recv sender: %s channel: %d tag: %d %s seq_num: %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&name), channel, (int)tag,
                         (0 == flag) ? "iovecs" : "buffer", recvd_seq_num));
    
    
    /* find the recv for this channel, tag, and type */
    ORTE_ACQUIRE_THREAD(&orte_rmcast_base.main_ctl);
    for (item = opal_list_get_first(&orte_rmcast_base.recvs);
         item != opal_list_get_end(&orte_rmcast_base.recvs);
         item = opal_list_get_next(item)) {
        ptr = (rmcast_base_recv_t*)item;
        
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base:process_recv checking channel %d tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (int)ptr->channel, (int)ptr->tag));
        
        if (channel != ptr->channel) {
            continue;
        }
        
        if (tag != ptr->tag && ORTE_RMCAST_TAG_WILDCARD != ptr->tag) {
            continue;
        }
        
        ptr->seq_num = recvd_seq_num;
        recv = ptr;
        break;
    }

    if (NULL == recv) {
        /* recv not found - dump msg */
        ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);
        goto cleanup;
    }

    if (!(ORTE_RMCAST_PERSISTENT & recv->flags)) {
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base:process_recv removing non-persistent recv",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        opal_list_remove_item(&orte_rmcast_base.recvs, &recv->item);
    }
    ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base:process_recv delivering message to channel %d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recv->channel, (int)tag));
        
    /* we have a matching recv - unpack the data */
    if (0 == flag) {
        /* get the number of iovecs in the buffer */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(msg->buf, &iovec_count, &n, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* malloc the required space */
        iovec_array = (struct iovec *)malloc(iovec_count * sizeof(struct iovec));
        /* unpack the iovecs */
        for (i=0; i < iovec_count; i++) {
            /* unpack the number of bytes in this iovec */
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(msg->buf, &isz, &n, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            iovec_array[i].iov_base = NULL;
            iovec_array[i].iov_len = isz;
            if (0 < isz) {
                /* allocate the space */
                iovec_array[i].iov_base = (IOVBASE_TYPE*)malloc(isz);
                /* unpack the data */
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(msg->buf, iovec_array[i].iov_base, &isz, OPAL_UINT8))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }                    
            }
        }
        if (NULL != recv->cbfunc_iovec) {
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:base:process_recv delivering iovecs to channel %d tag %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recv->channel, (int)tag));
            recv->cbfunc_iovec(ORTE_SUCCESS, recv->channel, recv->seq_num, tag,
                              &name, iovec_array, iovec_count, recv->cbdata);
        } else {
            /* if something is already present, then we have a problem */
            if (NULL != recv->iovec_array) {
                OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                     "%s rmcast:base:process_recv blocking recv already fulfilled",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                goto cleanup;
            }
            /* copy over the iovec array since it will be released by
             * the blocking recv
             */
            recv->iovec_array = (struct iovec *)malloc(iovec_count * sizeof(struct iovec));
            recv->iovec_count = iovec_count;
            for (i=0; i < iovec_count; i++) {
                recv->iovec_array[i].iov_base = (IOVBASE_TYPE*)malloc(iovec_array[i].iov_len);
                recv->iovec_array[i].iov_len = iovec_array[i].iov_len;
                memcpy(recv->iovec_array[i].iov_base, iovec_array[i].iov_base, iovec_array[i].iov_len);
            }
            /* release blocking recv */
            ORTE_WAKEUP_THREAD(&recv->ctl);
        }
    } else {
        if (NULL != recv->cbfunc_buffer) {
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:base:process_recv delivering buffer to channel %d tag %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recv->channel, (int)tag));
            recv->cbfunc_buffer(ORTE_SUCCESS, recv->channel, recv->seq_num, tag,
                               &name, msg->buf, recv->cbdata);
        } else {
            /* if something is already present, then we have a problem */
            if (NULL != recv->buf) {
                OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                     "%s rmcast:base:process_recv blocking recv already fulfilled",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                goto cleanup;
            }
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:base:process_recv copying buffer for blocking recv",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            /* copy the buffer across since it will be released
             * by the blocking recv
             */
            recv->buf = OBJ_NEW(opal_buffer_t);
            if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(recv->buf, msg->buf))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }                    
            /* release blocking recv */
            ORTE_WAKEUP_THREAD(&recv->ctl);
        }
    }
    
 cleanup:
    if (NULL != iovec_array) {
        for (i=0; i < iovec_count; i++) {
            free(iovec_array[i].iov_base);
        }
        free(iovec_array);
        iovec_array = NULL;
        iovec_count = 0;
    }
    if (NULL != msg) {
        OBJ_RELEASE(msg);
    }
    return;
}


static void* rcv_processing_thread(opal_object_t *obj)
{
    orte_rmcast_msg_t *msg;
    int rc;
    struct timespec tp={0, 10};

    OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base: recv processing thread operational",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    ORTE_ACQUIRE_THREAD(&orte_rmcast_base.recv_process_ctl);
    orte_rmcast_base.recv_process_ctl.running = true;
    ORTE_RELEASE_THREAD(&orte_rmcast_base.recv_process_ctl);

    while (1) {
        /* block here until a trigger arrives */
        if (0 > (rc = opal_fd_read(orte_rmcast_base.recv_pipe[0],
                                   sizeof(orte_rmcast_msg_t*), &msg))) {
            /* if something bad happened, punt */
            opal_output(0, "%s PUNTING THREAD", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            ORTE_ACQUIRE_THREAD(&orte_rmcast_base.recv_process_ctl);
            orte_rmcast_base.recv_process_ctl.running = false;
            ORTE_RELEASE_THREAD(&orte_rmcast_base.recv_process_ctl);
            /* give a little delay to ensure the main thread gets into
             * opal_thread_join before we exit
             */
            nanosleep(&tp, NULL);
            return OPAL_THREAD_CANCELLED;
        }
        /* check to see if we were told to stop */
        if (NULL == msg) {
            ORTE_ACQUIRE_THREAD(&orte_rmcast_base.recv_process_ctl);
            orte_rmcast_base.recv_process_ctl.running = false;
            ORTE_RELEASE_THREAD(&orte_rmcast_base.recv_process_ctl);
            return OPAL_THREAD_CANCELLED;
        }

        /* process it - processing function releases the msg */
        orte_rmcast.process_msg(msg);
    }
}

static int extract_hdr(opal_buffer_t *buf,
                       orte_process_name_t *name,
                       orte_rmcast_channel_t *channel,
                       orte_rmcast_tag_t *tag,
                       bool *restart,
                       orte_rmcast_seq_t *seq_num)
{
    int rc;
    int32_t n;
    uint8_t flag;

    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, name, &n, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, channel, &n, ORTE_RMCAST_CHANNEL_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, tag, &n, ORTE_RMCAST_TAG_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &flag, &n, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (flag) {
        *restart = true;
    } else {
        *restart = false;
    }

    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, seq_num, &n, ORTE_RMCAST_SEQ_T))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}
