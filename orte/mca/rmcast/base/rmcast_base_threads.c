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
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/threads/threads.h"

#include "orte/mca/rmcast/base/base.h"
#include "orte/mca/rmcast/base/private.h"

static void* rcv_progress_thread(opal_object_t *obj);
static void* rcv_processing_thread(opal_object_t *obj);
static int extract_hdr(opal_buffer_t *buf,
                       orte_process_name_t *name,
                       orte_rmcast_channel_t *channel,
                       orte_rmcast_tag_t *tag,
                       orte_rmcast_seq_t *seq_num);

int orte_rmcast_base_start_threads(bool rcv_thread, bool processing_thread)
{
    int rc;

    if (!orte_progress_threads_enabled) {
        return ORTE_SUCCESS;
    }

    if (rcv_thread) {
        OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base: starting recv thread",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* start the thread */
        orte_rmcast_base.recv_thread.t_run = rcv_progress_thread;
        if (ORTE_SUCCESS != (rc = opal_thread_start(&orte_rmcast_base.recv_thread))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        orte_rmcast_base.recv_ctl.running = true;
        OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base: recv thread started",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    }

    if (processing_thread) {
        OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base: starting recv processing thread",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* set the update to target the rmcast event base since we are
         * processing messages that arrive from that source
         */
        orte_rmcast_base.recv_process_ctl.evbase = orte_rmcast_base.event_base;
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
            return rc;
        }
        orte_rmcast_base.recv_process_ctl.running = true;

        OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base: recv processing thread started",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    }

    return ORTE_SUCCESS;
}

void orte_rmcast_base_stop_threads(void)
{
    opal_buffer_t *msg=NULL;

    if (!orte_progress_threads_enabled) {
        return;
    }

   OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base: stopping recv thread",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    /* if the thread is active, stop it */
    if (orte_rmcast_base.recv_ctl.running) {
        ORTE_ACQUIRE_THREAD(&orte_rmcast_base.recv_ctl);
        orte_rmcast_base.recv_ctl.stop = true;
        ORTE_RELEASE_THREAD(&orte_rmcast_base.recv_ctl);
        opal_thread_join(&orte_rmcast_base.recv_thread, NULL);
    }
    OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base: stopping recv processing thread",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    if (orte_rmcast_base.recv_process_ctl.running) {
        opal_fd_write(orte_rmcast_base.recv_pipe[1], sizeof(opal_buffer_t*), &msg);
        opal_thread_join(&orte_rmcast_base.recv_process, NULL);
    }
    OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base: all threads stopped",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
}

int orte_rmcast_base_process_msg(opal_buffer_t *msg)
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

    /* extract the header */
    if (ORTE_SUCCESS != (rc = extract_hdr(msg, &name, &channel, &tag, &recvd_seq_num))) {
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
    
    /* unpack the iovec vs buf flag */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(msg, &flag, &n, OPAL_INT8))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base:process_recv sender: %s channel: %d tag: %d %s seq_num: %lu",
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
    if (!(ORTE_RMCAST_PERSISTENT & recv->flags)) {
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base:process_recv removing non-persistent recv",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        opal_list_remove_item(&orte_rmcast_base.recvs, &recv->item);
    }
    ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);

    if (NULL == recv) {
        /* recv not found - dump msg */
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base:process_recv delivering message to channel %d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recv->channel, (int)tag));
        
    /* we have a matching recv - unpack the data */
    if (0 == flag) {
        /* get the number of iovecs in the buffer */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(msg, &iovec_count, &n, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* malloc the required space */
        iovec_array = (struct iovec *)malloc(iovec_count * sizeof(struct iovec));
        /* unpack the iovecs */
        for (i=0; i < iovec_count; i++) {
            /* unpack the number of bytes in this iovec */
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(msg, &isz, &n, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            iovec_array[i].iov_base = NULL;
            iovec_array[i].iov_len = isz;
            if (0 < isz) {
                /* allocate the space */
                iovec_array[i].iov_base = (IOVBASE_TYPE*)malloc(isz);
                /* unpack the data */
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(msg, iovec_array[i].iov_base, &isz, OPAL_UINT8))) {
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
            return ORTE_SUCCESS;
        }
    } else {
        if (NULL != recv->cbfunc_buffer) {
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:base:process_recv delivering buffer to channel %d tag %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recv->channel, (int)tag));
            recv->cbfunc_buffer(ORTE_SUCCESS, recv->channel, recv->seq_num, tag,
                               &name, msg, recv->cbdata);
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
            if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(recv->buf, msg))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }                    
            /* release blocking recv */
            ORTE_WAKEUP_THREAD(&recv->ctl);
            return ORTE_SUCCESS;
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
    if (NULL != recv && !(ORTE_RMCAST_PERSISTENT & recv->flags)) {
        OBJ_RELEASE(recv);
    }

    return rc;
}


static void* rcv_processing_thread(opal_object_t *obj)
{
    opal_buffer_t *msg;
    int rc;

    OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base: recv processing thread operational",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    while (1) {
        /* block here until a trigger arrives */
        if (0 > (rc = opal_fd_read(orte_rmcast_base.recv_pipe[0],
                                   sizeof(opal_buffer_t*), &msg))) {
            /* if something bad happened, punt */
            opal_output(0, "%s PUNTING THREAD", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            return OPAL_THREAD_CANCELLED;
        }
        /* check to see if we were told to stop */
        if (NULL == msg) {
            return OPAL_THREAD_CANCELLED;
        }

        /* process it - processing function releases the msg */
        if (ORTE_SUCCESS != (rc = orte_rmcast_base_process_msg(msg))) {
            ORTE_ERROR_LOG(rc);
        }
    }
}

static void* rcv_progress_thread(opal_object_t *obj)
{
    OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base: recv thread operational",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    while (1) {
        ORTE_ACQUIRE_THREAD(&orte_rmcast_base.recv_ctl);
        if (orte_rmcast_base.recv_ctl.stop) {
            ORTE_RELEASE_THREAD(&orte_rmcast_base.recv_ctl);
            return OPAL_THREAD_CANCELLED;
        }
        ORTE_RELEASE_THREAD(&orte_rmcast_base.recv_ctl);
        /* block in the event lib */
        opal_event_loop(orte_rmcast_base.event_base, OPAL_EVLOOP_ONCE);
    }
}

static int extract_hdr(opal_buffer_t *buf,
                       orte_process_name_t *name,
                       orte_rmcast_channel_t *channel,
                       orte_rmcast_tag_t *tag,
                       orte_rmcast_seq_t *seq_num)
{
    int rc;
    int32_t n;
    
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
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, seq_num, &n, ORTE_RMCAST_SEQ_T))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}
