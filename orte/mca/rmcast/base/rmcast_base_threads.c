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

    if (!orte_rmcast_base.enable_progress_thread) {
        return ORTE_SUCCESS;
    }

    if (rcv_thread) {
        OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base: starting recv thread",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* set the wakeup pipe to target the rmcast event base */
        orte_rmcast_base.recv_ctl.wakeup_pipe = orte_rmcast_base.event_base->wakeup_pipe[1];
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
        /* set the wakeup pipe to target the rmcast event base */
        orte_rmcast_base.recv_process_ctl.wakeup_pipe = orte_rmcast_base.event_base->wakeup_pipe[1];
        /* setup a pipe that we will use to signal the thread that a message
         * is waiting to be processed - don't define an event for it
         */
        if (pipe(orte_rmcast_base.recv_pipe) < 0) {
            opal_output(0, "%s Cannot open recv processing thread ctl pipe",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* start the thread - we will send it a specific character when
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
    char byte='s';

     if (!orte_rmcast_base.enable_progress_thread) {
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
        opal_fd_write(orte_rmcast_base.recv_pipe[1], 1, &byte);
        opal_thread_join(&orte_rmcast_base.recv_process, NULL);
    }
    OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base: all threads stopped",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
}

int orte_rmcast_base_process_msg(orte_mcast_msg_event_t *msg)
{
    orte_rmcast_channel_t channel;
    rmcast_base_recv_t *ptr;
    orte_process_name_t name;
    orte_rmcast_tag_t tag;
    int8_t flag;
    struct iovec *iovec_array=NULL;
    int32_t iovec_count=0, i, n, isz;
    int rc=ORTE_SUCCESS;
    orte_rmcast_seq_t recvd_seq_num;
    opal_list_item_t *item;

    /* extract the header */
    if (ORTE_SUCCESS != (rc = extract_hdr(msg->buf, &name, &channel, &tag, &recvd_seq_num))) {
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
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(msg->buf, &flag, &n, OPAL_INT8))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base:process_recv sender: %s channel: %d tag: %d %s seq_num: %lu",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&name), channel, (int)tag,
                         (0 == flag) ? "iovecs" : "buffer", recvd_seq_num));
    
    
    /* find the recv for this channel, tag, and type */
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
        
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base:process_recv delivering message to channel %d tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ptr->channel, (int)tag));
        
        ptr->seq_num = recvd_seq_num;
        /* we have a recv - unpack the data */
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
            if (NULL != ptr->cbfunc_iovec) {
                OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                     "%s rmcast:base:process_recv delivering iovecs to channel %d tag %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ptr->channel, (int)tag));
                ptr->cbfunc_iovec(ORTE_SUCCESS, ptr->channel, ptr->seq_num, tag,
                                  &name, iovec_array, iovec_count, ptr->cbdata);
                /* if it isn't persistent, remove it */
                if (!(ORTE_RMCAST_PERSISTENT & ptr->flags)) {
                    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                         "%s rmcast:base:process_recv removing non-persistent recv",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    opal_list_remove_item(&orte_rmcast_base.recvs, &ptr->item);
                    OBJ_RELEASE(ptr);
                }
            } else {
                /* if something is already present, then we have a problem */
                if (NULL != ptr->iovec_array) {
                    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                         "%s rmcast:base:process_recv blocking recv already fulfilled",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    goto cleanup;
                }
                /* copy over the iovec array since it will be released by
                 * the blocking recv
                 */
                ptr->iovec_array = (struct iovec *)malloc(iovec_count * sizeof(struct iovec));
                ptr->iovec_count = iovec_count;
                for (i=0; i < iovec_count; i++) {
                    ptr->iovec_array[i].iov_base = (IOVBASE_TYPE*)malloc(iovec_array[i].iov_len);
                    ptr->iovec_array[i].iov_len = iovec_array[i].iov_len;
                    memcpy(ptr->iovec_array[i].iov_base, iovec_array[i].iov_base, iovec_array[i].iov_len);
                }
                /* flag it as recvd to release blocking recv */
                ptr->recvd = true;
            }
        } else {
            if (NULL != ptr->cbfunc_buffer) {
                OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                     "%s rmcast:base:process_recv delivering buffer to channel %d tag %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ptr->channel, (int)tag));
                ptr->cbfunc_buffer(ORTE_SUCCESS, ptr->channel, ptr->seq_num, tag,
                                   &name, msg->buf, ptr->cbdata);
                /* if it isn't persistent, remove it */
                if (!(ORTE_RMCAST_PERSISTENT & ptr->flags)) {
                    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                         "%s rmcast:base:process_recv removing non-persistent recv",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    opal_list_remove_item(&orte_rmcast_base.recvs, &ptr->item);
                    OBJ_RELEASE(ptr);
                }
            } else {
                /* if something is already present, then we have a problem */
                if (NULL != ptr->buf) {
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
                ptr->buf = OBJ_NEW(opal_buffer_t);
                if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(ptr->buf, msg->buf))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }                    
                /* flag it as recvd to release blocking recv */
                ptr->recvd = true;
            }
        }
        /* we are done - only one recv can match */
        break;
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

    return rc;
}


static void* rcv_processing_thread(opal_object_t *obj)
{
    orte_mcast_msg_event_t *msg;
    char byte;
    int rc;

    OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base: recv processing thread operational",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    while (1) {
        /* block here until a trigger arrives */
        if (0 > (rc = opal_fd_read(orte_rmcast_base.recv_pipe[0], 1, &byte))) {
            /* if something bad happened, punt */
            opal_output(0, "%s PUNTING THREAD", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            return OPAL_THREAD_CANCELLED;
        }
        /* check to see if we were told to stop */
        if ('s' == byte) {
            return OPAL_THREAD_CANCELLED;
        }

        /* get a message off the list */
        ORTE_ACQUIRE_THREAD(&orte_rmcast_base.recv_process_ctl);
        if (NULL == (msg = (orte_mcast_msg_event_t*)opal_list_remove_first(&orte_rmcast_base.msg_list))) {
            /* nothing was there - error */
            opal_output(0, "%s ERROR PROCESSING MULTICAST MESSAGES",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            ORTE_RELEASE_THREAD(&orte_rmcast_base.recv_process_ctl);
            continue;
        }
        ORTE_RELEASE_THREAD(&orte_rmcast_base.recv_process_ctl);

        /* process it - processing function release the msg */
        if (ORTE_SUCCESS != (rc = orte_rmcast_base_process_msg(msg))) {
            ORTE_ERROR_LOG(rc);
        }
    }
}

static void* rcv_progress_thread(opal_object_t *obj)
{
    int events=0;

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
        events += opal_event_loop(orte_rmcast_base.event_base, OPAL_EVLOOP_ONCE);
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
