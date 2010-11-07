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
#include "opal/threads/threads.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/rmcast/base/base.h"
#include "orte/mca/rmcast/base/private.h"

static int extract_hdr(opal_buffer_t *buf,
                       orte_process_name_t *name,
                       orte_rmcast_channel_t *channel,
                       orte_rmcast_tag_t *tag,
                       orte_rmcast_seq_t *seq_num);

static int insert_hdr(opal_buffer_t *buf,
                      orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      orte_rmcast_seq_t seq_num);

int orte_rmcast_base_build_msg(rmcast_base_channel_t *ch,
                               opal_buffer_t **buffer,
                               rmcast_base_send_t *snd)
{
    int32_t sz;
    opal_buffer_t *buf;
    int rc;
    int8_t flag;
    int32_t tmp32;

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base:build_msg of %d %s"
                         " for multicast on channel %d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == snd->iovec_array) ? (int)snd->buf->bytes_used : (int)snd->iovec_count,
                         (NULL == snd->iovec_array) ? "bytes" : "iovecs",
                         (int)ch->channel, snd->tag));
    
    /* setup a buffer */
    buf = OBJ_NEW(opal_buffer_t);
    
    /* insert the header */
    if (ORTE_SUCCESS != (rc = insert_hdr(buf, ch->channel, snd->tag, ch->seq_num))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* are we sending a buffer? */
    if (NULL == snd->buf) {
        /* no, flag the buffer as containing iovecs */
        flag = 0;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &flag, 1, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* pack the number of iovecs */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &snd->iovec_count, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* pack each iovec into a buffer in prep for sending
         * so we can recreate the array at the other end
         */
        for (sz=0; sz < snd->iovec_count; sz++) {
            /* pack the size */
            tmp32 = snd->iovec_array[sz].iov_len;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &tmp32, 1, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if (0 < tmp32) {
                /* pack the bytes */
                if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, snd->iovec_array[sz].iov_base, tmp32, OPAL_UINT8))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
            }
        }
        
    } else {
        /* flag it as being a buffer */
        flag = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &flag, 1, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* copy the payload */
        if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(buf, snd->buf))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    *buffer = buf;
    return ORTE_SUCCESS;
    
cleanup:
    if (NULL != buf) {
        OBJ_RELEASE(buf);
    }
    *buffer = NULL;
    return rc;
}

int orte_rmcast_base_queue_recv(rmcast_base_recv_t **recvptr,
                                orte_rmcast_channel_t channel,
                                orte_rmcast_tag_t tag,
                                orte_rmcast_flag_t flags,
                                orte_rmcast_callback_fn_t cbfunc_iovec,
                                orte_rmcast_callback_buffer_fn_t cbfunc_buffer,
                                void *cbdata, bool blocking)
{
    opal_list_item_t *item;
    rmcast_base_recv_t *rptr;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base: queue_recv called on multicast channel %d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel, tag));
    
    if (!blocking) {
        /* do we already have a recv for this channel/tag? */
        OPAL_THREAD_LOCK(&orte_rmcast_base.lock);
        for (item = opal_list_get_first(&orte_rmcast_base.recvs);
             item != opal_list_get_end(&orte_rmcast_base.recvs);
             item = opal_list_get_next(item)) {
            rptr = (rmcast_base_recv_t*)item;
            if (channel != rptr->channel) {
                /* different channel */
                continue;
            }
            if (tag != rptr->tag) {
                /* different tag */
                continue;
            }
            if (NULL != cbfunc_iovec) {
                if (NULL != rptr->cbfunc_iovec) {
                    /* already have one in place */
                    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                         "%s rmcast:base: matching recv already active on multicast channel %d tag %d",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel, tag));
                    OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
                    return ORTE_EXISTS;
                }
                rptr->cbfunc_iovec = cbfunc_iovec;
            }
            if (NULL != cbfunc_buffer) {
                if (NULL != rptr->cbfunc_buffer) {
                    /* matching type - recv already in place */
                    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                         "%s rmcast:base: matching recv already active on multicast channel %d tag %d",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel, tag));
                    OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
                    return ORTE_EXISTS;
                }
                rptr->cbfunc_buffer = cbfunc_buffer;
            }
            if (NULL != recvptr) {
                *recvptr = rptr;
            }
            OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
            return ORTE_SUCCESS;
        }
    }
    
    /* if we get here, then we need to add a new recv */
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base: adding recv on multicast channel %d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel, tag));
    OPAL_THREAD_LOCK(&orte_rmcast_base.lock);
    rptr = OBJ_NEW(rmcast_base_recv_t);
    rptr->channel = channel;
    rptr->tag = tag;
    rptr->flags = flags;
    rptr->cbfunc_iovec = cbfunc_iovec;
    rptr->cbfunc_buffer = cbfunc_buffer;
    rptr->cbdata = cbdata;
    if (NULL != recvptr) {
        *recvptr = rptr;
    }

    /* wildcard tag recvs get pushed to the end of the list so
     * that specific tag recvs take precedence
     */
    if (ORTE_RMCAST_TAG_WILDCARD == tag) {
        opal_list_append(&orte_rmcast_base.recvs, &rptr->item);
    } else {
        opal_list_prepend(&orte_rmcast_base.recvs, &rptr->item);
    }
    OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
    
    return ORTE_SUCCESS;
}

void orte_rmcast_base_process_recv(orte_mcast_msg_event_t *msg)
{
    orte_rmcast_channel_t channel;
    opal_list_item_t *item;
    rmcast_base_recv_t *ptr;
    orte_process_name_t name;
    orte_rmcast_tag_t tag;
    int8_t flag;
    struct iovec *iovec_array=NULL;
    int32_t iovec_count=0, i, n, isz;
    opal_buffer_t *recvd_buf=NULL;
    int rc;
    orte_rmcast_seq_t recvd_seq_num;
    
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
                             "%s rmcast:base:recv checking channel %d tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (int)ptr->channel, (int)ptr->tag));
        
        if (channel != ptr->channel) {
            continue;
        }
        
        if (tag != ptr->tag && ORTE_RMCAST_TAG_WILDCARD != ptr->tag) {
            continue;
        }
        
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:base:recv delivering message to channel %d tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ptr->channel, (int)tag));
        
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
                                     "%s rmcast:base:recv delivering iovecs to channel %d tag %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ptr->channel, (int)tag));
                
                ptr->cbfunc_iovec(ORTE_SUCCESS, ptr->channel, recvd_seq_num, tag,
                                  &name, iovec_array, iovec_count, ptr->cbdata);
                /* if it isn't persistent, remove it */
                if (!(ORTE_RMCAST_PERSISTENT & ptr->flags)) {
                    OPAL_THREAD_LOCK(&orte_rmcast_base.lock);
                    opal_list_remove_item(&orte_rmcast_base.recvs, &ptr->item);
                    OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
                    OBJ_RELEASE(ptr);
                }
            } else {
                /* if something is already present, then we have a problem */
                if (NULL != ptr->iovec_array) {
                    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                         "%s rmcast:base:recv blocking recv already fulfilled",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    goto cleanup;
                }
                ptr->seq_num = recvd_seq_num;
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
            /* buffer was included */
            recvd_buf = OBJ_NEW(opal_buffer_t);
            if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(recvd_buf, msg->buf))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }                    
            if (NULL != ptr->cbfunc_buffer) {
                OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                     "%s rmcast:base:recv delivering buffer to channel %d tag %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ptr->channel, (int)tag));
                
                ptr->cbfunc_buffer(ORTE_SUCCESS, ptr->channel, recvd_seq_num, tag,
                                   &name, recvd_buf, ptr->cbdata);
                /* if it isn't persistent, remove it */
                if (!(ORTE_RMCAST_PERSISTENT & ptr->flags)) {
                    OPAL_THREAD_LOCK(&orte_rmcast_base.lock);
                    opal_list_remove_item(&orte_rmcast_base.recvs, &ptr->item);
                    OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
                    OBJ_RELEASE(ptr);
                }
            } else {
                /* if something is already present, then we have a problem */
                if (NULL != ptr->buf) {
                    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                         "%s rmcast:base:recv blocking recv already fulfilled",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    goto cleanup;
                }
                OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                     "%s rmcast:base:recv copying buffer for blocking recv",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                ptr->seq_num = recvd_seq_num;
                /* copy the buffer across since it will be released
                 * by the blocking recv
                 */
                ptr->buf = OBJ_NEW(opal_buffer_t);
                if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(ptr->buf, recvd_buf))) {
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
    }
    if (NULL != recvd_buf) {
        OBJ_RELEASE(recvd_buf);
    }
    return;
}

void orte_rmcast_base_cancel_recv(orte_rmcast_channel_t channel,
                                  orte_rmcast_tag_t tag)
{
    opal_list_item_t *item, *next;
    rmcast_base_recv_t *ptr;
    orte_rmcast_channel_t ch;
    
    if (ORTE_RMCAST_GROUP_INPUT_CHANNEL == channel) {
        ch = orte_rmcast_base.my_group_number;
    } else if (ORTE_RMCAST_GROUP_OUTPUT_CHANNEL == channel) {
        ch = orte_rmcast_base.my_group_number + 1;
    } else {
        ch = channel;
    }    
    
    /* find all recv's for this channel and tag */
    item = opal_list_get_first(&orte_rmcast_base.recvs);
    while (item != opal_list_get_end(&orte_rmcast_base.recvs)) {
        next = opal_list_get_next(item);
        
        ptr = (rmcast_base_recv_t*)item;
        if (ch == ptr->channel &&
            tag == ptr->tag) {
            OPAL_THREAD_LOCK(&orte_rmcast_base.lock);
            opal_list_remove_item(&orte_rmcast_base.recvs, &ptr->item);
            OBJ_RELEASE(ptr);
            OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
        }
        item = next;
    }
}

int orte_rmcast_base_close_channel(orte_rmcast_channel_t channel)
{
    opal_list_item_t *item;
    rmcast_base_channel_t *chan;
    
    OPAL_THREAD_LOCK(&orte_rmcast_base.lock);
    for (item = opal_list_get_first(&orte_rmcast_base.channels);
         item != opal_list_get_end(&orte_rmcast_base.channels);
         item = opal_list_get_next(item)) {
        chan = (rmcast_base_channel_t*)item;
        
        if (channel == chan->channel) {
            opal_list_remove_item(&orte_rmcast_base.channels, item);
            OBJ_RELEASE(chan);
            OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
            return ORTE_SUCCESS;
        }
    }
    
    OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
    return ORTE_ERR_NOT_FOUND;
}

int orte_rmcast_base_query(orte_rmcast_channel_t *output, orte_rmcast_channel_t *input)
{
    if (NULL != output) {
        if (NULL == orte_rmcast_base.my_output_channel) {
            *output = ORTE_RMCAST_INVALID_CHANNEL;
        } else {
            *output =  orte_rmcast_base.my_output_channel->channel;
        }
    }
    if (NULL != input) {
        if (NULL == orte_rmcast_base.my_input_channel) {
            *input = ORTE_RMCAST_INVALID_CHANNEL;
        } else {
            *input = orte_rmcast_base.my_input_channel->channel;
        }
    }
    return ORTE_SUCCESS;
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

static int insert_hdr(opal_buffer_t *buf,
                      orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      orte_rmcast_seq_t seq_num)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, ORTE_PROC_MY_NAME, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &channel, 1, ORTE_RMCAST_CHANNEL_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &tag, 1, ORTE_RMCAST_TAG_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &seq_num, 1, ORTE_RMCAST_SEQ_T))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}
