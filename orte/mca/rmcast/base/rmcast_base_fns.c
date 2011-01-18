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

static int insert_hdr(opal_buffer_t *buf,
                      orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      bool restart,
                      orte_rmcast_seq_t seq_num);

rmcast_base_channel_t* orte_rmcast_base_get_channel(orte_rmcast_channel_t channel)
{
    rmcast_base_channel_t *chptr, *ch;
    opal_list_item_t *item;

    /* if we were asked to send this on our group output
     * channel, substitute it
     */
    if (ORTE_RMCAST_GROUP_OUTPUT_CHANNEL == channel) {
        if (NULL == orte_rmcast_base.my_output_channel) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return NULL;
        }
        return orte_rmcast_base.my_output_channel;
    } else if (ORTE_RMCAST_GROUP_INPUT_CHANNEL == channel) {
        if (NULL == orte_rmcast_base.my_input_channel) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return NULL;
        }
        return orte_rmcast_base.my_input_channel;
    }
    
    /* find the channel */
    ch = NULL;
    ORTE_ACQUIRE_THREAD(&orte_rmcast_base.main_ctl);
    for (item = opal_list_get_first(&orte_rmcast_base.channels);
         item != opal_list_get_end(&orte_rmcast_base.channels);
         item = opal_list_get_next(item)) {
        chptr = (rmcast_base_channel_t*)item;
        if (channel == chptr->channel) {
            ch = chptr;
            break;
        }
    }
    ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);
    if (NULL == ch) {
        /* didn't find it */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }

    return ch;
}

int orte_rmcast_base_queue_xmit(rmcast_base_send_t *snd,
                                orte_rmcast_channel_t channel,
                                opal_buffer_t **buffer,
                                rmcast_base_channel_t **chan)
{
    rmcast_base_channel_t *ch;
    int32_t sz;
    int rc;
    int8_t flag;
    int32_t tmp32;
    opal_buffer_t *buf;

    /* setup default responses */
    *buffer = NULL;
    *chan = NULL;

    /* get the channel object */
    if (NULL == (ch = orte_rmcast_base_get_channel(channel))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    /* return the channel */
    *chan = ch;

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base:queue_xmit of %d %s"
                         " for multicast on channel %d tag %d seq_num %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == snd->iovec_array) ? (int)snd->buf->bytes_used : (int)snd->iovec_count,
                         (NULL == snd->iovec_array) ? "bytes" : "iovecs",
                         (int)ch->channel, snd->tag, ch->seq_num));
    
    /* setup a buffer */
    buf = OBJ_NEW(opal_buffer_t);
    *buffer = buf;

    /* assign a sequence number */
    ORTE_MULTICAST_NEXT_SEQUENCE_NUM(ch->seq_num);

    /* insert the header */
    if (ORTE_SUCCESS != (rc = insert_hdr(buf, ch->channel, snd->tag, ch->restart, ch->seq_num))) {
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
    /* flag this channel as no longer in restart mode since
     * it will have sent at least one message
     */
    ch->restart = false;
    return ORTE_SUCCESS;
    
cleanup:
    if (NULL != buf) {
        OBJ_RELEASE(buf);
    }
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
        ORTE_ACQUIRE_THREAD(&orte_rmcast_base.main_ctl);
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
                    ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);
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
                    ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);
                    return ORTE_EXISTS;
                }
                rptr->cbfunc_buffer = cbfunc_buffer;
            }
            if (NULL != recvptr) {
                *recvptr = rptr;
            }
            ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);
            return ORTE_SUCCESS;
        }
        ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);
    }
    
    /* if we get here, then we need to add a new recv */
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:base: adding recv on multicast channel %d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel, tag));
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
    ORTE_ACQUIRE_THREAD(&orte_rmcast_base.main_ctl);
    if (ORTE_RMCAST_TAG_WILDCARD == tag) {
        opal_list_append(&orte_rmcast_base.recvs, &rptr->item);
    } else {
        opal_list_prepend(&orte_rmcast_base.recvs, &rptr->item);
    }
    ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);
    
    return ORTE_SUCCESS;
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
    ORTE_ACQUIRE_THREAD(&orte_rmcast_base.main_ctl);
    item = opal_list_get_first(&orte_rmcast_base.recvs);
    while (item != opal_list_get_end(&orte_rmcast_base.recvs)) {
        next = opal_list_get_next(item);
        
        ptr = (rmcast_base_recv_t*)item;
        if (ch == ptr->channel &&
            tag == ptr->tag) {
            opal_list_remove_item(&orte_rmcast_base.recvs, &ptr->item);
            OBJ_RELEASE(ptr);
        }
        item = next;
    }
    ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);
}

int orte_rmcast_base_close_channel(orte_rmcast_channel_t channel)
{
    opal_list_item_t *item;
    rmcast_base_channel_t *chan;
    
    ORTE_ACQUIRE_THREAD(&orte_rmcast_base.main_ctl);
    for (item = opal_list_get_first(&orte_rmcast_base.channels);
         item != opal_list_get_end(&orte_rmcast_base.channels);
         item = opal_list_get_next(item)) {
        chan = (rmcast_base_channel_t*)item;
        
        if (channel == chan->channel) {
            opal_list_remove_item(&orte_rmcast_base.channels, item);
            OBJ_RELEASE(chan);
            ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);
            return ORTE_SUCCESS;
        }
    }
    
    ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);
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

static int insert_hdr(opal_buffer_t *buf,
                      orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      bool restart,
                      orte_rmcast_seq_t seq_num)
{
    int rc;
    uint8_t flag;
    
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
    
    if (restart) {
        flag = 1;
    } else {
        flag = 0;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &flag, 1, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &seq_num, 1, ORTE_RMCAST_SEQ_T))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}
