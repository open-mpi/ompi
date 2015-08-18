/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 * qos_base_channel_handlers.c - contains base functions handlers for open, send and close channel requests.
 */

/*
 * includes
 */
#include "orte_config.h"

#include <string.h>

#include "orte/constants.h"
#include "orte/types.h"

#include "opal/dss/dss.h"
#include "opal/util/output.h"
#include "opal/util/timings.h"
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"

#include "orte/mca/qos/qos.h"
#include "orte/mca/qos/base/base.h"


static int orte_qos_base_pack_attributes (opal_buffer_t * buffer,
                                          opal_list_t * qos_attributes)
{
    int32_t num_attributes;
    int32_t rc= ORTE_SUCCESS;
    orte_attribute_t *kv;
    num_attributes = opal_list_get_size (qos_attributes);
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, (void*)(&num_attributes), 1, ORTE_STD_CNTR))) {
        ORTE_LOG_ERROR (rc);
        return rc;
    }
    OPAL_LIST_FOREACH(kv, qos_attributes, orte_attribute_t) {
        if (ORTE_ATTR_GLOBAL == kv->local) {
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, (void*)&kv, 1, ORTE_ATTRIBUTE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    return rc;
}

static int orte_qos_base_unpack_attributes (opal_buffer_t *buffer,
                                            opal_list_t *qos_attributes)
{
    orte_attribute_t *kv;
    int32_t count, n, k;
    int32_t rc=ORTE_SUCCESS;
    /* unpack the attributes */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &count,
                              &n, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    for (k=0; k < count; k++) {
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &kv,
                                  &n, ORTE_ATTRIBUTE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        opal_list_append(qos_attributes, &kv->super);
    }
    return rc;
}

void* orte_qos_base_create_channel ( orte_rml_channel_t *channel,
                                     opal_list_t *qos_attributes)
{
    int32_t * type, type_val;
    mca_qos_base_component_t *qos_comp;
    if(!orte_get_attribute( qos_attributes, ORTE_QOS_TYPE, (void**)&type, OPAL_UINT8))
        return NULL;
    type_val = *type;
    //check if type is valid
    if (0 < type_val || ORTE_QOS_MAX_COMPONENTS <= type_val)
        return  NULL;
    // associate the qos  module
    qos_comp = (mca_qos_base_component_t *) opal_pointer_array_get_item(&orte_qos_base.actives, type_val);
    channel->qos = (void*) &qos_comp->mod;
    // call create channel function of the module.
    return (qos_comp->mod.create( qos_attributes));
}

void * orte_qos_base_create (opal_list_t *qos_attributes)
{
    orte_qos_base_channel_t * base_chan;
    int32_t num_attributes;
    int32_t rc, *window;
    orte_qos_type_t *type;
    orte_attribute_t *kv;
    base_chan = OBJ_NEW (orte_qos_base_channel_t);
    *type = orte_qos_noop;
    // TBD _ we ignore inapplicable attributes for now - need to return error?
    // get attributes of interest to the base and store them locally.
    if (ORTE_SUCCESS == (rc = orte_set_attribute( &base_chan->attributes, ORTE_QOS_TYPE, ORTE_ATTR_GLOBAL, (void*)type, OPAL_UINT8))) {
        // window size??
        if( orte_get_attribute (qos_attributes, ORTE_QOS_WINDOW_SIZE, (void**)&window, OPAL_UINT32)) {
            if ( ORTE_QOS_MAX_WINDOW_SIZE > (*window)) {
                ORTE_ERROR_LOG(OPAL_ERR_VALUE_OUT_OF_BOUNDS);
                OBJ_RELEASE(base_chan);
            }
            else {
                if (ORTE_SUCCESS != (rc = orte_set_attribute(&base_chan->attributes, ORTE_QOS_WINDOW_SIZE,
                                    ORTE_ATTR_GLOBAL, (void*)window, OPAL_UINT32))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(base_chan);
                }
            }
        } else
        OBJ_RELEASE(base_chan);
    } else {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(base_chan);
    }
    return base_chan;
}

int orte_qos_base_open_channel ( void * qos_channel,
                                 opal_buffer_t *buffer)
{
    int32_t rc = ORTE_SUCCESS;
    orte_qos_base_channel_t *base_chan;
    base_chan = (orte_qos_base_channel_t*) (qos_channel);
    // the Qos module puts the non local attributes  to be sent to the peer in a list at the time of create.
    // pack those attributes into the buffer.
    if (ORTE_SUCCESS != (rc =  orte_qos_base_pack_attributes(buffer, &base_chan->attributes)))
        ORTE_ERROR_LOG(rc);
    return rc;
}

void orte_qos_base_chan_recv_init ( void * qos_channel,
                                 opal_list_t *qos_attributes)
{
    // nothing to do for no op channel.
}

void orte_qos_base_close_channel ( void * qos_channel)
{
    qos_channel = (orte_qos_base_channel_t*) (qos_channel);
    OBJ_RELEASE(qos_channel);
}

int orte_qos_base_comp_channel (void *qos_channel,
                                 opal_list_t *qos_attributes)
{
    int32_t chan_typea, chan_typeb,  *ptr, window_sizea, window_sizeb;
    orte_qos_base_channel_t *base_chan = (orte_qos_base_channel_t*) qos_channel;
    ptr = &chan_typea;
    if (!orte_get_attribute(&base_chan->attributes, ORTE_QOS_TYPE, (void**)&ptr, OPAL_UINT8))
        return ORTE_ERROR;
    ptr = &chan_typeb;
    if (!orte_get_attribute(qos_attributes, ORTE_QOS_TYPE, (void**)&ptr, OPAL_UINT8))
        return ORTE_ERROR;
    if (chan_typea == chan_typeb) {
        ptr = &window_sizea;
        if (!orte_get_attribute(&base_chan->attributes, ORTE_QOS_WINDOW_SIZE, (void**)&ptr, OPAL_UINT32))
            return ORTE_ERROR;
        ptr = &window_sizeb;
        if (!orte_get_attribute(qos_attributes, ORTE_QOS_WINDOW_SIZE, (void**)&ptr, OPAL_UINT32))
            return ORTE_ERROR;
        return (window_sizea != window_sizeb);
    }
    else
        return ORTE_ERROR;
}
/*static void orte_qos_open_channel_reply_send_callback ( int status,
                                                  orte_process_name_t* sender,
                                                  opal_buffer_t* buffer,
                                                  orte_rml_tag_t tag,
                                                  void* cbdata)
{
    // this is the send call back for open channel reply
    orte_qos_channel_t *channel = (orte_qos_channel_t*) cbdata;
    // if the message was not sent we should retry or complete the request appropriately
    if (status!= ORTE_SUCCESS)
    {
        //retry request.
    }
    // if success then release the buffer and do open channel request completion after receiving response from peer
    OBJ_RELEASE(buffer);
}

static void orte_qos_open_channel_send_callback ( int status,
        orte_process_name_t* sender,
        opal_buffer_t* buffer,
        orte_rml_tag_t tag,
        void* cbdata)
{
    // this is the send call back for open channel request
    orte_qos_open_channel_t *req = (orte_qos_open_channel_t*) cbdata;
    // if the message was not sent we should retry or complete the request appropriately
    if (status!= ORTE_SUCCESS)
    {
        // retry if retriable failure.
        // else call completion handler.
        //remove channel from list
        opal_list_remove_item(&orte_qos_base.open_channels, &req->channel->super);
        OBJ_RELEASE(req->channel);
        // update msg status and channel num so end point can have appropriate info
        req->msg->status = status;
        req->msg->channel_num = ORTE_QOS_INVALID_CHANNEL_NUM;
        ORTE_RML_OPEN_CHANNEL_COMPLETE(req->msg);
        OBJ_RELEASE(req);
    }
    // if success then release the buffer and do open channel request completion after receiving response from peer
    OBJ_RELEASE(buffer);
}

void orte_qos_base_open_channel(int sd, short args, void *cbdata)
{
    opal_buffer_t *buffer; int rc;
    orte_qos_open_channel_t *open_channel;
    orte_qos_open_channel_request_t *req = (orte_qos_open_channel_request_t*)cbdata;
    // create channel on sender side by calling the respective qos module.
    req->post.channel = orte_qos_base_create_channel(req->post.msg->dst, req->post.msg->qos_attributes);
    buffer = OBJ_NEW(opal_buffer_t);
    //pack qos attributes list in buffer
    if (ORTE_SUCCESS != orte_qos_base_pack_attributes(buffer, req->post.msg->qos_attributes)) {
        //invalid attributes complete request with error
    }
    open_channel = OBJ_NEW(orte_qos_open_channel_t);
    open_channel->msg = req->post.msg;
    open_channel->channel = req->post.channel;
    open_channel->msg->channel_num = open_channel->channel->channel_num;
    OBJ_RELEASE(req);
    //  send request to peer to open channel
    orte_rml.send_buffer_nb( &open_channel->msg->dst, buffer, ORTE_RML_TAG_OPEN_CHANNEL_REQ,
                               orte_qos_open_channel_send_callback,
                                open_channel);
    // now post a recieve for open_channel_response tag
    orte_rml.recv_buffer_nb(&open_channel->msg->dst, ORTE_RML_TAG_OPEN_CHANNEL_REPLY,
                            ORTE_RML_NON_PERSISTENT, orte_qos_open_channel_reply_callback, open_channel);

}  */


/*
void orte_qos_open_channel_recv_callback (int status,
                                                orte_process_name_t* peer,
                                                struct opal_buffer_t* buffer,
                                                orte_rml_tag_t tag,
                                                void* cbdata)
{
    int32_t rc;
    opal_list_t *qos_attributes = OBJ_NEW(opal_list_t);
    orte_qos_channel_t *channel;
    // un pack attributes first
    if ( ORTE_SUCCESS == orte_qos_base_unpack_attributes( buffer, qos_attributes)) {
        // create channel
        if (NULL != (channel = orte_qos_base_create_channel ( *peer, qos_attributes)) ) {
            buffer = OBJ_NEW (opal_buffer_t);
            if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &channel->channel_num , 1, OPAL_INT))) {
                ORTE_ERROR_LOG(rc);
                return;
            }
            // send channel accept to sender with local channel num
            orte_rml.send_buffer_nb ( peer, buffer, ORTE_RML_TAG_OPEN_CHANNEL_REPLY,
                                       orte_qos_open_channel_reply_send_callback,
                                       channel);
        }
        else {
             // reply with error message
        }
    }
    else {
         //reply with error message
    }
}

void orte_qos_open_channel_reply_callback (int status,
        orte_process_name_t* peer,
        struct opal_buffer_t* buffer,
        orte_rml_tag_t tag,
        void* cbdata)
{
    orte_qos_open_channel_t *req = (orte_qos_open_channel_t*) cbdata;
    orte_qos_channel_t * channel = req->channel;
    int32_t count = 1;
    int32_t rc;
      // process open_channel response from a peer for a open channel request
    if (ORTE_SUCCESS == status) {
        // unpack buffer and get peer channel number.

        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &channel->peer_channel_num, &count, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            // do error completion
            channel->state = orte_qos_channel_closed;
            //remove channel from list
            opal_list_remove_item(&orte_qos_base.open_channels, &channel->super);
            OBJ_RELEASE(channel);
            // update msg status and channel num so end point can have appropriate info
            req->msg->status = ORTE_ERR_OPEN_CHANNEL_PEER_RESPONSE_INV;
            req->msg->channel_num = ORTE_QOS_INVALID_CHANNEL_NUM;
        }
        else {
            channel->state = orte_qos_channel_open;
            req->msg->status = ORTE_SUCCESS;
            req->msg->channel_num = channel->channel_num;
        }
    }
    else {
        channel->state = orte_qos_channel_closed;
        //remove channel from list
        opal_list_remove_item(&orte_qos_base.open_channels, &channel->super);
        OBJ_RELEASE(channel);
        // update msg status and channel num so end point can have appropriate info
        req->msg->status = ORTE_ERR_OPEN_CHANNEL_PEER_FAIL;
        req->msg->channel_num = ORTE_QOS_INVALID_CHANNEL_NUM;
    }
    ORTE_RML_OPEN_CHANNEL_COMPLETE(req->msg);
    OBJ_RELEASE(req);
    OBJ_RELEASE(buffer);
    // 1: If success record peer channel number, update channel state.
    //2: If not destroy channel.
    //3: complete openchannel request.
} */


