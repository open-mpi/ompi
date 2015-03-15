/*
 *
 * Copyright (c) 2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

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

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/qos/base/base.h"


static int unpack_channel_attributes (opal_buffer_t *buffer, opal_list_t *qos_attributes);
static orte_rml_channel_t * get_channel ( orte_process_name_t * peer, opal_list_t *qos_attributes);
static int send_open_channel_reply (orte_process_name_t *peer,
                                    orte_rml_channel_t *channel,
                                    bool accept);

void orte_rml_base_open_channel(int fd, short flags, void *cbdata)
{
    int32_t *type, type_val;
    orte_rml_send_request_t *req = (orte_rml_send_request_t*)cbdata;
    orte_process_name_t peer;
    orte_rml_open_channel_t *open_chan;
    orte_rml_channel_t *channel;
    opal_buffer_t *buffer;
    peer = req->post.channel.dst;
    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_open_channel to peer %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&peer)));
    OPAL_TIMING_EVENT((&tm_rml, "to %s", ORTE_NAME_PRINT(&peer)));
    channel = OBJ_NEW(orte_rml_channel_t);
    channel->channel_num = opal_pointer_array_add (&orte_rml_base.open_channels, channel);
    channel->peer = peer;
    open_chan = OBJ_NEW(orte_rml_open_channel_t);
    open_chan->dst = peer;
    open_chan->qos_attributes = req->post.channel.qos_attributes;
    open_chan->cbfunc = req->post.channel.cbfunc;
    open_chan->cbdata = req->post.channel.cbdata;
   // OBJ_RELEASE(req);
    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_open_channel to peer %s SUCCESS",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&peer)));
     // associate open channel request and the newly created channel object
    open_chan->channel = channel;
    type = &type_val;
    orte_get_attribute( open_chan->qos_attributes, ORTE_QOS_TYPE, (void**)&type, OPAL_UINT8);
    open_chan->channel->qos = (void*) orte_qos_get_module (open_chan->qos_attributes);

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_open_channel type = %d to peer %s ",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         *type,
                         ORTE_NAME_PRINT(&peer)));
    // now associate qos with the channel based on user requested attributes.
    if ( NULL != open_chan->channel->qos)
    {
        open_chan->channel->qos_channel_ptr = orte_qos_create_channel (open_chan->channel->qos, open_chan->qos_attributes,
                                                                       open_chan->channel->channel_num);
        // create rml send for open channel request. Call the corresponding QoS module to pack the attributes.
        buffer = OBJ_NEW (opal_buffer_t);
        // call QoS module to pack attributes
        if ( ORTE_SUCCESS == (orte_qos_open_channel(open_chan->channel->qos, open_chan->channel->qos_channel_ptr, buffer)))
        {
            /* pack channel number at the end */
            opal_dss.pack(buffer, (void*) &open_chan->channel->channel_num, 1, OPAL_UINT32);
            OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                                 "%s rml_open_channel to peer %s SUCCESS sending to peer",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&peer)));
            // now post a recieve for open_channel_response tag
            orte_rml.recv_buffer_nb(&peer, ORTE_RML_TAG_OPEN_CHANNEL_RESP,
                                    ORTE_RML_NON_PERSISTENT, orte_rml_open_channel_resp_callback, open_chan);
            //  send request to peer to open channel
            orte_rml.send_buffer_nb( &peer, buffer, ORTE_RML_TAG_OPEN_CHANNEL_REQ,
                                  orte_rml_open_channel_send_callback,
                                  open_chan);

        } else {
            open_chan->status = ORTE_ERR_PACK_FAILURE;
            ORTE_RML_OPEN_CHANNEL_COMPLETE(open_chan);
            opal_pointer_array_set_item ( &orte_rml_base.open_channels, open_chan->channel->channel_num, NULL);
            // call QoS module to release the QoS channel object.
            orte_qos_close_channel (open_chan->channel->qos, open_chan->channel->qos_channel_ptr);
            OBJ_RELEASE (buffer);
            OBJ_RELEASE(open_chan->channel);
            OBJ_RELEASE(open_chan);
        }
    }
    else
    {
        // do error completion because a component for the requested QoS does not exist
        open_chan->status = ORTE_ERR_QOS_TYPE_UNSUPPORTED;
        ORTE_RML_OPEN_CHANNEL_COMPLETE(open_chan);
        opal_pointer_array_set_item ( &orte_rml_base.open_channels, open_chan->channel->channel_num, NULL);
        OBJ_RELEASE(open_chan->channel);
        OBJ_RELEASE(open_chan);
    }

}

void orte_rml_open_channel_send_callback ( int status,
        orte_process_name_t* sender,
        opal_buffer_t* buffer,
        orte_rml_tag_t tag,
        void* cbdata)
{
    // this is the send call back for open channel request
    orte_rml_open_channel_t *req = (orte_rml_open_channel_t*) cbdata;
    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_open_channel_send_callback to peer %s status = %d",
                         ORTE_NAME_PRINT(sender),
                         ORTE_NAME_PRINT(&req->dst), status));
    // if the message was not sent we should retry or complete the request appropriately
    if (status!= ORTE_SUCCESS)
    {
        req->status = status;
        ORTE_RML_OPEN_CHANNEL_COMPLETE(req);
        opal_pointer_array_set_item ( &orte_rml_base.open_channels, req->channel->channel_num, NULL);
        // call QoS module to release the QoS channel object.
        orte_qos_close_channel (req->channel->qos, req->channel->qos_channel_ptr);
        OBJ_RELEASE(req->channel);
        OBJ_RELEASE(req);
    }
    else {
        // start a timer for response from peer
    }
    //OBJ_RELEASE(buffer);
}

void orte_rml_open_channel_resp_callback (int status,
                                          orte_process_name_t* peer,
                                          struct opal_buffer_t* buffer,
                                          orte_rml_tag_t tag,
                                          void* cbdata)
{
    orte_rml_open_channel_t *req = (orte_rml_open_channel_t*) cbdata;
    orte_rml_channel_t * channel = req->channel;
    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_open_channel_resp_callback to peer %s status = %d channel = %p",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), status,
                         channel));
    int32_t rc;
    bool peer_resp = false;
    int32_t count = 1;
    // unpack peer  response from buffer to determine if peer has accepted the open request
    if ((ORTE_SUCCESS == (rc = opal_dss.unpack(buffer, &peer_resp, &count, OPAL_BOOL))) && peer_resp) {

        OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                             "%s rml_open_channel_resp_callback to peer response = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             peer_resp));
        /* response will contain the peer channel number -  the peer does not have the
           option to change the channel attributes */
        // unpack  and get peer channel number.
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &channel->peer_channel, &count, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            req->status = ORTE_ERR_UNPACK_FAILURE;
            opal_pointer_array_set_item ( &orte_rml_base.open_channels, req->channel->channel_num, NULL);
            // call QoS module to release the QoS channel object.
            orte_qos_close_channel (req->channel->qos, req->channel->qos_channel_ptr);
            OBJ_RELEASE(req->channel);
            // TBD : should we send a close channel to the peer??
        }
        else {
            // call qos module to update the channel state.??
            req->status = ORTE_SUCCESS;
            req->channel->state = orte_rml_channel_open;
        }
    }
    else {
        if (rc) {
            ORTE_ERROR_LOG(rc);
            req->status = ORTE_ERR_UNPACK_FAILURE;
        } else {
            req->status = ORTE_ERR_OPEN_CHANNEL_PEER_REJECT;
        }
        opal_pointer_array_set_item ( &orte_rml_base.open_channels, req->channel->channel_num, NULL);
        // call QoS module to release the QoS channel object.
        orte_qos_close_channel (req->channel->qos, req->channel->qos_channel_ptr);
        OBJ_RELEASE(req->channel);
    }
    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_open_channel_resp_callback to peer %s status = %d channel =%p num = %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), req->status,
                         channel, channel->channel_num));
    ORTE_RML_OPEN_CHANNEL_COMPLETE(req);
    OBJ_RELEASE(req);
}

static int unpack_channel_attributes (opal_buffer_t *buffer,
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
    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_unpack_attributes num attributes = %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         count));
    for (k=0; k < count; k++) {
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &kv,
                                  &n, ORTE_ATTRIBUTE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                             "rml_unpack_attributes unpacked attribute key = %d, value = %d ",
                              kv->key,
                              kv->data.uint8));
        kv->local = ORTE_ATTR_GLOBAL;
        opal_list_append(qos_attributes, &kv->super);
    }
    return rc;
}

void orte_rml_open_channel_recv_callback (int status,
        orte_process_name_t* peer,
        struct opal_buffer_t* buffer,
        orte_rml_tag_t tag,
        void* cbdata)
{
    opal_list_t qos_attributes;
    orte_rml_channel_t *channel;
    uint8_t *type, type_val = 10;
    int32_t count =1;
    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_open_channel_recv_callback from peer %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer)));
    OBJ_CONSTRUCT(&qos_attributes, opal_list_t);
    /* unpack attributes first */
    if ( ORTE_SUCCESS == unpack_channel_attributes( buffer, &qos_attributes)) {
        type = &type_val;
        orte_get_attribute( &qos_attributes, ORTE_QOS_TYPE, (void**)&type, OPAL_UINT8);
        OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                             "rml_open_channel_recv_callback type =%d",
                             type_val));
        /* scan the list of channels to see if we already have a channel with qos_attributes */
        if (NULL == (channel = get_channel ( peer, &qos_attributes))) {
            /* create a new channel for the req */
            channel = OBJ_NEW(orte_rml_channel_t);
            channel->channel_num = opal_pointer_array_add (&orte_rml_base.open_channels, channel);
            OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                                 "rml_open_channel_recv_callback channel num =%d",
                                 channel->channel_num));
            channel->peer = *peer;
            channel->receive = true;
            channel->qos = (void*) orte_qos_get_module (&qos_attributes);
            /* now associate qos with the channel based on requested attributes */
            channel->qos_channel_ptr = (void*) orte_qos_create_channel(channel->qos, &qos_attributes,
                                               channel->channel_num);
            if (channel->qos_channel_ptr) {
                /* call qos to init recv state */
                 orte_qos_init_recv_channel ( channel->qos, channel->qos_channel_ptr, &qos_attributes);
                /* send channel accept reply to sender */
                if(ORTE_SUCCESS == send_open_channel_reply (peer, channel, true))  {
                    /* update channel state */
                    channel->state = orte_rml_channel_open;
                    /*store src channel number */
                    opal_dss.unpack(buffer, (void*) &channel->peer_channel, &count, OPAL_UINT32);
                }
                else {
                    /* the receiver shall not attempt to resend  or send a reject message
                     instead we let the sender's request timeout at his end.
                     release the channel etc */
                    opal_pointer_array_set_item ( &orte_rml_base.open_channels, channel->channel_num, NULL);
                    orte_qos_close_channel (channel->qos, channel->qos_channel_ptr);
                    OBJ_RELEASE(channel);
                }
            } else {
                send_open_channel_reply (peer, NULL, false);
                opal_pointer_array_set_item ( &orte_rml_base.open_channels, channel->channel_num, NULL);
                //orte_qos_close_channel (channel->qos, channel->qos_channel_ptr);
                OBJ_RELEASE(channel);
            }
        }
        else {
            /*this means that there exists a channel with the same attributes which was
              previously created on user or sender's open channel request
              send channel accept reply to sender */
            if(ORTE_SUCCESS == send_open_channel_reply (peer, channel, true))
                /* exercise caution while updating state of a bidirectional channel*/
                channel->state = orte_rml_channel_open;
            else {
                /* the receiver shall not attempt to resend  or send a reject message
                   instead we let the sender's request timeout at his end.
                   release the channel etc */
                opal_pointer_array_set_item ( &orte_rml_base.open_channels, channel->channel_num, NULL);
                orte_qos_close_channel (channel->qos, channel->qos_channel_ptr);
                OBJ_RELEASE(channel);
            }
        }

    }
    else {
        //reply with error message
        send_open_channel_reply (peer, NULL, false);
    }
}

static int send_open_channel_reply (orte_process_name_t *peer,
                                     orte_rml_channel_t *channel,
                                     bool accept)
{
    opal_buffer_t *buffer;
    int32_t rc;
    buffer = OBJ_NEW (opal_buffer_t);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &accept , 1, OPAL_BOOL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (accept) {
        if (OPAL_SUCCESS != (rc = opal_dss.pack(buffer, &channel->channel_num , 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    /* TBD: should specify reason for reject
      send channel accept to sender */
    orte_rml.send_buffer_nb ( peer, buffer, ORTE_RML_TAG_OPEN_CHANNEL_RESP,
                              orte_rml_open_channel_reply_send_callback,
                              channel);

    return rc;
}

static orte_rml_channel_t * get_channel ( orte_process_name_t * peer, opal_list_t *qos_attributes)
{
    orte_rml_channel_t *channel = NULL;
    int32_t i = 0;
    for (i=0; i < orte_rml_base.open_channels.size; i++) {
        if (NULL != (channel = (orte_rml_channel_t*) opal_pointer_array_get_item (&orte_rml_base.open_channels, i))) {
            /* compare basic properties */
            if ((OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &channel->peer, peer)) &&
                                            ((orte_rml_channel_open == channel->state) ||
                                             (orte_rml_channel_opening == channel->state)))
            {
                /* compare channel attributes */
                if( ORTE_SUCCESS == orte_qos_cmp_channel ( channel->qos, channel->qos_channel_ptr, qos_attributes)) {
                    /* we have an existing channel that we can use */
                    /* make it a receive channel and inform qos to init recv state */
                    channel->receive = true;
                    orte_qos_init_recv_channel ( channel->qos, channel->qos_channel_ptr, qos_attributes);
                    return channel;
                }
                else
                    return NULL;
            }
        }
    }
    return NULL;
}

void orte_rml_open_channel_reply_send_callback ( int status,
        orte_process_name_t* sender,
        opal_buffer_t* buffer,
        orte_rml_tag_t tag,
        void* cbdata)
{
    // this is the send call back for open channel reply
    orte_rml_channel_t *channel = (orte_rml_channel_t*) cbdata;
    // if the message was not sent we should retry or release the channel resources
    if (status!= ORTE_SUCCESS)
    {
        ORTE_ERROR_LOG (status);
        // release channel
        if(NULL != channel) {
            opal_pointer_array_set_item ( &orte_rml_base.open_channels, channel->channel_num, NULL);
            // call QoS module to release the QoS channel object.
            orte_qos_close_channel (channel->qos, channel->qos_channel_ptr);
            OBJ_RELEASE(channel);
        } else {
            // we did not accept the request so nothing to do
        }
    }
    // if success then release the buffer and do open channel request completion after receiving response from peer
    OBJ_RELEASE(buffer);
}

orte_rml_channel_t * orte_rml_base_get_channel (orte_rml_channel_num_t chan_num) {
    orte_rml_channel_t * channel;

    channel = (orte_rml_channel_t*) opal_pointer_array_get_item (&orte_rml_base.open_channels, chan_num);
  /*  if (NULL != channel)
        OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "orte_rml_base_get_channel channel = %p num=%d qos_channel= %p state =%d",
                          channel, chan_num, channel->qos_channel_ptr, channel->state));
    else
        OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                             "orte_rml_base_get_channel channel %d is null",
                              chan_num));*/
    if ((NULL != channel) && (orte_rml_channel_open == channel->state))
        return channel;
    else
        return NULL;
    return channel;
}

void orte_rml_base_prep_send_channel (orte_rml_channel_t *channel,
                                      orte_rml_send_t *send)
{
    // add channel number and notify Qos
    send->dst_channel = channel->peer_channel;
    orte_qos_send_channel (channel->qos, channel->qos_channel_ptr, send);
}

void orte_rml_base_process_recv_channel (orte_rml_channel_t *channel,
        orte_rml_recv_t *recv)
{
     // call qos for recv post processing
    orte_qos_recv_channel (channel->qos, channel->qos_channel_ptr, recv);
}
