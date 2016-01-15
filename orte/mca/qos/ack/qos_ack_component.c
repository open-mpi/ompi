/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"


#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "orte/mca/oob/base/base.h"
#include "orte/mca/qos/base/base.h"
#include "orte/mca/qos/qos.h"
#include "qos_ack.h"

/* ack module functions */
static int qos_ack_start (void);
static void qos_ack_shutdown (void);
static void* ack_create (opal_list_t *qos_attributes, uint32_t channel_num);
static int ack_open (void *qos_channel,
                       opal_buffer_t * buf);
static int ack_send ( void *qos_channel, orte_rml_send_t *msg);
static int ack_recv (void *channel, orte_rml_recv_t *msg);
static int ack_close (void * channel);
static int ack_init_recv (void *channel, opal_list_t *attributes);
static int ack_cmp (void *channel, opal_list_t *attributes);
static void ack_send_callback (orte_rml_send_t *msg);

/* utility functions */
static inline int send_ack (orte_qos_ack_channel_t * channel,
                            orte_rml_channel_num_t channel_num,
                            uint32_t ack_type,
                            uint32_t last_msg_seq_num);

void orte_qos_ack_channel_process_ack (int status, orte_process_name_t* sender,
                               opal_buffer_t *buffer, orte_rml_tag_t tag, void *cbdata);

void orte_qos_ack_msg_send_callback ( int status,
                                      orte_process_name_t *peer,
                                      struct opal_buffer_t* buffer,
                                      orte_rml_tag_t tag,
                                      void* cbdata);
static inline int process_out_of_order_msg ( orte_qos_ack_channel_t *channel,
                                             orte_rml_recv_t *msg);
/**
 * ack module definition
 */
orte_qos_module_t orte_qos_ack_module = {
   ack_create,
   ack_open,
   ack_send,
   ack_recv,
   ack_close,
   ack_init_recv,
   ack_cmp,
   ack_send_callback
};

/**
 * component definition
 */
mca_qos_base_component_t mca_qos_ack_component = {
    /* First, the mca_base_component_t struct containing meta
         information about the component itself */

    {
        MCA_QOS_BASE_VERSION_2_0_0,

        "ack", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,
        NULL,
    },
    qos_ack_start,
    qos_ack_shutdown,
    orte_qos_ack,
    {
        ack_create,
        ack_open,
        ack_send,
        ack_recv,
        ack_close,
        ack_init_recv,
        ack_cmp,
        ack_send_callback
    }
};

static int qos_ack_start(void) {
   orte_rml.recv_buffer_nb (ORTE_NAME_WILDCARD, ORTE_RML_TAG_MSG_ACK,
                             ORTE_RML_PERSISTENT, orte_qos_ack_channel_process_ack,
                             NULL);
    /* post a persistent recieve for ACK TAG */
    return ORTE_SUCCESS;
}

static void qos_ack_shutdown (void) {
}

static void* ack_create (opal_list_t *qos_attributes, uint32_t channel_num) {
    orte_qos_ack_channel_t * ack_chan;
    int32_t rc;
    uint32_t *type, type_val, *attribute, attribute_val;
    type_val = orte_qos_ack;
    ack_chan = OBJ_NEW (orte_qos_ack_channel_t);
    ack_chan->channel_num = channel_num;
    type = &type_val;
    attribute = &attribute_val;
    /* validate and store ack specific channel attributes */
    /* set channel type */
    if (ORTE_SUCCESS == (rc = orte_set_attribute( &ack_chan->attributes, ORTE_QOS_TYPE, ORTE_ATTR_GLOBAL, (void*)type, OPAL_UINT8))) {
        if( orte_get_attribute (qos_attributes, ORTE_QOS_WINDOW_SIZE, (void**)&attribute, OPAL_UINT32)) {
            if ( QOS_ACK_MAX_WINDOW < (*attribute)) {
                ORTE_ERROR_LOG(OPAL_ERR_VALUE_OUT_OF_BOUNDS);
                OBJ_RELEASE(ack_chan);
            }
            else {
                ack_chan->window = *attribute;
                if (ORTE_SUCCESS != (rc = orte_set_attribute(&ack_chan->attributes, ORTE_QOS_WINDOW_SIZE,
                                          ORTE_ATTR_GLOBAL, (void*)attribute, OPAL_UINT32))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(ack_chan);
                } else {
                    if( orte_get_attribute (qos_attributes, ORTE_QOS_ACK_NACK_TIMEOUT, (void**)&attribute, OPAL_UINT32)) {
                        ack_chan->timeout_secs = *attribute;
                        if (ORTE_SUCCESS != (rc = orte_set_attribute(&ack_chan->attributes, ORTE_QOS_ACK_NACK_TIMEOUT,
                                                  ORTE_ATTR_GLOBAL, (void*)attribute, OPAL_UINT32))) {
                            ORTE_ERROR_LOG(rc);
                            OBJ_RELEASE(ack_chan);
                        } else {
                            if( orte_get_attribute (qos_attributes, ORTE_QOS_MSG_RETRY, NULL, OPAL_BOOL)) {
                                OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                                                     "%s ack_create created channel = %p window = %d timeout =%d retry = %d",
                                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                                     (void*)ack_chan,
                                                     ack_chan->window,
                                                     ack_chan->timeout_secs,
                                                     ack_chan->retry));
                                ack_chan->retry = true;
                                if (ORTE_SUCCESS != (rc = orte_set_attribute(&ack_chan->attributes, ORTE_QOS_MSG_RETRY,
                                                          ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL))) {
                                    ORTE_ERROR_LOG(rc);
                                    OBJ_RELEASE(ack_chan);
                                }
                            } else {
                                ack_chan->retry = false;
                                OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                                                     "%s ack_create created channel = %p window = %d timeout =%d retry = %d",
                                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                                     (void*)ack_chan,
                                                     ack_chan->window,
                                                     ack_chan->timeout_secs,
                                                     ack_chan->retry));
                            }
                        }
                    }else {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(ack_chan);
                    }
                }
            }
        }else
            OBJ_RELEASE(ack_chan);
    }else {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(ack_chan);
    }
    return ack_chan;
}

static int ack_open (void *qos_channel, opal_buffer_t * buf)  {
    int32_t rc = ORTE_SUCCESS;
    uint32_t eviction_timeout;
    orte_qos_ack_channel_t *ack_chan;
    ack_chan = (orte_qos_ack_channel_t*) (qos_channel);
    /* TO DO - need to adjust eviction timeout according to window size
       lets keep max time out for the first pass */
    eviction_timeout = (ack_chan->timeout_secs + QOS_ACK_WINDOW_TIMEOUT_IN_SECS) * 100000;
    /* init outstanding msg hotel */
    opal_hotel_init (&ack_chan->outstanding_msgs, QOS_ACK_MAX_OUTSTANDING_MSGS,
                       orte_event_base, eviction_timeout, 0,
                       orte_qos_ack_msg_ack_timeout_callback);
    OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                         "%s ack_open channel = %p init hotel timeout =%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (void*)ack_chan, eviction_timeout));
    /* set the message window timer event, but don't activate it */
    /*opal_event_set(opal_event_base,
                   &ack_chan->msg_window_timer_event,
                   -1, 0, orte_qos_ack_msg_window_timeout_callback,
                   ack_chan);
    opal_event_set_priority(&ack_chan->msg_window_timer_event, ORTE_MSG_PRI);*/
    /* the Qos module puts the non local attributes  to be sent to the peer in a list at the time of create.
      pack those attributes into the buffer.*/
    if (ORTE_SUCCESS != (rc =  orte_qos_base_pack_attributes(buf, &ack_chan->attributes)))
        ORTE_ERROR_LOG(rc);
    return rc;
}

static int ack_send ( void *qos_channel,  orte_rml_send_t *msg) {
    int32_t room_num;
    orte_qos_ack_channel_t *ack_chan = (orte_qos_ack_channel_t*) (qos_channel);
    if (ack_chan->out_msg_seq_num == ack_chan->window_first_seq_num -1 ) {
        /* begining msg window */
        ack_chan->out_msg_seq_num = ack_chan->window_first_seq_num;
        OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                             "%s ack_send msg = %p to peer = %s\n begining window at seq_num = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (void*)msg, ORTE_NAME_PRINT(&msg->dst), ack_chan->out_msg_seq_num));
        ack_chan->state = orte_qos_ack_channel_state_filling_window;
    }
    else
        ack_chan->out_msg_seq_num++;
    if(ack_chan->out_msg_seq_num - ack_chan->window_first_seq_num == ack_chan->window - 1) {
        /* we are at the end of the window. */
        /* update state */
        ack_chan->state = orte_qos_ack_channel_state_window_completed;
        /* set begin window for next sequence */
        ack_chan->window_first_seq_num = ack_chan->out_msg_seq_num + 1;
    }
    msg->seq_num = ack_chan->out_msg_seq_num;
    /* check msg into hotel */
    if( OPAL_SUCCESS == (opal_hotel_checkin(&ack_chan->outstanding_msgs, msg, &room_num ))) {
        /* store room number */
        orte_qos_ack_channel_set_msg_room(ack_chan, msg->seq_num, room_num);
    } else {
        OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                             "%s ack_send msg = %p to peer = %s returned with error %d\n",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (void*)msg, ORTE_NAME_PRINT(&msg->dst),
                             ORTE_ERR_QOS_ACK_WINDOW_FULL));
        return ORTE_ERR_QOS_ACK_WINDOW_FULL;
    }
    OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                         "%s ack_send msg = %p to peer = %s\n",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (void*)msg, ORTE_NAME_PRINT(&msg->dst)));
    return ORTE_SUCCESS;
}

static inline int send_ack (orte_qos_ack_channel_t * ack_chan,
                             orte_rml_channel_num_t channel_num,
                             uint32_t ack_type, uint32_t last_msg_seq_num)
{
    int rc;
    orte_rml_channel_t *rml_channel;
    opal_buffer_t *buffer;
    uint32_t num_msgs_to_ack = 0;
    uint32_t *ack_seq_num_array;
    uint32_t i;
    rml_channel = orte_rml_base_get_channel (channel_num);
    num_msgs_to_ack = ack_chan->in_msg_seq_num - ack_chan->ack_msg_seq_num + 1;
    OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                         "%s sending ack type = %d \n",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ack_type));
    if ( NULL != (ack_seq_num_array = malloc (sizeof(uint32_t) * num_msgs_to_ack))) {
        for (i = 1; i <= num_msgs_to_ack ; i++) {
            ack_seq_num_array[i-1] = ack_chan->ack_msg_seq_num + i;
            OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                                 "%s ack_recv acking msg %d to peer = %s\n",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ack_seq_num_array[i-1],
                                 ORTE_NAME_PRINT(&rml_channel->peer)));
        }
        ack_seq_num_array[num_msgs_to_ack - 1] = last_msg_seq_num;
        OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                             "%s ack_recv acking last msg %d to peer = %s\n",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ack_seq_num_array[num_msgs_to_ack - 1],
                             ORTE_NAME_PRINT(&rml_channel->peer)));
    }
    else {
        OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                             "%s ack_recv cannot allocate ack array to send ack to peer = %s\n",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&rml_channel->peer)));
        rc = ORTE_ERR_TEMP_OUT_OF_RESOURCE;
        return rc;
    }
    buffer = OBJ_NEW (opal_buffer_t);
    /* pack channel number */
    opal_dss.pack (buffer, &rml_channel->peer_channel, 1, OPAL_UINT32);
    /* pack ack type */
    opal_dss.pack (buffer, &ack_type, 1, OPAL_UINT32);
    /* pack num messages */
    opal_dss.pack (buffer, &num_msgs_to_ack, 1, OPAL_UINT32);
    /* pack seq number array */
    for (i =0; i<num_msgs_to_ack; i++) {
        opal_dss.pack (buffer, &ack_seq_num_array[i], 1 , OPAL_UINT32);
    }
    rc = orte_rml.send_buffer_nb  (&rml_channel->peer, buffer, ORTE_RML_TAG_MSG_ACK,
                                   orte_qos_ack_msg_send_callback, rml_channel);
    if(ORTE_SUCCESS == rc) {
        /* update last acked msg */
        ack_chan->ack_msg_seq_num = last_msg_seq_num;
    } else {
        //TO DO
    }
    return rc;
}

static inline int process_out_of_order_msg ( orte_qos_ack_channel_t *ack_chan,
        orte_rml_recv_t *msg)
{
    int32_t rc, room_num, first_lost_msg_seq_num, num_lost_msgs, i;
    orte_rml_recv_t *out_msg;
    void *occupant = NULL;
    OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                         "%s process_out_of_order_msg msg %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         msg->seq_num));
    /* if this msg is a duplicate - then do nothing */
    if ((orte_qos_ack_channel_get_msg_room(ack_chan, msg->seq_num)) != -1) {
        OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                             "%s process_out_of_order_msg msg %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             msg->seq_num));
        rc = ORTE_ERR_DUPLICATE_MSG;
    }
    else {
        if (OPAL_SUCCESS != (rc = opal_hotel_checkin(&ack_chan->outstanding_msgs, (void*)msg, &room_num))) {
            return rc;
        }
        OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                             "process_out_of_order_msg checked in msg %d in room %d\n",
                              msg->seq_num, room_num));
        orte_qos_ack_channel_set_msg_room (ack_chan, msg->seq_num, room_num);
        rc = ORTE_ERR_OUT_OF_ORDER_MSG;
        /*  check if we need to send an ACK */
        if (ack_chan->ack_msg_seq_num <= ack_chan->in_msg_seq_num) {
            OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                                 "%s process_out_of_order_msg sending ack last seq_num = %d\n",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 msg->seq_num));
            /* send ACK. */
            send_ack (ack_chan, msg->channel_num, ACK_OUT_OF_ORDER, msg->seq_num);
            /* stop window ack timer */
            opal_event_evtimer_del (&ack_chan->msg_ack_timer_event);
        }
        else {
            /* if we got a lost msg - any seq num between in_msg_seq_num and ack_seq_num*/
            if (ack_chan->ack_msg_seq_num > msg->seq_num) {
                /* check if we have got all lost msgs */
                first_lost_msg_seq_num = ack_chan->in_msg_seq_num + 1;
                num_lost_msgs = ack_chan->ack_msg_seq_num - ack_chan->in_msg_seq_num;
                OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                                     "%s process_out_of_order_msg msg %d first_lost_msg =%d num_lost_msgs =%d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     msg->seq_num, first_lost_msg_seq_num, num_lost_msgs));
                for (i =0 ; i < num_lost_msgs; i++) {
                    if ((orte_qos_ack_channel_get_msg_room(ack_chan, first_lost_msg_seq_num +i)) == -1)
                        break;
                }
                if (i == num_lost_msgs) {

                    /* we got all the lost msgs so we can complete all the msgs in the hotel now */
                    /* reset ack_seq_num */
                    ack_chan->ack_msg_seq_num = first_lost_msg_seq_num -1;
                    room_num = 0;
                    for ( i = 0; room_num != -1; i++) {
                        OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                                             "%s process_out_of_order_msg got all lost msgs  completing outstanding msgs %d",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             (first_lost_msg_seq_num + i)));
                        /* evict msg and complete it */
                        room_num = orte_qos_ack_channel_get_msg_room (ack_chan, first_lost_msg_seq_num +i);
                        opal_hotel_checkout_and_return_occupant(&ack_chan->outstanding_msgs, room_num, &occupant);
                        orte_qos_ack_channel_set_msg_room(ack_chan, first_lost_msg_seq_num +i, -1);
                        out_msg = (orte_rml_recv_t *) occupant;
                        if ((NULL != out_msg) && (room_num != -1)) {
                            // set in seq num */
                            ack_chan->in_msg_seq_num = out_msg->seq_num;
                            orte_rml_base_complete_recv_msg(&out_msg);
                            /* completing recv msg to rml */
                            OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                                                 "process_out_of_order_msg completed recv msg %d",
                                                 (first_lost_msg_seq_num + i)));
                            } else {
                            OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                                                 "%s process_out_of_order_msg lost msg %d not in hotel",
                                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                                 (first_lost_msg_seq_num + i)));
                        }
                    } //end for
                    /* send ACK */
                    send_ack (ack_chan, ack_chan->channel_num, ACK_RECV_MISSED_MSG,
                                ack_chan->in_msg_seq_num);
                } //end if (i== num_lost_msgs)
            } // if (ack_chan->ack_msg_seq_num > msg->seq_num)
        } //end else
    } // end duplicate else
    return rc;
}

static int ack_recv (void *qos_channel, orte_rml_recv_t *msg) {
    orte_qos_ack_channel_t *ack_chan;
    ack_chan = (orte_qos_ack_channel_t*) (qos_channel);
    int32_t rc;
    struct timeval ack_timeout;
    OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                         "%s ack_recv msg = %p seq_num = %d from peer = %s\n",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (void*)msg, msg->seq_num,
                         ORTE_NAME_PRINT(&msg->sender)));
    /** HACK - drop every third msg to stimulate lost msg */
 /*   if ((msg->seq_num == 3) && (hack == 0)) {
        OBJ_RELEASE(msg);
        hack = 1;
        return ORTE_ERROR;
    }*/
    /* check if this is the next expected msg*/
    if((ack_chan->in_msg_seq_num + 1 == msg->seq_num) && (ack_chan->ack_msg_seq_num < msg->seq_num))
    {
        /* check if we are at the end of the window */
        if(ack_chan->window == (msg->seq_num - ack_chan->ack_msg_seq_num)) {
            /* stop window ack timer */
            opal_event_evtimer_del (&ack_chan->msg_ack_timer_event);
            rc = send_ack (ack_chan, msg->channel_num, ACK_WINDOW_COMPLETE, msg->seq_num);
        } else {
            if(ack_chan->in_msg_seq_num ==  ack_chan->ack_msg_seq_num) {
                /* begining window -start window ack timer */
                ack_timeout.tv_sec = ack_chan->timeout_secs;
                ack_timeout.tv_usec = 0;
                opal_event_evtimer_add (&ack_chan->msg_ack_timer_event, &ack_timeout);
            }
            rc = ORTE_SUCCESS;
        }
        ack_chan->in_msg_seq_num = msg->seq_num;
    }
    else {
        rc = process_out_of_order_msg(ack_chan, msg);
    }
    return rc;
}

static int ack_close (void * channel) {
    int32_t rc = ORTE_SUCCESS;
    orte_qos_ack_channel_t *ack_chan;
    ack_chan = (orte_qos_ack_channel_t*) (channel);
    /* check if channel is busy (no outstanding msgs */
    if (opal_hotel_is_empty (&ack_chan->outstanding_msgs)) {
        /* no outstanding msgs, release channel */
        OBJ_RELEASE(ack_chan);
        rc = ORTE_SUCCESS;
    } else
        rc = ORTE_ERR_CHANNEL_BUSY;
    return rc;
}

static int ack_init_recv (void *channel, opal_list_t *attributes) {
    int32_t rc = ORTE_SUCCESS;
    uint32_t eviction_timeout;
    orte_qos_ack_channel_t *ack_chan;
    ack_chan = (orte_qos_ack_channel_t*) channel;
    /* TO DO - need to adjust eviction timeout according to window size
       lets keep max time out for the first pass */
    eviction_timeout = (ack_chan->timeout_secs + QOS_ACK_WINDOW_TIMEOUT_IN_SECS) * 100000;
    /* init outstanding msg hotel */
    opal_hotel_init (&ack_chan->outstanding_msgs, QOS_ACK_MAX_OUTSTANDING_MSGS,
                     orte_event_base, eviction_timeout, 0,
                     orte_qos_ack_recv_msg_timeout_callback);
    OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                         "%s ack_open channel = %p init hotel timeout =%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (void*)ack_chan, eviction_timeout));
    opal_event_evtimer_set (orte_event_base, &ack_chan->msg_ack_timer_event,
                            orte_qos_ack_msg_window_timeout_callback, (void *) ack_chan);
    return rc;
}

static int ack_cmp (void *channel, opal_list_t *attributes) {
    return false;

}

static void ack_send_callback (orte_rml_send_t *msg)
{
    /* complete the request back to the user only upon receiving the ack
       nothing to do here, just make sure that the request is in the hotel */
    OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                         "%s ack_send_callback for msg = %p seq num =%d\n",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (void*)msg, msg->seq_num));
    /* if msg->status != SUCCESS - then evict all messages in the window and
       complete them?? */
    if(ORTE_SUCCESS == msg->status) {
#if OPAL_ENABLE_DEBUG
    orte_qos_ack_channel_t *ack_chan;
    ack_chan = (orte_qos_ack_channel_t *) msg->channel->qos_channel_ptr;
#endif
        // nothing to do
        assert((orte_qos_ack_channel_get_msg_room(ack_chan, msg->seq_num)) != -1);
    } else {
        // TO DO : error handling
        OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                             "%s ack_send_callback for msg = %p seq num =%d SEND FAILED status = %d\n",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (void*)msg, msg->seq_num, msg->status));
        /* evict message from hotel and send end of window to receiver?? */

    }
}

void orte_qos_ack_msg_ack_timeout_callback (struct opal_hotel_t *hotel,
                                            int room_num, void *occupant)
{
    orte_rml_send_t *msg;
    orte_qos_ack_channel_t *ack_chan;
    msg = (orte_rml_send_t *) occupant;
    ack_chan = (orte_qos_ack_channel_t*) msg->channel->qos_channel_ptr;
    OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                        "%s orte_qos_ack_msg_ack_timeout_callback for msg = %p seq num =%d\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (void*)msg, msg->seq_num));
    /* for now complete only the msg that timed out
      TO DO : handle the completion of all messages in the window */
    msg->status = ORTE_ERR_ACK_TIMEOUT_SENDER;
    // set room num to -1 for the msg's seq number
    orte_qos_ack_channel_set_msg_room (ack_chan, msg->seq_num , -1);
    // complete the msg
    ORTE_RML_SEND_COMPLETE(msg);
}

void orte_qos_ack_recv_msg_timeout_callback (struct opal_hotel_t *hotel,
                                             int room_num, void *occupant)
{
#if OPAL_ENABLE_DEBUG
    orte_rml_recv_t *msg = (orte_rml_recv_t *) occupant;
#endif
#if 0
    orte_qos_ack_channel_t *ack_chan;
    orte_rml_channel_t *channel;

    channel = orte_rml_base_get_channel(msg->channel_num);
    ack_chan = (orte_qos_ack_channel_t*) channel->qos_channel_ptr;
#endif

    OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                         "%s OOPS received msg = %p seq num =%d timed out on ACK Queue\n",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (void*)msg, msg->seq_num));
    /* Need to determine correct action here as the sender hasn't responded yet to
       a lost msg event */
    /* This is highly unlikely - lets assert to enable debug*/
    assert(0);
    /*
    // set room num to -1 for the msg's seq number
    ack_chan->seq_num_to_room_num[msg->seq_num % QOS_ACK_MAX_OUTSTANDING_MSGS] = -1;
    // complete the msg
    ORTE_RML_REACTIVATE_MESSAGE(msg);*/
}

void orte_qos_ack_channel_process_ack (int status, orte_process_name_t* sender,
                                       opal_buffer_t *buffer,
                                       orte_rml_tag_t tag, void *cbdata)
{
    /*  process ack received for the msg */
    uint32_t num_msgs_acked, channel_num, i;
    int32_t num_values, room_num;
    orte_rml_send_t *msg, *missed_msg;
    void *occupant = NULL;
    orte_rml_channel_t *channel;
    orte_qos_ack_channel_t *ack_chan;
    uint32_t *seq_num_array;
    uint32_t ack_type;
    uint32_t missed_msg_seq_num = 0;
    num_values = 1;
    /* unpack channel number first */
    opal_dss.unpack(buffer, (void*) &channel_num, &num_values, OPAL_UINT32);
    OPAL_OUTPUT_VERBOSE((5, orte_qos_base_framework.framework_output,
                         "orte_qos_ack_channel_process_ack recieved ack on channel = %d",
                         channel_num));
    channel = orte_rml_base_get_channel (channel_num);
    if ((NULL != channel) || (NULL != channel->qos_channel_ptr)) {
        ack_chan = (orte_qos_ack_channel_t *) (channel->qos_channel_ptr);
        seq_num_array = malloc (sizeof(uint32_t) * ack_chan->window);
        num_values = 1;
        /* unpack ack type */
        opal_dss.unpack(buffer, (void*) &ack_type, &num_values, OPAL_UINT32);
        num_values = 1;
        /* unpack num messages acked */
        opal_dss.unpack(buffer, (void*) &num_msgs_acked, &num_values, OPAL_UINT32);
        OPAL_OUTPUT_VERBOSE((5, orte_qos_base_framework.framework_output,
                             "orte_qos_ack_channel_process_ack recieved ack type %d for %d msgs on channel = %d",
                             ack_type, num_msgs_acked, channel_num));
        if (ACK_OUT_OF_ORDER != ack_type)   {
            //handle normal ACK
            for (i = 0; i < num_msgs_acked; i++)
                {
                    opal_dss.unpack(buffer, (void*) &seq_num_array[i], &num_values, OPAL_UINT32);
                    room_num = orte_qos_ack_channel_get_msg_room (ack_chan, seq_num_array[i]);
                    opal_hotel_checkout_and_return_occupant(&ack_chan->outstanding_msgs, room_num, &occupant);
                    orte_qos_ack_channel_set_msg_room(ack_chan, seq_num_array[i], -1);
                    if((occupant != NULL) && (room_num != -1)) {
                        msg = (orte_rml_send_t*) occupant;
                        OPAL_OUTPUT_VERBOSE((10, orte_rml_base_framework.framework_output,
                                             "Releasing sent message with tag %d and seq_num %d after receiving Ack from dest ",
                                             msg->tag, msg->seq_num ));
                        msg->status = ORTE_SUCCESS;
                        ORTE_RML_SEND_COMPLETE(msg);
                    } else {
                        OPAL_OUTPUT_VERBOSE((10, orte_rml_base_framework.framework_output,
                                             "OOPS received an ACK for already completed seq_num =%d ",
                                             seq_num_array[i] ));
                    }
                }
        } else {
            // handle out of order ACK - complete msgs received in order, retry the lost msg.
            for (i = 0; i < num_msgs_acked; i++)
                {
                    opal_dss.unpack(buffer, (void*) &seq_num_array[i], &num_values, OPAL_UINT32);
                    room_num = orte_qos_ack_channel_get_msg_room (ack_chan, seq_num_array[i]);
                    opal_hotel_checkout_and_return_occupant(&ack_chan->outstanding_msgs, room_num, &occupant);
                    orte_qos_ack_channel_set_msg_room(ack_chan, seq_num_array[i], -1);
                    if ((NULL != occupant) && ((i == 0 )|| (seq_num_array[i] == seq_num_array[i-1] +1 ))) {
                        msg = (orte_rml_send_t*) occupant;
                        msg->status = ORTE_SUCCESS;
                        ORTE_RML_SEND_COMPLETE(msg);
                    } else {
                        if (NULL != occupant) {
                            // num_missed_msgs = (seq_num_array[i] - seq_num_array [i-1] - 1);
                            assert( i == num_msgs_acked -1);
                            /* recheck the ith msg */
                            opal_hotel_checkin(&ack_chan->outstanding_msgs, (void*)occupant, &room_num);
                            orte_qos_ack_channel_set_msg_room (ack_chan, seq_num_array[i], room_num);
                            /* resend and recheck all the missed msgs*/
                            missed_msg_seq_num = seq_num_array[i-1] + 1;
                            for (; missed_msg_seq_num < seq_num_array[i]; missed_msg_seq_num++) {
                                room_num = orte_qos_ack_channel_get_msg_room (ack_chan, missed_msg_seq_num);
                                opal_hotel_checkout_and_return_occupant (&ack_chan->outstanding_msgs, room_num, &occupant);
                                assert ( NULL != occupant);
                                missed_msg = (orte_rml_send_t*) occupant;
                                missed_msg->status = ORTE_ERR_LOST_MSG_IN_WINDOW;
                                opal_hotel_checkin(&ack_chan->outstanding_msgs, (void*)missed_msg, &room_num);
                                orte_qos_ack_channel_set_msg_room (ack_chan, missed_msg_seq_num, room_num);
                                /* send this out on wire directly */
                                ORTE_OOB_SEND (missed_msg);
                            } //end for
                        } else {
                            OPAL_OUTPUT_VERBOSE((10, orte_rml_base_framework.framework_output,
                                                 "OOPS received an ACK for already completed seq_num =%d ",
                                                 seq_num_array[i] ));
                        }//end  if (NULL != occupant)
                    } //end else
                } // end for
        }//end out of order ack processing
        free(seq_num_array);
    }else {
        OPAL_OUTPUT_VERBOSE((5, orte_qos_base_framework.framework_output,
                             "orte_qos_ack_channel_msg_ack_recv_callback recieved ack on non existent channel = %d",
                             channel_num));
    }
}


void orte_qos_ack_msg_send_callback ( int status,
                                      orte_process_name_t *peer,
                                      struct opal_buffer_t* buffer,
                                      orte_rml_tag_t tag,
                                      void* cbdata)
{
#if OPAL_ENABLE_DEBUG
    orte_rml_channel_t *channel = (orte_rml_channel_t*) cbdata;
#endif
    OPAL_OUTPUT_VERBOSE ((0, orte_qos_base_framework.framework_output,
                          " orte_qos_ack_msg_send_callback channel num =%d status =%d",
                          channel->channel_num, status));
}

void orte_qos_ack_msg_window_timeout_callback (int fd, short flags, void *cbdata)
{
    // int32_t rc;
    orte_qos_ack_channel_t *ack_chan = (orte_qos_ack_channel_t*) cbdata;
    OPAL_OUTPUT_VERBOSE ((0, orte_qos_base_framework.framework_output,
                          " orte_qos_ack_msg_window_timeout_callback for channel = %p last acked seq num = %d, last received seq num =%d",
                          (void*)ack_chan, ack_chan->ack_msg_seq_num, ack_chan->in_msg_seq_num ));
    /*  send  ack message */
    send_ack(ack_chan, ack_chan->channel_num, ACK_TIMEOUT, ack_chan->in_msg_seq_num);

}



/*** ACK  QOS CLASS INSTANCES   ***/

static void channel_cons (orte_qos_ack_channel_t *ptr)
{
    int i;
    OBJ_CONSTRUCT (&ptr->attributes, opal_list_t);
    ptr->out_msg_seq_num = 0;
    ptr->window_first_seq_num = 1;
    ptr->in_msg_seq_num = 0;
    ptr->ack_msg_seq_num = 0;
    /* init seq num to room num array to -1 */
    for (i =0; i< QOS_ACK_MAX_OUTSTANDING_MSGS; i++)
        ptr->seq_num_to_room_num[i] = -1;
    OBJ_CONSTRUCT (&ptr->outstanding_msgs, opal_hotel_t);
    ptr->state = orte_qos_ack_channel_state_inactive;
}
static void channel_des (orte_qos_ack_channel_t *ptr)
{
   // OPAL_LIST_DESTRUCT(&ptr->attributes);
    //OBJ_DESTRUCT (&ptr->outstanding_msgs);
}
OBJ_CLASS_INSTANCE (orte_qos_ack_channel_t,
                    opal_list_item_t,
                    channel_cons, channel_des);
