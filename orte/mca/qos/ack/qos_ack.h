/*
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * QoS Ack Component interface
 *
 *
 *
 */

#ifndef MCA_QOS_ACK_H
#define MCA_QOS_ACK_H

#include "orte_config.h"
#include "orte/mca/qos/qos.h"
#include "orte/mca/qos/base/base.h"
#include "opal/class/opal_hotel.h"

BEGIN_C_DECLS

#define QOS_ACK_SEQ_NUM_UNINITIALIZED 0
#define QOS_ACK_MAX_WINDOW 100
#define QOS_ACK_MAX_OUTSTANDING_MSGS  (QOS_ACK_MAX_WINDOW *2)
/* window timeout in secs - 100 seconds ok?
  TO DO: make this a QOS attribute that can be specified by the user */
#define QOS_ACK_WINDOW_TIMEOUT_IN_SECS  1
#define ACK_WINDOW_COMPLETE         0
#define ACK_TIMEOUT                 1
#define ACK_OUT_OF_ORDER            2
#define ACK_RECV_MISSED_MSG         3 /* received previously missed msgs*/

typedef enum {
    orte_qos_ack_channel_state_inactive = 0,
    orte_qos_ack_channel_state_filling_window = 1,
    orte_qos_ack_channel_state_window_completed = 2,
    orte_qos_ack_channel_state_awaiting_ack = 3,
    orte_qos_ack_channel_state_received_ack = 4,
}orte_qos_ack_channel_state_t ;

/* Ack Qos channel data structure */
typedef struct orte_qos_ack_channel {
    opal_list_item_t super;
     uint32_t channel_num;
    // we retain the attributes  so we can compare channels - we can get rid of this and compare incoming attributes
    // with attributes of interest to this channel type
    opal_list_t attributes;
    /* size of the message window */
    uint32_t window;
    /* window timeout in secs.*/
    uint32_t timeout_secs;
    /* retry msg window on ack fail */
    bool retry;
    /* seq number of the first msg in the active window */
    uint32_t window_first_seq_num;
    /* sequence number of last outgoing msg */
    uint32_t out_msg_seq_num;
    /* sequence number of  last incoming msg */
    uint32_t in_msg_seq_num;
    /* sequence number of the last message acked */
    uint32_t ack_msg_seq_num;
    /* ACK outstanding msgs hotel */
    opal_hotel_t outstanding_msgs;
    /* array for mapping msg seq num to room num for outgoing msgs in hotels */
    int seq_num_to_room_num[QOS_ACK_MAX_OUTSTANDING_MSGS];
    /* channel state */
    orte_qos_ack_channel_state_t state;
    /* window timer event */
    opal_event_t msg_ack_timer_event;
}orte_qos_ack_channel_t;

OBJ_CLASS_DECLARATION(orte_qos_ack_channel_t);

extern orte_qos_module_t  orte_qos_ack_module;
static inline int  orte_qos_ack_channel_get_msg_room (orte_qos_ack_channel_t * ack_chan,
                                                      uint32_t seq_num)
{
     return ack_chan->seq_num_to_room_num[(seq_num % QOS_ACK_MAX_OUTSTANDING_MSGS)];
}

static inline void orte_qos_ack_channel_set_msg_room (orte_qos_ack_channel_t * ack_chan,
                                                      uint32_t seq_num, int room_num)
{
    ack_chan->seq_num_to_room_num[(seq_num % QOS_ACK_MAX_OUTSTANDING_MSGS)] = room_num;
}

ORTE_DECLSPEC void orte_qos_ack_msg_ack_timeout_callback (struct opal_hotel_t *hotel,
                                                      int room_num, void *occupant);
ORTE_DECLSPEC void orte_qos_ack_msg_window_timeout_callback (int fd, short flags, void *cbdata);
ORTE_DECLSPEC void orte_qos_ack_recv_msg_timeout_callback (struct opal_hotel_t *hotel,
                                                      int room_num, void *occupant);
END_C_DECLS

#endif /* MCA_QOS_ACK_H */
