/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef ORTE_MCA_RMCAST_PRIVATE_H
#define ORTE_MCA_RMCAST_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#include "opal/event/event.h"
#include "opal/class/opal_list.h"

#include "orte/mca/rmcast/rmcast.h"

BEGIN_C_DECLS

#if !ORTE_DISABLE_FULL_SUPPORT

#define CLOSE_THE_SOCKET(socket)    \
    do {                            \
        shutdown(socket, 2);        \
        close(socket);              \
    } while(0)



/*
 * globals that might be needed
 */
typedef struct {
    int rmcast_output;
    opal_list_t rmcast_opened;
    uint32_t xmit_network;
    char *my_group_name;
    uint8_t my_group_number;
    uint32_t interface;
    uint16_t ports[256];
} orte_rmcast_base_t;

ORTE_DECLSPEC extern orte_rmcast_base_t orte_rmcast_base;


/****    CLASS DEFINITIONS    ****/
/*
 * Data structure for tracking assigned channels
 */
typedef struct {
    opal_list_item_t item;
    char *name;
    orte_rmcast_channel_t channel;
    uint32_t network;
    uint16_t port;
    uint32_t interface;
    int xmit;
    int recv;
    struct sockaddr_in addr;
    opal_event_t send_ev;
    opal_mutex_t send_lock;
    bool sends_in_progress;
    opal_list_t pending_sends;
    uint8_t *send_data;
    opal_event_t recv_ev;
} rmcast_base_channel_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(rmcast_base_channel_t);


/*
 * Data structure for tracking registered non-blocking recvs
 */
typedef struct {
    opal_list_item_t item;
    orte_rmcast_channel_t channel;
    bool recvd;
    opal_buffer_t *data;
    orte_rmcast_tag_t tag;
    orte_rmcast_flag_t flags;
    orte_rmcast_callback_fn_t cbfunc;
    void *cbdata;
} rmcast_base_recv_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(rmcast_base_recv_t);


/*
 * Data structure for tracking pending sends
 */
typedef struct {
    opal_list_item_t item;
    opal_buffer_t *data;
    orte_rmcast_tag_t tag;
    orte_rmcast_callback_fn_t cbfunc;
    void *cbdata;
} rmcast_base_send_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(rmcast_base_send_t);


/* Setup an event to process a multicast message
 *
 * Multicast messages can come at any time and rate. To minimize
 * the probability of loss, and to avoid conflict when we send
 * data when responding to an input message, we use a timer
 * event to break out of the recv and process the message later
 */
typedef struct {
    opal_object_t super;
    opal_event_t *ev;
    uint8_t *data;
    ssize_t sz;
    rmcast_base_channel_t *channel;
} orte_mcast_msg_event_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_mcast_msg_event_t);

#define ORTE_MULTICAST_MESSAGE_EVENT(dat, n, chan, cbfunc)      \
    do {                                                        \
        orte_mcast_msg_event_t *mev;                            \
        struct timeval now;                                     \
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,              \
                            "defining mcast msg event: %s %d",  \
                            __FILE__, __LINE__));               \
        mev = OBJ_NEW(orte_mcast_msg_event_t);                  \
        mev->data = (dat);                                      \
        mev->sz = (n);                                          \
        mev->channel = (chan);                                  \
        opal_evtimer_set(mev->ev, (cbfunc), mev);               \
        now.tv_sec = 0;                                         \
        now.tv_usec = 0;                                        \
        opal_evtimer_add(mev->ev, &now);                        \
    } while(0);



#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS

#endif
