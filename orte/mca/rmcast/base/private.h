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
    orte_process_name_t name;
    orte_rmcast_channel_t channel;
    bool recvd;
    orte_rmcast_tag_t tag;
    orte_rmcast_flag_t flags;
    struct iovec *iovec_array;
    int iovec_count;
    opal_buffer_t *buf;
    orte_rmcast_callback_fn_t cbfunc_iovec;
    orte_rmcast_callback_buffer_fn_t cbfunc_buffer;
    void *cbdata;
} rmcast_base_recv_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(rmcast_base_recv_t);


/*
 * Data structure for tracking pending sends
 */
typedef struct {
    opal_list_item_t item;
    struct iovec *iovec_array;
    int32_t iovec_count;
    opal_buffer_t *buf;
    orte_rmcast_tag_t tag;
    orte_rmcast_callback_fn_t cbfunc_iovec;
    orte_rmcast_callback_buffer_fn_t cbfunc_buffer;
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


#define ORTE_MULTICAST_MESSAGE_HDR_HTON(bfr, tg)    \
    do {                                            \
        uint32_t nm;                                \
        uint16_t tmp;                               \
        nm = htonl(ORTE_PROC_MY_NAME->jobid);       \
        memcpy((bfr), &nm, 4);                      \
        nm = htonl(ORTE_PROC_MY_NAME->vpid);        \
        memcpy((bfr)+4, &nm, 4);                    \
        /* add the tag data, also converted */      \
        tmp = htons(snd->tag);                      \
        memcpy((bfr)+8, &tmp, 2);                   \
    } while(0);

#define ORTE_MULTICAST_MESSAGE_HDR_NTOH(bfr, nm, tg)        \
    do {                                                    \
        uint32_t tmp;                                       \
        uint16_t tmp16;                                     \
        /* extract the name and convert it to host order */ \
        memcpy(&tmp, (bfr), 4);                             \
        (nm)->jobid = ntohl(tmp);                           \
        memcpy(&tmp, (bfr)+4, 4);                           \
        (nm)->vpid = ntohl(tmp);                            \
        /* extract the target tag */                        \
        memcpy(&tmp16, (bfr)+8, 2);                         \
        (tg) = ntohs(tmp16);                                \
    } while(0);

#define ORTE_MULTICAST_LOAD_MESSAGE(bfr, dat, sz, maxsz, endsz) \
    do {                                                        \
        if ((maxsz) <= (sz) + 10) {                             \
            *(endsz) = 0;                                       \
        } else {                                                \
            memcpy((bfr)+10, (dat), (sz));                      \
            *(endsz) = (sz) + 10;                               \
        }                                                       \
    } while(0);

#define ORTE_MULTICAST_UNLOAD_MESSAGE(bfr, dat, sz) \
        opal_dss.load((bfr), (dat)+10, (sz)-10);

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS

#endif
