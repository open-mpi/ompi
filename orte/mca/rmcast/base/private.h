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

#include "opal/mca/event/event.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_ring_buffer.h"
#include "opal/util/fd.h"

#include "orte/threads/threads.h"
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
    orte_rmcast_seq_t seq_num;
    int recv;
    struct sockaddr_in addr;
    opal_event_t send_ev;
    opal_mutex_t send_lock;
    bool sends_in_progress;
    opal_list_t pending_sends;
    uint8_t *send_data;
    opal_event_t recv_ev;
    /* ring buffer to cache our messages */
    opal_ring_buffer_t cache;
} rmcast_base_channel_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(rmcast_base_channel_t);


/*
 * Data structure for tracking registered non-blocking recvs
 */
typedef struct {
    opal_list_item_t item;
    orte_process_name_t name;
    orte_rmcast_channel_t channel;
    orte_rmcast_seq_t seq_num;
    orte_thread_ctl_t ctl;
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
    bool retransmit;
    struct iovec *iovec_array;
    int32_t iovec_count;
    opal_buffer_t *buf;
    orte_rmcast_tag_t tag;
    orte_rmcast_callback_fn_t cbfunc_iovec;
    orte_rmcast_callback_buffer_fn_t cbfunc_buffer;
    void *cbdata;
    orte_thread_ctl_t ctl;
} rmcast_base_send_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(rmcast_base_send_t);


/* Data structure for tracking recvd sequence numbers */
typedef struct {
    opal_object_t super;
    orte_process_name_t name;
    orte_rmcast_channel_t channel;
    orte_rmcast_seq_t seq_num;
} rmcast_recv_log_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(rmcast_recv_log_t);


/* Data structure for holding messages in case
 * of retransmit
 */
typedef struct {
    opal_object_t super;
    orte_rmcast_seq_t seq_num;
    orte_rmcast_channel_t channel;
    opal_buffer_t *buf;
} rmcast_send_log_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(rmcast_send_log_t);

#define ORTE_MULTICAST_MESSAGE_EVENT(dt, sz)                            \
    do {                                                                \
        opal_buffer_t *buf;                                             \
        OPAL_OUTPUT_VERBOSE((1, orte_rmcast_base.rmcast_output,         \
                             "defining mcast msg event: %s %d",         \
                             __FILE__, __LINE__));                      \
        buf = OBJ_NEW(opal_buffer_t);                                   \
        opal_dss.load(buf, (dt), (sz));                                 \
        if (orte_progress_threads_enabled) {                            \
            opal_fd_write(orte_rmcast_base.recv_pipe[1],                \
                          sizeof(opal_buffer_t*), &buf);                \
        } else {                                                        \
            orte_rmcast_base_process_msg(buf);                          \
        }                                                               \
   } while(0);


#define ORTE_MULTICAST_NEXT_SEQUENCE_NUM(seq)   \
    do {                                        \
        if ((seq) < ORTE_RMCAST_SEQ_MAX) {      \
            (seq) += 1;                         \
        } else {                                \
            (seq) = 0;                          \
        }                                       \
    } while(0);

/****    FUNCTIONS    ****/
ORTE_DECLSPEC int orte_rmcast_base_queue_recv(rmcast_base_recv_t **recvptr,
                                              orte_rmcast_channel_t channel,
                                              orte_rmcast_tag_t tag,
                                              orte_rmcast_flag_t flags,
                                              orte_rmcast_callback_fn_t cbfunc_iovec,
                                              orte_rmcast_callback_buffer_fn_t cbfunc_buffer,
                                              void *cbdata, bool blocking);

ORTE_DECLSPEC int orte_rmcast_base_queue_xmit(rmcast_base_send_t *snd,
                                              orte_rmcast_channel_t channel,
                                              opal_buffer_t **buffer,
                                              rmcast_base_channel_t **chan);

ORTE_DECLSPEC int orte_rmcast_base_start_threads(bool rcv_thread, bool processing_thread);
ORTE_DECLSPEC void orte_rmcast_base_stop_threads(void);

ORTE_DECLSPEC int orte_rmcast_base_process_msg(opal_buffer_t *msg);

ORTE_DECLSPEC void orte_rmcast_base_cancel_recv(orte_rmcast_channel_t channel,
                                                orte_rmcast_tag_t tag);

ORTE_DECLSPEC int orte_rmcast_base_close_channel(orte_rmcast_channel_t channel);

ORTE_DECLSPEC int orte_rmcast_base_query(orte_rmcast_channel_t *output,
                                         orte_rmcast_channel_t *input);

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS

#endif
