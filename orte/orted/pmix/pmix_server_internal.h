/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC. 
 *                         All rights reserved.
 * Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef _PMIX_SERVER_INTERNAL_H_
#define _PMIX_SERVER_INTERNAL_H_

#include "orte_config.h"
#include "orte/types.h"

#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif

#include "opal/types.h"
#include "opal/mca/base/base.h"
#include "opal/mca/event/event.h"


BEGIN_C_DECLS

/**
 * the state of the connection
 */
typedef enum {
    PMIX_SERVER_UNCONNECTED,
    PMIX_SERVER_CLOSED,
    PMIX_SERVER_RESOLVE,
    PMIX_SERVER_CONNECTING,
    PMIX_SERVER_CONNECT_ACK,
    PMIX_SERVER_CONNECTED,
    PMIX_SERVER_FAILED,
    PMIX_SERVER_ACCEPTING
} pmix_server_state_t;

/* define a command type for client-server communications */
typedef uint8_t pmix_cmd_t;
#define PMIX_CMD_T OPAL_UINT8

/* define some commands */
#define PMIX_ABORT_CMD  1
#define PMIX_FENCE_CMD  2
#define PMIX_PUT_CMD    3
#define PMIX_GET_CMD    4

/* define some message types */
#define PMIX_USOCK_IDENT  1
#define PMIX_USOCK_USER   2

/* header for pmix client-server msgs - must
 * match that in opal/mca/pmix/native! */
typedef struct {
    opal_identifier_t id;
    uint8_t type;
    uint32_t tag;
    size_t nbytes;
} pmix_server_hdr_t;

/* usock structure for sending a message */
typedef struct {
    opal_list_item_t super;
    pmix_server_hdr_t hdr;
    char *data;
    bool hdr_sent;
    char *sdptr;
    size_t sdbytes;
} pmix_server_send_t;
OBJ_CLASS_DECLARATION(pmix_server_send_t);

/* usock structure for recving a message */
typedef struct {
    opal_list_item_t super;
    pmix_server_hdr_t hdr;
    bool hdr_recvd;
    char *data;
    char *rdptr;
    size_t rdbytes;
} pmix_server_recv_t;
OBJ_CLASS_DECLARATION(pmix_server_recv_t);

/* object for tracking peers */
typedef struct {
    opal_object_t super;
    /* although not required, there is enough debug
     * value that retaining the name makes sense
     */
    orte_process_name_t name;
    int sd;
    int retries;                  // number of times we have tried to connect to this address
    pmix_server_state_t state;
    opal_event_t op_event;      // used for connecting and operations other than read/write
    opal_event_t send_event;    /**< registration with event thread for send events */
    bool send_ev_active;
    opal_event_t recv_event;    /**< registration with event thread for recv events */
    bool recv_ev_active;
    opal_event_t timer_event;   /**< timer for retrying connection failures */
    bool timer_ev_active;
    opal_list_t send_queue;      /**< list of messages to send */
    pmix_server_send_t *send_msg; /**< current send in progress */
    pmix_server_recv_t *recv_msg; /**< current recv in progress */
} pmix_server_peer_t;
OBJ_CLASS_DECLARATION(pmix_server_peer_t);

/*
 * Data structure for accepting connections.
 */
struct pmix_server_listener_t {
    opal_object_t super;
    bool ev_active;
    opal_event_t event;
    int sd;
};
typedef struct pmix_server_listener_t pmix_server_listener_t;
OBJ_CLASS_DECLARATION(pmix_server_listener_t);

typedef struct {
    opal_object_t super;
    opal_event_t ev;
    pmix_server_peer_t *peer;
} pmix_server_peer_op_t;
OBJ_CLASS_DECLARATION(pmix_server_peer_op_t);

#define ORTE_ACTIVATE_USOCK_PEER_OP(p, cbfunc)          \
    do {                                                \
        pmix_server_peer_op_t *op;                      \
        op = OBJ_NEW(pmix_server_peer_op_t);            \
        op->peer = (p);                                 \
        opal_event_set(orte_event_base, &op->ev, -1,    \
                       OPAL_EV_WRITE, (cbfunc), op);    \
        opal_event_set_priority(&op->ev, ORTE_MSG_PRI); \
        opal_event_active(&op->ev, OPAL_EV_WRITE, 1);   \
    } while(0);

/* Queue a message to be sent to a specified peer. The macro
 * checks to see if a message is already in position to be
 * sent - if it is, then the message provided is simply added
 * to the peer's message queue. If not, then the provided message
 * is placed in the "ready" position
 *
 * If the provided boolean is true, then the send event for the
 * peer is checked and activated if not already active. This allows
 * the macro to either immediately send the message, or to queue
 * it as "pending" for later transmission - e.g., after the
 * connection procedure is completed
 *
 * p => pointer to pmix_server_peer_t
 * s => pointer to pmix_server_send_t
 * f => true if send event is to be activated
 */
#define PMIX_SERVER_QUEUE_MSG(p, s, f)                                \
    do {                                                                \
        opal_output_verbose(5, orte_oob_base_framework.framework_output, \
                            "%s:[%s:%d] queue msg to %s",               \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                            __FILE__, __LINE__,                         \
                            ORTE_NAME_PRINT(&((s)->hdr.dst)));          \
        /* if there is no message on-deck, put this one there */        \
        if (NULL == (p)->send_msg) {                                    \
            (p)->send_msg = (s);                                        \
        } else {                                                        \
            /* add it to the queue */                                   \
            opal_list_append(&(p)->send_queue, &(s)->super);            \
        }                                                               \
        if ((f)) {                                                      \
            /* if we aren't connected, then start connecting */         \
            if (PMIX_SERVER_CONNECTED != (p)->state) {                \
                (p)->state = PMIX_SERVER_CONNECTING;                  \
                ORTE_ACTIVATE_USOCK_CONN_STATE((p),                     \
                                       pmix_server_peer_try_connect); \
            } else {                                                    \
                /* ensure the send event is active */                   \
                if (!(p)->send_ev_active) {                             \
                    opal_event_add(&(p)->send_event, 0);                \
                    (p)->send_ev_active = true;                         \
                }                                                       \
            }                                                           \
        }                                                               \
    }while(0);

/* queue a message to be sent by one of our modules - must
 * provide the following params:
 *
 * m - the RML message to be sent
 * p - the final recipient
 */
#define PMIX_SERVER_QUEUE_SEND(m, p)                                  \
    do {                                                                \
        pmix_server_send_t *msg;                                      \
        int i;                                                          \
        opal_output_verbose(5, orte_oob_base_framework.framework_output, \
                            "%s:[%s:%d] queue send to %s",              \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                            __FILE__, __LINE__,                         \
                            ORTE_NAME_PRINT(&((m)->dst)));              \
        msg = OBJ_NEW(pmix_server_send_t);                            \
        /* setup the header */                                          \
        msg->hdr.origin = (m)->origin;                                  \
        msg->hdr.dst = (m)->dst;                                        \
        msg->hdr.type = PMIX_SERVER_USER;                             \
        msg->hdr.tag = (m)->tag;                                        \
        /* point to the actual message */                               \
        msg->msg = (m);                                                 \
        /* set the total number of bytes to be sent */                  \
        if (NULL != (m)->buffer) {                                      \
            msg->hdr.nbytes = (m)->buffer->bytes_used;                  \
        } else if (NULL != (m)->iov) {                                  \
            msg->hdr.nbytes = 0;                                        \
            for (i=0; i < (m)->count; i++) {                            \
                msg->hdr.nbytes += (m)->iov[i].iov_len;                 \
            }                                                           \
        } else {                                                        \
            msg->hdr.nbytes = (m)->count;                               \
        }                                                               \
        /* start the send with the header */                            \
        msg->sdptr = (char*)&msg->hdr;                                  \
        msg->sdbytes = sizeof(pmix_server_hdr_t);                     \
        /* add to the msg queue for this peer */                        \
        PMIX_SERVER_QUEUE_MSG((p), msg, true);                        \
    }while(0);

/* queue a message to be sent by one of our modules upon completing
 * the connection process - must provide the following params:
 *
 * m - the RML message to be sent
 * p - the final recipient
 */
#define PMIX_SERVER_QUEUE_PENDING(m, p)                               \
    do {                                                                \
        pmix_server_send_t *msg;                                      \
        int i;                                                          \
        opal_output_verbose(5, orte_oob_base_framework.framework_output, \
                            "%s:[%s:%d] queue pending to %s",           \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                            __FILE__, __LINE__,                         \
                            ORTE_NAME_PRINT(&((m)->dst)));              \
        msg = OBJ_NEW(pmix_server_send_t);                            \
        /* setup the header */                                          \
        msg->hdr.origin = (m)->origin;                                  \
        msg->hdr.dst = (m)->dst;                                        \
        msg->hdr.type = PMIX_SERVER_USER;                             \
        msg->hdr.tag = (m)->tag;                                        \
        /* point to the actual message */                               \
        msg->msg = (m);                                                 \
        /* set the total number of bytes to be sent */                  \
        if (NULL != (m)->buffer) {                                      \
            msg->hdr.nbytes = (m)->buffer->bytes_used;                  \
        } else if (NULL != (m)->iov) {                                  \
            msg->hdr.nbytes = 0;                                        \
            for (i=0; i < (m)->count; i++) {                            \
                msg->hdr.nbytes += (m)->iov[i].iov_len;                 \
            }                                                           \
        } else {                                                        \
            msg->hdr.nbytes = (m)->count;                               \
        }                                                               \
        /* start the send with the header */                            \
        msg->sdptr = (char*)&msg->hdr;                                  \
        msg->sdbytes = sizeof(pmix_server_hdr_t);                     \
        /* add to the msg queue for this peer */                        \
        PMIX_SERVER_QUEUE_MSG((p), msg, false);                       \
    }while(0);

/* State machine for processing message */
typedef struct {
    opal_object_t super;
    opal_event_t ev;
    pmix_server_send_t *msg;
} pmix_server_msg_op_t;
OBJ_CLASS_DECLARATION(pmix_server_msg_op_t);

#define ORTE_ACTIVATE_USOCK_POST_SEND(ms, cbfunc)                       \
    do {                                                                \
        pmix_server_msg_op_t *mop;                                    \
        opal_output_verbose(5, orte_oob_base_framework.framework_output, \
                            "%s:[%s:%d] post send to %s",               \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                            __FILE__, __LINE__,                         \
                            ORTE_NAME_PRINT(&((ms)->dst)));             \
        mop = OBJ_NEW(pmix_server_msg_op_t);                          \
        mop->msg = (ms);                                                \
        opal_event_set(orte_event_base, &mop->ev, -1,      \
                       OPAL_EV_WRITE, (cbfunc), mop);                   \
        opal_event_set_priority(&mop->ev, ORTE_MSG_PRI);                \
        opal_event_active(&mop->ev, OPAL_EV_WRITE, 1);                  \
    } while(0);



/* State machine for connection operations */
typedef struct {
    opal_object_t super;
    pmix_server_peer_t *peer;
    opal_event_t ev;
} pmix_server_conn_op_t;
OBJ_CLASS_DECLARATION(pmix_server_conn_op_t);

#define CLOSE_THE_SOCKET(socket)    \
    do {                            \
        shutdown(socket, 2);        \
        close(socket);              \
    } while(0)

#define ORTE_ACTIVATE_USOCK_CONN_STATE(p, cbfunc)                       \
    do {                                                                \
        pmix_server_conn_op_t *cop;                                   \
        opal_output_verbose(5, orte_oob_base_framework.framework_output, \
                            "%s:[%s:%d] connect to %s",                 \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                            __FILE__, __LINE__,                         \
                            ORTE_NAME_PRINT((&(p)->name)));             \
        cop = OBJ_NEW(pmix_server_conn_op_t);                         \
        cop->peer = (p);                                                \
        opal_event_set(orte_event_base, &cop->ev, -1,      \
                       OPAL_EV_WRITE, (cbfunc), cop);                   \
        opal_event_set_priority(&cop->ev, ORTE_MSG_PRI);                \
        opal_event_active(&cop->ev, OPAL_EV_WRITE, 1);                  \
    } while(0);

#define ORTE_ACTIVATE_USOCK_ACCEPT_STATE(s, a, cbfunc)                  \
    do {                                                                \
        pmix_server_conn_op_t *cop;                                   \
        cop = OBJ_NEW(pmix_server_conn_op_t);                         \
        opal_event_set(orte_event_base, &cop->ev, s,       \
                       OPAL_EV_READ, (cbfunc), cop);                    \
        opal_event_set_priority(&cop->ev, ORTE_MSG_PRI);                \
        opal_event_add(&cop->ev, 0);                                    \
    } while(0);

#define ORTE_RETRY_USOCK_CONN_STATE(p, cbfunc, tv)                      \
    do {                                                                \
        pmix_server_conn_op_t *cop;                                   \
        opal_output_verbose(5, orte_oob_base_framework.framework_output, \
                            "%s:[%s:%d] retry connect to %s",           \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                            __FILE__, __LINE__,                         \
                            ORTE_NAME_PRINT((&(p)->name)));             \
        cop = OBJ_NEW(pmix_server_conn_op_t);                         \
        cop->peer = (p);                                                \
        opal_event_evtimer_set(orte_event_base,            \
                               &cop->ev,                                \
                               (cbfunc), cop);                          \
        opal_event_evtimer_add(&cop->ev, (tv));                         \
    } while(0);


/* expose shared functions */
extern void pmix_server_send_handler(int fd, short args, void *cbdata);
extern void pmix_server_recv_handler(int fd, short args, void *cbdata);
extern int pmix_server_peer_recv_connect_ack(pmix_server_peer_t* pr,
                                             int sd, pmix_server_hdr_t *dhdr);
extern void pmix_server_recv_handler(int sd, short flags, void *cbdata);
extern void pmix_server_peer_connected(pmix_server_peer_t* peer);
extern int pmix_server_send_connect_ack(pmix_server_peer_t* peer);
extern int pmix_server_recv_connect_ack(pmix_server_peer_t* pr, int sd,
                                        pmix_server_hdr_t *dhdr);
extern void pmix_server_peer_event_init(pmix_server_peer_t* peer);
extern char* pmix_server_state_print(pmix_server_state_t state);
extern pmix_server_peer_t* pmix_server_peer_lookup(const orte_process_name_t *name);
extern void pmix_server_peer_dump(pmix_server_peer_t* peer, const char* msg);


/* exposed shared variables */
extern bool pmix_server_distribute_data;
extern opal_hash_table_t *pmix_server_peers;
extern int pmix_server_verbosity;
extern int pmix_server_output;

END_C_DECLS

#endif /* PMIX_SERVER_INTERNAL_H_ */

