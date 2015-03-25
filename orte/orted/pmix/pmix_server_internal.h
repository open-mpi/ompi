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
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved. 
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "opal/mca/pmix/pmix.h"
#include "opal/util/proc.h"

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
#define PMIX_ABORT_CMD        1
#define PMIX_FENCE_CMD        2
#define PMIX_FENCENB_CMD      3
#define PMIX_PUT_CMD          4
#define PMIX_GET_CMD          5
#define PMIX_GETNB_CMD        6
#define PMIX_FINALIZE_CMD     7
#define PMIX_GETATTR_CMD      8

/* define some message types */
#define PMIX_USOCK_IDENT  1
#define PMIX_USOCK_USER   2

/* header for pmix client-server msgs - must
 * match that in opal/mca/pmix/native! */
typedef struct {
    opal_process_name_t id;
    uint8_t type;
    uint32_t tag;
    size_t nbytes;
} pmix_server_hdr_t;

/* usock structure for sending a message */
typedef struct {
    opal_list_item_t super;
    pmix_server_hdr_t hdr;
    opal_buffer_t *data;
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

/* object for tracking peers - each peer can have multiple
 * connections. This can occur if the initial app executes
 * a fork/exec, and the child initiates its own connection
 * back to the PMIx server. Thus, the trackers are "indexed"
 * by the socket, not the process name */
typedef struct {
    opal_object_t super;
    int sd;
    orte_process_name_t name;
    char *auth_method;         // method used by peer to authenticate
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

/* object for tracking remote modex requests so we can
 * correctly route the eventual reply */
typedef struct {
    opal_list_item_t super;
    pmix_server_peer_t *peer;
    orte_proc_t *proxy;
    opal_process_name_t target;
    uint32_t tag;
} pmix_server_dmx_req_t;
OBJ_CLASS_DECLARATION(pmix_server_dmx_req_t);

/* queue a message to be sent by one of our procs - must
 * provide the following params:
 *
 * p - the peer object of the process
 * t - tag to be sent to
 * b - buffer to be sent
 */
#define PMIX_SERVER_QUEUE_SEND(p, t, b)                                 \
    do {                                                                \
        pmix_server_send_t *msg;                                        \
        opal_output_verbose(2, pmix_server_output,                      \
                            "%s:[%s:%d] queue send to %s",              \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                            __FILE__, __LINE__,                         \
                            ORTE_NAME_PRINT(&(p)->name));               \
        msg = OBJ_NEW(pmix_server_send_t);                              \
        /* setup the header */                                          \
        msg->hdr.id = OPAL_PROC_MY_NAME;                                \
        msg->hdr.type = PMIX_USOCK_USER;                                \
        msg->hdr.tag = (t);                                             \
        msg->hdr.nbytes = (b)->bytes_used;                              \
        /* point to the buffer */                                       \
        msg->data = (b);                                                \
        /* start the send with the header */                            \
        msg->sdptr = (char*)&msg->hdr;                                  \
        msg->sdbytes = sizeof(pmix_server_hdr_t);                     \
        /* if there is no message on-deck, put this one there */        \
        if (NULL == (p)->send_msg) {                                    \
            (p)->send_msg = msg;                                        \
        } else {                                                        \
            /* add it to the queue */                                   \
            opal_list_append(&(p)->send_queue, &msg->super);            \
        }                                                               \
        /* ensure the send event is active */                           \
        if (!(p)->send_ev_active) {                                     \
            opal_event_add(&(p)->send_event, 0);                        \
            (p)->send_ev_active = true;                                 \
        }                                                               \
    }while(0);

#define CLOSE_THE_SOCKET(socket)    \
    do {                            \
        shutdown(socket, 2);        \
        close(socket);              \
    } while(0)

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
extern pmix_server_peer_t* pmix_server_peer_lookup(int sd);
extern void pmix_server_peer_dump(pmix_server_peer_t* peer, const char* msg);
extern int pack_segment_info(opal_process_name_t id, opal_buffer_t *reply);

extern int pmix_server_fetch_proc_map(opal_buffer_t *reply,
                                      orte_job_t *jdata,
                                      orte_proc_t *proc);
extern void pmix_server_process_message(pmix_server_peer_t *peer);

/* exposed shared variables */
extern bool pmix_server_distribute_data;
extern opal_hash_table_t *pmix_server_peers;
extern int pmix_server_verbosity;
extern int pmix_server_output;
extern int pmix_server_local_handle, pmix_server_remote_handle, pmix_server_global_handle;
extern opal_list_t pmix_server_pending_dmx_reqs;

END_C_DECLS

#endif /* PMIX_SERVER_INTERNAL_H_ */

