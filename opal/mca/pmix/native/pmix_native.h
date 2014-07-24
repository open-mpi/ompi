/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_PMIX_NATIVE_H
#define MCA_PMIX_NATIVE_H

#include "opal_config.h"

#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/event/event.h"
#include "opal/mca/pmix/pmix.h"

BEGIN_C_DECLS

/**
 * the state of the connection to the server
 */
typedef enum {
    PMIX_USOCK_UNCONNECTED,
    PMIX_USOCK_CLOSED,
    PMIX_USOCK_RESOLVE,
    PMIX_USOCK_CONNECTING,
    PMIX_USOCK_CONNECT_ACK,
    PMIX_USOCK_CONNECTED,
    PMIX_USOCK_FAILED,
    PMIX_USOCK_ACCEPTING
} pmix_usock_state_t;

/* define a command type for communicating to the
 * pmix server */
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

/* internally used cbfunc */
typedef void (*pmix_usock_cbfunc_t)(opal_buffer_t *buf, void *cbdata);

/* header for messages */
typedef struct {
    opal_identifier_t id;
    uint8_t type;
    uint32_t tag;
    size_t nbytes;
} pmix_usock_hdr_t;

/* usock structure for sending a message */
typedef struct {
    opal_list_item_t super;
    opal_event_t ev;
    pmix_usock_hdr_t hdr;
    char *data;
    bool hdr_sent;
    char *sdptr;
    size_t sdbytes;
} pmix_usock_send_t;
OBJ_CLASS_DECLARATION(pmix_usock_send_t);

/* usock structure for recving a message */
typedef struct {
    opal_list_item_t super;
    opal_event_t ev;
    pmix_usock_hdr_t hdr;
    char *data;
    bool hdr_recvd;
    char *rdptr;
    size_t rdbytes;
} pmix_usock_recv_t;
OBJ_CLASS_DECLARATION(pmix_usock_recv_t);

/* usock structure for tracking posted recvs */
typedef struct {
    opal_list_item_t super;
    opal_event_t ev;
    uint32_t tag;
    pmix_usock_cbfunc_t cbfunc;
    void *cbdata;
} pmix_usock_posted_recv_t;
OBJ_CLASS_DECLARATION(pmix_usock_posted_recv_t);


/* usock struct for tracking ops */
typedef struct {
    opal_object_t super;
    opal_event_t ev;
    volatile bool active;
    opal_pmix_cbfunc_t cbfunc;
    void *cbdata;
} pmix_cb_t;
OBJ_CLASS_DECLARATION(pmix_cb_t);


typedef struct {
    opal_pmix_base_component_t super;
    opal_buffer_t *cache;
    opal_event_base_t *evbase;
    opal_identifier_t server;
    char *uri;
    struct sockaddr_un address;
    int sd;
    int max_retries;
    int retries;                  // number of times we have tried to connect to this address
    pmix_usock_state_t state;
    opal_event_t op_event;        // used for connecting and operations other than read/write
    opal_event_t send_event;      // registration with event thread for send events
    bool send_ev_active;
    opal_event_t recv_event;      // registration with event thread for recv events
    bool recv_ev_active;
    opal_event_t timer_event;     // timer for retrying connection failures
    bool timer_ev_active;
    opal_list_t send_queue;       // list of pmix_usock_sent_t to be sent
    pmix_usock_send_t *send_msg;  // current send in progress
    pmix_usock_recv_t *recv_msg;  // current recv in progress
    opal_list_t posted_recvs;     // list of pmix_usock_posted_recv_t
    opal_list_t unmatched_msgs;   // list of pmix_usock_recv_t messages waiting to be matched
} opal_pmix_native_component_t;

OPAL_DECLSPEC extern opal_pmix_native_component_t mca_pmix_native_component;

OPAL_DECLSPEC extern const opal_pmix_base_module_t opal_pmix_native_module;


/* module-level shared functions */
OPAL_MODULE_DECLSPEC void pmix_usock_process_send(int fd, short args, void *cbdata);
OPAL_MODULE_DECLSPEC void pmix_usock_process_msg(int fd, short flags, void *cbdata);
OPAL_MODULE_DECLSPEC void pmix_usock_post_recv(int fd, short args, void *cbdata);
OPAL_MODULE_DECLSPEC void pmix_usock_send_handler(int sd, short flags, void *cbdata);
OPAL_MODULE_DECLSPEC void pmix_usock_recv_handler(int sd, short flags, void *cbdata);
OPAL_MODULE_DECLSPEC char* pmix_usock_state_print(pmix_usock_state_t state);
OPAL_MODULE_DECLSPEC void pmix_usock_dump(const char* msg);
OPAL_MODULE_DECLSPEC int usock_send_connect_ack(void);


/* internal convenience macros */
#define PMIX_ACTIVATE_POST_SEND(b)                                      \
    do {                                                                \
        pmix_usock_send_t *ms;                                          \
        opal_output_verbose(5, opal_pmix_base_framework.framework_output, \
                            "[%s:%d] post send to server",              \
                            __FILE__, __LINE__);                        \
        ms = OBJ_NEW(pmix_usock_send_t);                                \
        ms->hdr.id = *OPAL_MY_ID;                                       \
        ms->hdr.type = PMIX_USOCK_USER;                                 \
        ms->hdr.nbytes = (b)->bytes_used;                               \
        ms->data = (b)->base_ptr;                                       \
        /* always start with the header */                              \
        ms->sdptr = (char*)&ms->hdr;                                    \
        ms->sdbytes = sizeof(pmix_usock_hdr_t);                         \
        opal_event_set(mca_pmix_native_component.evbase, &((ms)->ev), -1, \
                       OPAL_EV_WRITE, pmix_usock_process_send, (ms));   \
        opal_event_set_priority(&((ms)->ev), OPAL_EV_MSG_LO_PRI);       \
        opal_event_active(&((ms)->ev), OPAL_EV_WRITE, 1);               \
    } while(0);

#define PMIX_ACTIVATE_POST_RECV(cb, d)                                  \
    do {                                                                \
        pmix_usock_posted_recv_t *req;                                  \
        opal_output_verbose(5, opal_pmix_base_framework.framework_output, \
                            "[%s:%d] post recv",                        \
                            __FILE__, __LINE__);                        \
        req = OBJ_NEW(pmix_usock_posted_recv_t);                        \
        /* take the next tag in the sequence */                         \
        if (UINT32_MAX == tag) {                                        \
            tag = 0;                                                    \
        }                                                               \
        req->tag = tag++;                                               \
        req->cbfunc = (cb);                                             \
        req->cbdata = (d);                                              \
        opal_event_set(mca_pmix_native_component.evbase, &req->ev, -1,  \
                       OPAL_EV_WRITE,                                   \
                       pmix_usock_post_recv, req);                      \
        opal_event_set_priority(&req->ev, OPAL_EV_MSG_LO_PRI);          \
        opal_event_active(&req->ev, OPAL_EV_WRITE, 1);                  \
    } while(0);

#define PMIX_ACTIVATE_POST_MSG(ms)                                      \
    do {                                                                \
        opal_output_verbose(5, opal_pmix_base_framework.framework_output, \
                            "[%s:%d] post msg",                         \
                            __FILE__, __LINE__);                        \
        opal_event_set(mca_pmix_native_component.evbase, &ms->ev, -1,   \
                       OPAL_EV_WRITE,                                   \
                       pmix_usock_process_msg, ms);                     \
        opal_event_set_priority(&ms->ev, OPAL_EV_MSG_LO_PRI);           \
        opal_event_active(&ms->ev, OPAL_EV_WRITE, 1);                   \
    } while(0);

#define CLOSE_THE_SOCKET(socket)    \
    do {                            \
        shutdown(socket, 2);        \
        close(socket);              \
    } while(0)


#define PMIX_WAIT_FOR_COMPLETION(a)             \
    do {                                        \
        while ((a)) {                           \
            usleep(10);                         \
        }                                       \
    } while (0);

END_C_DECLS

#endif /* MCA_PMIX_NATIVE_H */
