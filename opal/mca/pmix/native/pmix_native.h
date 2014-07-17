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

/* usock structure for send/recv a message */
typedef struct {
    opal_list_item_t super;
    opal_event_t ev;
    char *data;
    char *crnt_ptr;
    size_t crnt_bytes;
    size_t total_bytes;
} pmix_usock_msg_t;
OBJ_CLASS_DECLARATION(pmix_usock_msg_t);

typedef struct {
    opal_pmix_base_component_t super;
    opal_buffer_t *cache;
    opal_event_base_t *evbase;
    char *uri;
    int sd;
    int retries;                  // number of times we have tried to connect to this address
    pmix_usock_state_t state;
    opal_event_t op_event;        // used for connecting and operations other than read/write
    opal_event_t send_event;      // registration with event thread for send events
    bool send_ev_active;
    opal_event_t recv_event;      // registration with event thread for recv events
    bool recv_ev_active;
    opal_event_t timer_event;     // timer for retrying connection failures
    bool timer_ev_active;
    opal_list_t send_queue;       // list of messages to send
    pmix_usock_msg_t *send_msg;   // current send in progress
    pmix_usock_msg_t *recv_msg;   // current recv in progress
} opal_pmix_native_component_t;

OPAL_DECLSPEC extern const opal_pmix_native_component_t mca_pmix_native_component;

OPAL_DECLSPEC extern const opal_pmix_base_module_t opal_pmix_native_module;

/* internally used objects */
typedef struct {
    opal_object_t super;
    opal_event_t ev;
    volatile bool active;
    opal_pmix_cbfunc_t cbfunc;
    void *cbata;
} pmix_cb_t;
OBJ_CLASS_DECLARATION(pmix_cb_t);

/* module-level shared functions */
ORTE_MODULE_DECLSPEC void pmix_usock_send_handler(int fd, short args, void *cbdata);
ORTE_MODULE_DECLSPEC void pmix_usock_recv_handler(int fd, short args, void *cbdata);

/* internal convenience macros */
#define PMIX_ACTIVATE_POST_SEND(b)                                      \
    do {                                                                \
        pmix_usock_msg_t *ms;                                           \
        opal_output_verbose(5, opal_pmix_base_framework.framework_output, \
                            "post send to server",                      \
                            __FILE__, __LINE__);                        \
        ms = OBJ_NEW(pmix_usock_msg_t);                                 \
        ms->data = (b)->bytes;                                          \
        ms->total_bytes = (b)->bytes_used;                              \
        opal_event_set(mca_pmix_native_component.ev_base, &((ms)->ev), -1, \
                       OPAL_EV_WRITE, pmix_usock_process_send, (ms));   \
        opal_event_set_priority(&((ms)->ev), ORTE_MSG_PRI);             \
        opal_event_active(&((ms)->ev), OPAL_EV_WRITE, 1);               \
    } while(0);



END_C_DECLS

#endif /* MCA_PMIX_NATIVE_H */
