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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef _MCA_OOB_USOCK_PEER_H_
#define _MCA_OOB_USOCK_PEER_H_

#include "orte_config.h"

#include "oob_usock.h"
#include "oob_usock_sendrecv.h"

/* object for tracking peers */
typedef struct {
    opal_list_item_t super;
    /* although not required, there is enough debug
     * value that retaining the name makes sense
     */
    orte_process_name_t name;
    char *auth_method;     // how the peer authenticated themselves to use
    int sd;
    int retries;                  // number of times we have tried to connect to this address
    mca_oob_usock_state_t state;
    opal_event_t op_event;      // used for connecting and operations other than read/write
    opal_event_t send_event;    /**< registration with event thread for send events */
    bool send_ev_active;
    opal_event_t recv_event;    /**< registration with event thread for recv events */
    bool recv_ev_active;
    opal_event_t timer_event;   /**< timer for retrying connection failures */
    bool timer_ev_active;
    opal_list_t send_queue;      /**< list of messages to send */
    mca_oob_usock_send_t *send_msg; /**< current send in progress */
    mca_oob_usock_recv_t *recv_msg; /**< current recv in progress */
} mca_oob_usock_peer_t;
OBJ_CLASS_DECLARATION(mca_oob_usock_peer_t);

typedef struct {
    opal_object_t super;
    opal_event_t ev;
    mca_oob_usock_peer_t *peer;
} mca_oob_usock_peer_op_t;
OBJ_CLASS_DECLARATION(mca_oob_usock_peer_op_t);

#define ORTE_ACTIVATE_USOCK_PEER_OP(p, cbfunc)                          \
    do {                                                                \
        mca_oob_usock_peer_op_t *op;                                    \
        op = OBJ_NEW(mca_oob_usock_peer_op_t);                          \
        op->peer = (p);                                                 \
        opal_event_set(mca_usock_component.ev_base, &op->ev, -1,        \
                       OPAL_EV_WRITE, (cbfunc), op);                    \
        opal_event_set_priority(&op->ev, ORTE_MSG_PRI);                 \
        opal_event_active(&op->ev, OPAL_EV_WRITE, 1);                   \
    } while(0);
    
#define ORTE_ACTIVATE_USOCK_CMP_OP(p, cbfunc)                   \
    do {                                                        \
        mca_oob_usock_peer_op_t *pop;                           \
        pop = OBJ_NEW(mca_oob_usock_peer_op_t);                 \
        pop->peer = (p);                                        \
        opal_event_set(orte_event_base, &pop->ev, -1,           \
                       OPAL_EV_WRITE, (cbfunc), pop);           \
        opal_event_set_priority(&pop->ev, ORTE_MSG_PRI);        \
        opal_event_active(&pop->ev, OPAL_EV_WRITE, 1);          \
    } while(0);


#endif /* _MCA_OOB_USOCK_PEER_H_ */
