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
 * Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _MCA_OOB_TCP_PEER_H_
#define _MCA_OOB_TCP_PEER_H_

#include "prte_config.h"

#include "src/event/event-internal.h"

#include "oob_tcp.h"
#include "oob_tcp_sendrecv.h"
#include "src/threads/pmix_threads.h"

typedef struct {
    pmix_list_item_t super;
    struct sockaddr_storage addr; // an address where a peer can be found
    int retries;                  // number of times we have tried to connect to this address
    prte_oob_tcp_state_t state;   // state of this address
    int if_mask;                  // if mask of this address
} prte_oob_tcp_addr_t;
PMIX_CLASS_DECLARATION(prte_oob_tcp_addr_t);

/* object for tracking peers in the module */
typedef struct {
    pmix_list_item_t super;
    /* although not required, there is enough debug
     * value that retaining the name makes sense
     */
    pmix_proc_t name;
    char *auth_method; // method they used to authenticate
    int sd;
    pmix_list_t addrs;
    prte_oob_tcp_addr_t *active_addr;
    prte_oob_tcp_state_t state;
    int num_retries;
    prte_event_t send_event; /**< registration with event thread for send events */
    bool send_ev_active;
    prte_event_t recv_event; /**< registration with event thread for recv events */
    bool recv_ev_active;
    prte_event_t timer_event; /**< timer for retrying connection failures */
    bool timer_ev_active;
    pmix_list_t send_queue;        /**< list of messages to send */
    prte_oob_tcp_send_t *send_msg; /**< current send in progress */
    prte_oob_tcp_recv_t *recv_msg; /**< current recv in progress */
} prte_oob_tcp_peer_t;
PMIX_CLASS_DECLARATION(prte_oob_tcp_peer_t);

/* state machine for processing peer data */
typedef struct {
    pmix_object_t super;
    prte_event_t ev;
    pmix_proc_t peer;
    uint16_t af_family;
    char *net;
    char *port;
} prte_oob_tcp_peer_op_t;
PMIX_CLASS_DECLARATION(prte_oob_tcp_peer_op_t);

#define PRTE_ACTIVATE_TCP_CMP_OP(p, cbfunc)                     \
    do {                                                        \
        prte_oob_tcp_peer_op_t *pop;                            \
        pop = PMIX_NEW(prte_oob_tcp_peer_op_t);                 \
        PMIX_XFER_PROCID(&pop->peer, &(p)->name);               \
        PRTE_PMIX_THREADSHIFT(pop, prte_event_base, (cbfunc));  \
    } while (0);

#endif /* _MCA_OOB_TCP_PEER_H_ */
