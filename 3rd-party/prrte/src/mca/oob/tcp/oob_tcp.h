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
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _MCA_OOB_TCP_H_
#define _MCA_OOB_TCP_H_

#include "prte_config.h"

#include "types.h"

#include "src/event/event-internal.h"
#include "src/mca/base/pmix_base.h"

#include "src/mca/oob/base/base.h"
#include "src/mca/oob/oob.h"

BEGIN_C_DECLS

/* define some debug levels */
#define OOB_TCP_DEBUG_FAIL    2
#define OOB_TCP_DEBUG_CONNECT 7

/* forward declare a couple of structures */
struct prte_oob_tcp_module_t;
struct prte_oob_tcp_msg_error_t;

/* define a struct for tracking NIC addresses */
typedef struct {
    pmix_list_item_t super;
    uint16_t af_family;
    struct sockaddr addr;
} prte_oob_tcp_nicaddr_t;
PMIX_CLASS_DECLARATION(prte_oob_tcp_nicaddr_t);

/* Module definition */
typedef void (*prte_oob_tcp_module_accept_connection_fn_t)(const int accepted_fd,
                                                           const struct sockaddr *addr);
typedef void (*prte_oob_tcp_module_ping_fn_t)(const pmix_proc_t *proc);
typedef void (*prte_oob_tcp_module_send_nb_fn_t)(prte_rml_send_t *msg);

typedef struct {
    prte_oob_tcp_module_accept_connection_fn_t accept_connection;
    prte_oob_tcp_module_ping_fn_t ping;
    prte_oob_tcp_module_send_nb_fn_t send_nb;
} prte_oob_tcp_module_t;
PRTE_MODULE_EXPORT extern prte_oob_tcp_module_t prte_oob_tcp_module;

/**
 * the state of the connection
 */
typedef enum {
    MCA_OOB_TCP_UNCONNECTED,
    MCA_OOB_TCP_CLOSED,
    MCA_OOB_TCP_RESOLVE,
    MCA_OOB_TCP_CONNECTING,
    MCA_OOB_TCP_CONNECT_ACK,
    MCA_OOB_TCP_CONNECTED,
    MCA_OOB_TCP_FAILED,
    MCA_OOB_TCP_ACCEPTING
} prte_oob_tcp_state_t;

/* module-level shared functions */
PRTE_MODULE_EXPORT void prte_oob_tcp_send_handler(int fd, short args, void *cbdata);
PRTE_MODULE_EXPORT void prte_oob_tcp_recv_handler(int fd, short args, void *cbdata);
PRTE_MODULE_EXPORT void prte_oob_tcp_queue_msg(int sd, short args, void *cbdata);

END_C_DECLS

#endif /* MCA_OOB_TCP_H_ */
