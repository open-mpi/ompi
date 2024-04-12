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

#ifndef _MCA_OOB_TCP_CONNECTION_H_
#define _MCA_OOB_TCP_CONNECTION_H_

#include "prte_config.h"

#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#endif

#include "oob_tcp.h"
#include "oob_tcp_peer.h"
#include "src/threads/pmix_threads.h"

/* State machine for connection operations */
typedef struct {
    pmix_object_t super;
    prte_oob_tcp_peer_t *peer;
    prte_event_t ev;
} prte_oob_tcp_conn_op_t;
PMIX_CLASS_DECLARATION(prte_oob_tcp_conn_op_t);

#define CLOSE_THE_SOCKET(socket) \
    do {                         \
        shutdown(socket, 2);     \
        close(socket);           \
    } while (0)

#define PRTE_ACTIVATE_TCP_CONN_STATE(p, cbfunc)                                             \
    do {                                                                                    \
        prte_oob_tcp_conn_op_t *cop;                                                        \
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,                    \
                            "%s:[%s:%d] connect to %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), \
                            __FILE__, __LINE__, PRTE_NAME_PRINT((&(p)->name)));             \
        cop = PMIX_NEW(prte_oob_tcp_conn_op_t);                                             \
        cop->peer = (p);                                                                    \
        PRTE_PMIX_THREADSHIFT(cop, prte_event_base, (cbfunc));                              \
    } while (0);

#define PRTE_ACTIVATE_TCP_ACCEPT_STATE(s, a, cbfunc)                               \
    do {                                                                           \
        prte_oob_tcp_conn_op_t *cop;                                               \
        cop = PMIX_NEW(prte_oob_tcp_conn_op_t);                                    \
        prte_event_set(prte_event_base, &cop->ev, s, PRTE_EV_READ, (cbfunc), cop); \
        PMIX_POST_OBJECT(cop);                                                     \
        prte_event_add(&cop->ev, 0);                                               \
    } while (0);

#define PRTE_RETRY_TCP_CONN_STATE(p, cbfunc, tv)                                                  \
    do {                                                                                          \
        prte_oob_tcp_conn_op_t *cop;                                                              \
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,                          \
                            "%s:[%s:%d] retry connect to %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), \
                            __FILE__, __LINE__, PRTE_NAME_PRINT((&(p)->name)));                   \
        cop = PMIX_NEW(prte_oob_tcp_conn_op_t);                                                   \
        cop->peer = (p);                                                                          \
        prte_event_evtimer_set(prte_event_base, &cop->ev, (cbfunc), cop);                         \
        PMIX_POST_OBJECT(cop);                                                                    \
        prte_event_evtimer_add(&cop->ev, (tv));                                                   \
    } while (0);

PRTE_MODULE_EXPORT void prte_oob_tcp_peer_try_connect(int fd, short args, void *cbdata);
PRTE_MODULE_EXPORT void prte_oob_tcp_peer_dump(prte_oob_tcp_peer_t *peer, const char *msg);
PRTE_MODULE_EXPORT bool prte_oob_tcp_peer_accept(prte_oob_tcp_peer_t *peer);
PRTE_MODULE_EXPORT void prte_oob_tcp_peer_complete_connect(prte_oob_tcp_peer_t *peer);
PRTE_MODULE_EXPORT int prte_oob_tcp_peer_recv_connect_ack(prte_oob_tcp_peer_t *peer, int sd,
                                                          prte_oob_tcp_hdr_t *dhdr);
PRTE_MODULE_EXPORT void prte_oob_tcp_peer_close(prte_oob_tcp_peer_t *peer);

#endif /* _MCA_OOB_TCP_CONNECTION_H_ */
