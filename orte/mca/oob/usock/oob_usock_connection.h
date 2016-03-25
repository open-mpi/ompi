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
 * Copyright (c) 2013      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _MCA_OOB_USOCK_CONNECTION_H_
#define _MCA_OOB_USOCK_CONNECTION_H_

#include "orte_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif

#include "oob_usock.h"
#include "oob_usock_peer.h"

/* State machine for connection operations */
typedef struct {
    opal_object_t super;
    mca_oob_usock_peer_t *peer;
    opal_event_t ev;
} mca_oob_usock_conn_op_t;
OBJ_CLASS_DECLARATION(mca_oob_usock_conn_op_t);

#define CLOSE_THE_SOCKET(socket)    \
    do {                            \
        shutdown(socket, 2);        \
        close(socket);              \
    } while(0)

#define ORTE_ACTIVATE_USOCK_CONN_STATE(p, cbfunc)                       \
    do {                                                                \
        mca_oob_usock_conn_op_t *cop;                                   \
        opal_output_verbose(5, orte_oob_base_framework.framework_output, \
                            "%s:[%s:%d] connect to %s",                 \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                            __FILE__, __LINE__,                         \
                            ORTE_NAME_PRINT((&(p)->name)));             \
        cop = OBJ_NEW(mca_oob_usock_conn_op_t);                         \
        cop->peer = (p);                                                \
        opal_event_set(mca_oob_usock_module.ev_base, &cop->ev, -1,      \
                       OPAL_EV_WRITE, (cbfunc), cop);                   \
        opal_event_set_priority(&cop->ev, ORTE_MSG_PRI);                \
        opal_event_active(&cop->ev, OPAL_EV_WRITE, 1);                  \
    } while(0);

#define ORTE_ACTIVATE_USOCK_ACCEPT_STATE(s, a, cbfunc)                  \
    do {                                                                \
        mca_oob_usock_conn_op_t *cop;                                   \
        cop = OBJ_NEW(mca_oob_usock_conn_op_t);                         \
        opal_event_set(mca_oob_usock_module.ev_base, &cop->ev, s,       \
                       OPAL_EV_READ, (cbfunc), cop);                    \
        opal_event_set_priority(&cop->ev, ORTE_MSG_PRI);                \
        opal_event_add(&cop->ev, 0);                                    \
    } while(0);

#define ORTE_RETRY_USOCK_CONN_STATE(p, cbfunc, tv)                      \
    do {                                                                \
        mca_oob_usock_conn_op_t *cop;                                   \
        opal_output_verbose(5, orte_oob_base_framework.framework_output, \
                            "%s:[%s:%d] retry connect to %s",           \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                            __FILE__, __LINE__,                         \
                            ORTE_NAME_PRINT((&(p)->name)));             \
        cop = OBJ_NEW(mca_oob_usock_conn_op_t);                         \
        cop->peer = (p);                                                \
        opal_event_evtimer_set(mca_oob_usock_module.ev_base,            \
                               &cop->ev,                                \
                               (cbfunc), cop);                          \
        opal_event_evtimer_add(&cop->ev, (tv));                         \
    } while(0);

ORTE_MODULE_DECLSPEC void mca_oob_usock_peer_try_connect(int fd, short args, void *cbdata);
ORTE_MODULE_DECLSPEC void mca_oob_usock_peer_dump(mca_oob_usock_peer_t* peer, const char* msg);
ORTE_MODULE_DECLSPEC bool mca_oob_usock_peer_accept(mca_oob_usock_peer_t* peer);
ORTE_MODULE_DECLSPEC void mca_oob_usock_peer_complete_connect(mca_oob_usock_peer_t* peer);
ORTE_MODULE_DECLSPEC int mca_oob_usock_peer_recv_connect_ack(mca_oob_usock_peer_t* peer,
                                                             int sd, mca_oob_usock_hdr_t *hdr);
ORTE_MODULE_DECLSPEC void mca_oob_usock_peer_close(mca_oob_usock_peer_t *peer);

#endif /* _MCA_OOB_USOCK_CONNECTION_H_ */
