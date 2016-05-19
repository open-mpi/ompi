/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef USOCK_H
#define USOCK_H

#include <src/include/pmix_config.h>

#include <src/include/types.h>
#include <pmix/pmix_common.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include PMIX_EVENT_HEADER

#include "src/include/pmix_globals.h"
#include "src/buffer_ops/buffer_ops.h"
#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_list.h"

/* usock structure for tracking posted recvs */
typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    uint32_t tag;
    pmix_usock_cbfunc_t cbfunc;
    void *cbdata;
} pmix_usock_posted_recv_t;
PMIX_CLASS_DECLARATION(pmix_usock_posted_recv_t);

/* usock struct for posting send/recv request */
typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    pmix_peer_t *peer;
    pmix_buffer_t *bfr;
    pmix_usock_cbfunc_t cbfunc;
    void *cbdata;
} pmix_usock_sr_t;
PMIX_CLASS_DECLARATION(pmix_usock_sr_t);

/* usock struct for tracking ops */
typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    volatile bool active;
    bool checked;
    int status;
    pmix_status_t pstatus;
    pmix_scope_t scope;
    pmix_buffer_t data;
    pmix_usock_cbfunc_t cbfunc;
    pmix_op_cbfunc_t op_cbfunc;
    pmix_value_cbfunc_t value_cbfunc;
    pmix_lookup_cbfunc_t lookup_cbfunc;
    pmix_spawn_cbfunc_t spawn_cbfunc;
    pmix_errhandler_reg_cbfunc_t errreg_cbfunc;
    int errhandler_ref;
    void *cbdata;
    char nspace[PMIX_MAX_NSLEN+1];
    int rank;
    char *key;
    pmix_value_t *value;
    pmix_proc_t *procs;
    pmix_info_t *info;
    size_t ninfo;
    size_t nvals;
} pmix_cb_t;
PMIX_CLASS_DECLARATION(pmix_cb_t);

typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    void *cbdata;
} pmix_timer_t;
PMIX_CLASS_DECLARATION(pmix_timer_t);

/* internal convenience macros */
#define PMIX_ACTIVATE_SEND_RECV(p, b, cb, d)                            \
    do {                                                                \
        int rc = -1;                                                    \
        pmix_usock_sr_t *ms;                                            \
        pmix_output_verbose(5, pmix_globals.debug_output,               \
                            "[%s:%d] post send to server",              \
                            __FILE__, __LINE__);                        \
        ms = PMIX_NEW(pmix_usock_sr_t);                                  \
        ms->peer = (p);                                                 \
        ms->bfr = (b);                                                  \
        ms->cbfunc = (cb);                                              \
        ms->cbdata = (d);                                               \
        rc = event_assign(&((ms)->ev), pmix_globals.evbase, -1,         \
                          EV_WRITE, pmix_usock_send_recv, (ms));        \
        pmix_output_verbose(10, pmix_globals.debug_output,              \
                            "event_assign returned %d", rc);            \
        event_active(&((ms)->ev), EV_WRITE, 1);                         \
    } while (0)

#define PMIX_ACTIVATE_POST_MSG(ms)                                      \
    do {                                                                \
        pmix_output_verbose(5, pmix_globals.debug_output,               \
                            "[%s:%d] post msg",                         \
                            __FILE__, __LINE__);                        \
        event_assign(&((ms)->ev), pmix_globals.evbase, -1,              \
                     EV_WRITE, pmix_usock_process_msg, (ms));           \
        event_active(&((ms)->ev), EV_WRITE, 1);                         \
    } while (0)

#define CLOSE_THE_SOCKET(socket)                \
    do {                                        \
        if (0 <= socket) {                      \
            shutdown(socket, 2);                \
            close(socket);                      \
            socket = -1;                        \
        }                                       \
    } while (0)


#define PMIX_TIMER_EVENT(s, f, d)                                       \
    do {                                                                \
        pmix_timer_t *tm;                                               \
        struct timeval tv;                                              \
        tm = PMIX_NEW(pmix_timer_t);                                     \
        tm->cbdata = (d);                                               \
        event_assign(&tm->ev, pmix_globals.evbase, -1, 0, (f), tm);     \
        tv.tv_sec = (s);                                                \
        tv.tv_usec = 0;                                                 \
        PMIX_OUTPUT_VERBOSE((1, pmix_globals.debug_output,              \
                             "defining timer event: %ld sec %ld usec at %s:%d", \
                             (long)tv.tv_sec, (long)tv.tv_usec,         \
                             __FILE__, __LINE__));                      \
        event_add(&tm->ev, &tv);                                        \
    } while (0)


/* usock common variables */
typedef struct {
    pmix_list_t posted_recvs;     // list of pmix_usock_posted_recv_t
} pmix_usock_globals_t;
extern pmix_usock_globals_t pmix_usock_globals;

/* usock common functions */
void pmix_usock_init(pmix_usock_cbfunc_t cbfunc);
void pmix_usock_finalize(void);
pmix_status_t pmix_usock_set_nonblocking(int sd);
pmix_status_t  pmix_usock_set_blocking(int sd);
pmix_status_t pmix_usock_send_blocking(int sd, char *ptr, size_t size);
pmix_status_t pmix_usock_recv_blocking(int sd, char *data, size_t size);
void pmix_usock_send_recv(int sd, short args, void *cbdata);
void pmix_usock_send_handler(int sd, short flags, void *cbdata);
void pmix_usock_recv_handler(int sd, short flags, void *cbdata);
void pmix_usock_process_msg(int fd, short flags, void *cbdata);

#endif // USOCK_H
