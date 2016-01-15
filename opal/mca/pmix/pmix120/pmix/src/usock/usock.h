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
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef USOCK_H
#define USOCK_H

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>
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

#include "src/buffer_ops/buffer_ops.h"
#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_list.h"

/* define a command type for communicating to the
 * pmix server */
#define PMIX_CMD PMIX_UINT32

/* define some commands */
typedef enum {
    PMIX_REQ_CMD,
    PMIX_ABORT_CMD,
    PMIX_COMMIT_CMD,
    PMIX_FENCENB_CMD,
    PMIX_GETNB_CMD,
    PMIX_FINALIZE_CMD,
    PMIX_PUBLISHNB_CMD,
    PMIX_LOOKUPNB_CMD,
    PMIX_UNPUBLISHNB_CMD,
    PMIX_SPAWNNB_CMD,
    PMIX_CONNECTNB_CMD,
    PMIX_DISCONNECTNB_CMD,
    PMIX_NOTIFY_CMD,
    PMIX_REGEVENTS_CMD,
    PMIX_DEREGEVENTS_CMD,
} pmix_cmd_t;


/* header for messages */
typedef struct {
    int pindex;
    uint32_t tag;
    size_t nbytes;
} pmix_usock_hdr_t;

// forward declaration
struct pmix_peer_t;

/* internally used cbfunc */
typedef void (*pmix_usock_cbfunc_t)(struct pmix_peer_t *peer, pmix_usock_hdr_t *hdr,
                                    pmix_buffer_t *buf, void *cbdata);

/* usock structure for sending a message */
typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    pmix_usock_hdr_t hdr;
    pmix_buffer_t *data;
    bool hdr_sent;
    char *sdptr;
    size_t sdbytes;
} pmix_usock_send_t;
PMIX_CLASS_DECLARATION(pmix_usock_send_t);

/* usock structure for recving a message */
typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    struct pmix_peer_t *peer;
    int sd;
    pmix_usock_hdr_t hdr;
    char *data;
    bool hdr_recvd;
    char *rdptr;
    size_t rdbytes;
} pmix_usock_recv_t;
PMIX_CLASS_DECLARATION(pmix_usock_recv_t);

/* usock structure for tracking posted recvs */
typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    uint32_t tag;
    pmix_usock_cbfunc_t cbfunc;
    void *cbdata;
} pmix_usock_posted_recv_t;
PMIX_CLASS_DECLARATION(pmix_usock_posted_recv_t);

/* object for tracking peers - each peer can have multiple
 * connections. This can occur if the initial app executes
 * a fork/exec, and the child initiates its own connection
 * back to the PMIx server. Thus, the trackers should be "indexed"
 * by the socket, not the process nspace/rank */
typedef struct pmix_peer_t {
    pmix_object_t super;
    pmix_rank_info_t *info;
    int proc_cnt;
    void *server_object;
    int index;
    int sd;
    pmix_event_t send_event;    /**< registration with event thread for send events */
    bool send_ev_active;
    pmix_event_t recv_event;    /**< registration with event thread for recv events */
    bool recv_ev_active;
    pmix_list_t send_queue;      /**< list of messages to send */
    pmix_usock_send_t *send_msg; /**< current send in progress */
    pmix_usock_recv_t *recv_msg; /**< current recv in progress */
} pmix_peer_t;
PMIX_CLASS_DECLARATION(pmix_peer_t);

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

/* an internal macro for shifting incoming requests
 * to the internal event thread */
#define PMIX_THREAD_SHIFT(c, f)                             \
    do {                                                    \
       event_assign(&((c)->ev), pmix_globals.evbase, -1,    \
                          EV_WRITE, (f), (c));              \
        event_active(&((c)->ev), EV_WRITE, 1);              \
    } while(0);

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
    } while(0);

#define PMIX_ACTIVATE_POST_MSG(ms)                                      \
    do {                                                                \
        pmix_output_verbose(5, pmix_globals.debug_output,               \
                            "[%s:%d] post msg",                         \
                            __FILE__, __LINE__);                        \
        event_assign(&((ms)->ev), pmix_globals.evbase, -1,              \
                     EV_WRITE, pmix_usock_process_msg, (ms));           \
        event_active(&((ms)->ev), EV_WRITE, 1);                         \
    } while(0);

#define CLOSE_THE_SOCKET(socket)                \
    do {                                        \
        if (0 <= socket) {                      \
            shutdown(socket, 2);                \
            close(socket);                      \
            socket = -1;                        \
        }                                       \
    } while(0)


#define PMIX_WAIT_FOR_COMPLETION(a)             \
    do {                                        \
        while ((a)) {                           \
            usleep(10);                         \
        }                                       \
    } while (0);

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
    }while(0);                                                          \


/* usock common variables */
typedef struct {
    pmix_list_t posted_recvs;     // list of pmix_usock_posted_recv_t
} pmix_usock_globals_t;
extern pmix_usock_globals_t pmix_usock_globals;

/* usock common functions */
void pmix_usock_init(pmix_usock_cbfunc_t cbfunc);
void pmix_usock_finalize(void);
int pmix_usock_set_nonblocking(int sd);
int pmix_usock_set_blocking(int sd);
pmix_status_t pmix_usock_send_blocking(int sd, char *ptr, size_t size);
pmix_status_t pmix_usock_recv_blocking(int sd, char *data, size_t size);
void pmix_usock_send_recv(int sd, short args, void *cbdata);
void pmix_usock_send_handler(int sd, short flags, void *cbdata);
void pmix_usock_recv_handler(int sd, short flags, void *cbdata);
void pmix_usock_process_msg(int fd, short flags, void *cbdata);

#endif // USOCK_H
