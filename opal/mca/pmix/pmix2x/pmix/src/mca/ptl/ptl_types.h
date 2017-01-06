/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Buffer management types.
 */

#ifndef PMIX_PTL_TYPES_H_
#define PMIX_PTL_TYPES_H_

#include <src/include/pmix_config.h>
#include "src/include/types.h"

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

#include "src/class/pmix_list.h"
#include "src/util/output.h"
#include "src/buffer_ops/types.h"

BEGIN_C_DECLS

// forward declaration
struct pmix_peer_t;
struct pmix_ptl_module_t;

/****    MESSAGING STRUCTURES    ****/
typedef uint32_t pmix_ptl_tag_t;

/* header for messages */
typedef struct {
    pmix_ptl_tag_t tag;
    size_t nbytes;
} pmix_ptl_hdr_t;

/* define the messaging cbfunc */
typedef void (*pmix_ptl_cbfunc_t)(struct pmix_peer_t *peer,
                                  pmix_ptl_hdr_t *hdr,
                                  pmix_buffer_t *buf, void *cbdata);

/* define a callback function for notifying that server connection
 * has completed */
typedef void (*pmix_ptl_connect_cbfunc_t)(pmix_status_t status, void *cbdata);

/* define a callback function for processing pending connections */
typedef void (*pmix_ptl_pending_cbfunc_t)(int sd, short args, void *cbdata);


/* structure for sending a message */
typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    pmix_buffer_t *data;
    pmix_ptl_hdr_t hdr;
    struct iovec iov[2];
    struct iovec *iov_ptr;
    size_t iov_cnt;
    size_t iov_idx;
    size_t size;
} pmix_ptl_send_t;
PMIX_CLASS_DECLARATION(pmix_ptl_send_t);

/* structure for recving a message */
typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    struct pmix_peer_t *peer;
    int sd;
    pmix_ptl_hdr_t hdr;
    char *data;
    bool hdr_recvd;
    char *rdptr;
    size_t rdbytes;
} pmix_ptl_recv_t;
PMIX_CLASS_DECLARATION(pmix_ptl_recv_t);

/* structure for tracking posted recvs */
typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    uint32_t tag;
    pmix_ptl_cbfunc_t cbfunc;
    void *cbdata;
} pmix_ptl_posted_recv_t;
PMIX_CLASS_DECLARATION(pmix_ptl_posted_recv_t);

/* struct for posting send/recv request */
typedef struct {
    pmix_object_t super;
    volatile bool active;
    pmix_event_t ev;
    struct pmix_peer_t *peer;
    pmix_status_t status;
    pmix_buffer_t *bfr;
    pmix_ptl_cbfunc_t cbfunc;
    void *cbdata;
} pmix_ptl_sr_t;
PMIX_CLASS_DECLARATION(pmix_ptl_sr_t);

typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    struct pmix_peer_t *peer;
    pmix_buffer_t *buf;
    pmix_ptl_tag_t tag;
} pmix_ptl_queue_t;
PMIX_CLASS_DECLARATION(pmix_ptl_queue_t);

/* define listener protocol types */
typedef uint16_t pmix_listener_protocol_t;
#define PMIX_PROTOCOL_V1        0       // legacy usock
#define PMIX_PROTOCOL_V2        1       // tcp
#define PMIX_PROTOCOL_V3        2       // updated usock

/* connection support */
typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    pmix_listener_protocol_t protocol;
    int sd;
    char nspace[PMIX_MAX_NSLEN+1];
    pmix_info_t *info;
    size_t ninfo;
    pmix_status_t status;
    struct sockaddr_storage addr;
    char *bfrop;
    char *psec;
    struct pmix_ptl_module_t *ptl;
    pmix_bfrop_buffer_type_t buffer_type;
    char *cred;
    size_t len;
    uid_t uid;
    gid_t gid;
} pmix_pending_connection_t;
PMIX_CLASS_DECLARATION(pmix_pending_connection_t);

/* listener objects */
typedef struct pmix_listener_t {
    pmix_list_item_t super;
    pmix_listener_protocol_t protocol;
    struct pmix_ptl_module_t *ptl;
    int socket;
    char *varname;
    char *uri;
    uint32_t owner;
    bool owner_given;
    uint32_t group;
    bool group_given;
    uint32_t mode;
    pmix_ptl_pending_cbfunc_t cbfunc;
} pmix_listener_t;
PMIX_CLASS_DECLARATION(pmix_listener_t);


#define PMIX_ACTIVATE_POST_MSG(ms)                                      \
    do {                                                                \
        pmix_output_verbose(5, pmix_globals.debug_output,               \
                            "[%s:%d] post msg",                         \
                            __FILE__, __LINE__);                        \
        event_assign(&((ms)->ev), pmix_globals.evbase, -1,              \
                     EV_WRITE, pmix_ptl_base_process_msg, (ms));        \
        event_active(&((ms)->ev), EV_WRITE, 1);                         \
    } while (0)

#define PMIX_SND_CADDY(c, h, s)                                         \
    do {                                                                \
        (c) = PMIX_NEW(pmix_server_caddy_t);                            \
        (void)memcpy(&(c)->hdr, &(h), sizeof(pmix_ptl_hdr_t));          \
        PMIX_RETAIN((s));                                               \
        (c)->snd = (s);                                                 \
    } while (0)

/* queue a message to be sent a peer - must
 * provide the following params:
 * p - pmix_peer_t of target recipient
 * t - tag to be sent to
 * b - buffer to be sent
 */
#define PMIX_PTL_SEND(p, t, b)                                                          \
    do {                                                                                \
        pmix_ptl_send_t *snd;                                                           \
        pmix_output_verbose(5, pmix_globals.debug_output,                               \
                            "[%s:%d] send to %s:%d on tag %d size %d",                  \
                            __FILE__, __LINE__,                                         \
                            (p)->info->nptr->nspace,                                    \
                            (p)->info->rank, (t), (int)(b)->bytes_used);                \
        snd = PMIX_NEW(pmix_ptl_send_t);                                                \
        snd->hdr.tag = htonl(t);                                                        \
        snd->hdr.nbytes = htonl((b)->bytes_used);                                       \
        snd->data = (b);                                                                \
        snd->iov[0].iov_base = (IOVBASE_TYPE*)&snd->hdr;                                \
        snd->iov[0].iov_len = sizeof(snd->hdr);                                         \
        snd->iov[1].iov_base = (IOVBASE_TYPE*)(b)->base_ptr;                            \
        snd->iov[1].iov_len = (b)->bytes_used;                                          \
        /* if there is no message on-deck, put this one there */                        \
        if (NULL == (p)->send_msg) {                                                    \
            (p)->send_msg = snd;                                                        \
        } else {                                                                        \
            /* add it to the queue */                                                   \
            pmix_list_append(&(p)->send_queue, &snd->super);                            \
        }                                                                               \
        /* ensure the send event is active */                                           \
        if (!(p)->send_ev_active && 0 <= (p)->sd) {                                     \
            event_add(&(p)->send_event, 0);                                             \
            (p)->send_ev_active = true;                                                 \
        }                                                                               \
    } while (0)


#define CLOSE_THE_SOCKET(socket)                \
    do {                                        \
        if (0 <= socket) {                      \
            shutdown(socket, 2);                \
            close(socket);                      \
            socket = -1;                        \
        }                                       \
    } while (0)


END_C_DECLS

#endif /* PMIX_PTL_TYPES_H */
