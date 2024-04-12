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
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
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

#include "src/include/pmix_config.h"
#include "src/include/pmix_types.h"

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#    include <sys/un.h>
#endif
#ifdef HAVE_SYS_UIO_H
#    include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#    include <net/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#include <event.h>

#include "src/class/pmix_list.h"
#include "src/include/pmix_stdatomic.h"
#include "src/mca/bfrops/bfrops_types.h"
#include "src/mca/ptl/base/ptl_base_handshake.h"
#include "src/util/pmix_output.h"

BEGIN_C_DECLS

// forward declaration
struct pmix_peer_t;
struct pmix_ptl_module_t;

/* define a process type */
typedef struct {
    uint32_t type;
    uint8_t major;
    uint8_t minor;
    uint8_t release;
    uint8_t flag; // see pmix_ptl_base_set_flag for definition of values
} pmix_proc_type_t;

#define PMIX_MAJOR_WILDCARD   255
#define PMIX_MINOR_WILDCARD   255
#define PMIX_RELEASE_WILDCARD 255

/* use 255 as WILDCARD for the release triplet values */
#define PMIX_PROC_TYPE_STATIC_INIT          \
    {                                       \
        .type = PMIX_PROC_UNDEF,            \
        .major = PMIX_MAJOR_WILDCARD,       \
        .minor = PMIX_MINOR_WILDCARD,       \
        .release = PMIX_RELEASE_WILDCARD,   \
        .flag = 0                           \
    }

/* Define process types - we use a bit-mask as procs can
 * span multiple types */
#define PMIX_PROC_UNDEF           0x00000000
#define PMIX_PROC_CLIENT          0x00000001 // simple client process
#define PMIX_PROC_SERVER          0x00000002 // simple server process
#define PMIX_PROC_TOOL            0x00000004 // simple tool
#define PMIX_PROC_SINGLETON_ACT   0x00000008 // self-started client process
#define PMIX_PROC_SINGLETON       (PMIX_PROC_CLIENT | PMIX_PROC_SINGLETON_ACT)
#define PMIX_PROC_LAUNCHER_ACT    0x10000000 // process acting as launcher
#define PMIX_PROC_LAUNCHER        (PMIX_PROC_TOOL | PMIX_PROC_SERVER | PMIX_PROC_LAUNCHER_ACT)
#define PMIX_PROC_CLIENT_LAUNCHER (PMIX_PROC_LAUNCHER | PMIX_PROC_CLIENT)
#define PMIX_PROC_CLIENT_TOOL_ACT 0x20000000
#define PMIX_PROC_CLIENT_TOOL     (PMIX_PROC_TOOL | PMIX_PROC_CLIENT | PMIX_PROC_CLIENT_TOOL_ACT)
#define PMIX_PROC_GATEWAY_ACT     0x40000000
#define PMIX_PROC_GATEWAY         (PMIX_PROC_SERVER | PMIX_PROC_GATEWAY_ACT)
#define PMIX_PROC_SCHEDULER_ACT   0x80000000
#define PMIX_PROC_SCHEDULER       (PMIX_PROC_SERVER | PMIX_PROC_SCHEDULER_ACT)
#define PMIX_PROC_SYS_CTRLR_ACT   0x01000000
#define PMIX_PROC_SYS_CTRLR       (PMIX_PROC_SERVER | PMIX_PROC_SYS_CTRLR_ACT)


#define PMIX_SET_PEER_TYPE(a, b) (a)->proc_type.type |= (b)
#define PMIX_SET_PROC_TYPE(a, b) (a)->type |= (b)

/* define some convenience macros for testing proc type */
#define PMIX_PEER_IS_CLIENT(p)    (PMIX_PROC_CLIENT & (p)->proc_type.type)
#define PMIX_PEER_IS_SINGLETON(p) (PMIX_PROC_SINGLETON_ACT & (p)->proc_type.type)
#define PMIX_PEER_IS_SERVER(p)    (PMIX_PROC_SERVER & (p)->proc_type.type)
#define PMIX_PEER_IS_TOOL(p)      (PMIX_PROC_TOOL & (p)->proc_type.type)
#define PMIX_PEER_IS_LAUNCHER(p)  (PMIX_PROC_LAUNCHER_ACT & (p)->proc_type.type)
#define PMIX_PEER_IS_CLIENT_LAUNCHER(p) \
    ((PMIX_PROC_LAUNCHER_ACT & (p)->proc_type.type) && (PMIX_PROC_CLIENT & (p)->proc_type.type))
#define PMIX_PEER_IS_CLIENT_TOOL(p) \
    ((PMIX_PROC_CLIENT_TOOL_ACT & (p)->proc_type.type) && (PMIX_PROC_CLIENT & (p)->proc_type.type))
#define PMIX_PEER_IS_GATEWAY(p)   (PMIX_PROC_GATEWAY_ACT & (p)->proc_type.type)
#define PMIX_PEER_IS_SCHEDULER(p) (PMIX_PROC_SCHEDULER_ACT & (p)->proc_type.type)
#define PMIX_PEER_IS_SYS_CTRLR(p) (PMIX_PROC_SYS_CTRLR_ACT & (p)->proc_type.type)


#define PMIX_PROC_IS_CLIENT(p)   (PMIX_PROC_CLIENT & (p)->type)
#define PMIX_PROC_IS_SERVER(p)   (PMIX_PROC_SERVER & (p)->type)
#define PMIX_PROC_IS_TOOL(p)     (PMIX_PROC_TOOL & (p)->type)
#define PMIX_PROC_IS_LAUNCHER(p) (PMIX_PROC_LAUNCHER_ACT & (p)->type)
#define PMIX_PROC_IS_CLIENT_LAUNCHER(p) \
    ((PMIX_PROC_LAUNCHER_ACT & (p)->type) && (PMIX_PROC_CLIENT & (p)->type))
#define PMIX_PROC_IS_CLIENT_TOOL(p) \
    ((PMIX_PROC_CLIENT_TOOL_ACT & (p)->type) && (PMIX_PROC_CLIENT & (p)->type))
#define PMIX_PROC_IS_GATEWAY(p)   (PMIX_PROC_GATEWAY_ACT & (p)->type)
#define PMIX_PROC_IS_SCHEDULER(p) (PMIX_PROC_SCHEDULER_ACT & (p)->type)
#define PMIX_PROC_IS_SYS_CTRLR(p) (PMIX_PROC_SYS_CTRLR_ACT & (p)->type)


/* provide macros for setting the major, minor, and release values
 * just so people don't have to deal with the details of the struct */
#define PMIX_SET_PEER_VERSION(p, e, a, b)     \
    do {                                      \
        char *e2;                             \
        unsigned long mj, mn, rl;             \
        if (NULL != e) {                      \
            if ('v' == e[0]) {                \
                mj = strtoul(&e[1], &e2, 10); \
            } else {                          \
                mj = strtoul(e, &e2, 10);     \
            }                                 \
            ++e2;                             \
            mn = strtoul(e2, &e2, 10);        \
            ++e2;                             \
            rl = strtoul(e2, NULL, 10);       \
            PMIX_SET_PEER_MAJOR((p), mj);     \
            PMIX_SET_PEER_MINOR((p), mn);     \
            PMIX_SET_PEER_RELEASE((p), rl);   \
        } else {                              \
            PMIX_SET_PEER_MAJOR((p), (a));    \
            PMIX_SET_PEER_MINOR((p), (b));    \
        }                                     \
    } while (0)

#define PMIX_SET_PEER_MAJOR(p, a)   (p)->proc_type.major = (a)
#define PMIX_SET_PEER_MINOR(p, a)   (p)->proc_type.minor = (a)
#define PMIX_SET_PEER_RELEASE(p, a) (p)->proc_type.release = (a)
#define PMIX_SET_PROC_MAJOR(p, a)   (p)->major = (a)
#define PMIX_SET_PROC_MINOR(p, a)   (p)->minor = (a)
#define PMIX_SET_PROC_RELEASE(p, a) (p)->release = (a)

/* define some convenience macros for testing version */
#define PMIX_PEER_MAJOR_VERSION(p) (p)->proc_type.major
#define PMIX_PEER_MINOR_VERSION(p) (p)->proc_type.minor
#define PMIX_PEER_REL_VERSION(p)   (p)->proc_type.release
#define PMIX_PROC_MAJOR_VERSION(p) (p)->major
#define PMIX_PROC_MINOR_VERSION(p) (p)->minor
#define PMIX_PROC_REL_VERSION(p)   (p)->release
#define PMIX_PEER_IS_V1(p)         ((p)->proc_type.major == 1)
#define PMIX_PEER_IS_V20(p)        ((p)->proc_type.major == 2 && (p)->proc_type.minor == 0)
#define PMIX_PEER_IS_V21(p)        ((p)->proc_type.major == 2 && (p)->proc_type.minor == 1)
#define PMIX_PEER_IS_V3(p)         ((p)->proc_type.major == 3)
#define PMIX_PEER_IS_V40(p)        ((p)->proc_type.major == 4 && (p)->proc_type.minor == 0)
#define PMIX_PEER_IS_V41(p)        ((p)->proc_type.major == 4 && (p)->proc_type.minor == 1)

#define PMIX_PEER_TRIPLET(p, a, b, c)                                                      \
    ((PMIX_PEER_MAJOR_VERSION(p) == PMIX_MAJOR_WILDCARD || (a) == PMIX_MAJOR_WILDCARD      \
      || PMIX_PEER_MAJOR_VERSION(p) == (a))                                                \
     && (PMIX_PEER_MINOR_VERSION(p) == PMIX_MINOR_WILDCARD || (b) == PMIX_MINOR_WILDCARD   \
         || PMIX_PEER_MINOR_VERSION(p) == (b))                                             \
     && (PMIX_PEER_REL_VERSION(p) == PMIX_RELEASE_WILDCARD || (c) == PMIX_RELEASE_WILDCARD \
         || PMIX_PEER_REL_VERSION(p) == (c)))

#define PMIX_PROC_TRIPLET(p, a, b, c)                                                            \
    ((PMIX_PROC_MAJOR_VERSION(p) == PMIX_MAJOR_WILDCARD || PMIX_PROC_MAJOR_VERSION(p) == (a))    \
     && (PMIX_PROC_MINOR_VERSION(p) == PMIX_MINOR_WILDCARD || PMIX_PROC_MINOR_VERSION(p) == (b)) \
     && (PMIX_PROC_REL_VERSION(p) == PMIX_RELEASE_WILDCARD || PMIX_PROC_REL_VERSION(p) == (c)))

#define PMIX_PEER_IS_EARLIER(p, a, b, c) pmix_ptl_base_peer_is_earlier(p, a, b, c)

/****    MESSAGING STRUCTURES    ****/
typedef uint32_t pmix_ptl_tag_t;
/* define a range of "reserved" tags - these
 * are tags that are used for persistent recvs
 * within the system */
#define PMIX_PTL_TAG_NOTIFY    0
#define PMIX_PTL_TAG_HEARTBEAT 1
#define PMIX_PTL_TAG_IOF       2

/* define the start of dynamic tags that are
 * assigned for send/recv operations */
#define PMIX_PTL_TAG_DYNAMIC 100

/* header for messages */
typedef struct {
    int32_t pindex;
    pmix_ptl_tag_t tag;
    uint32_t nbytes;
#if SIZEOF_SIZE_T == 8
    uint32_t padding;
#endif
} pmix_ptl_hdr_t;

/* define the messaging cbfunc */
typedef void (*pmix_ptl_cbfunc_t)(struct pmix_peer_t *peer, pmix_ptl_hdr_t *hdr, pmix_buffer_t *buf,
                                  void *cbdata);

/* define a callback function for notifying that server connection
 * has completed */
typedef void (*pmix_ptl_connect_cbfunc_t)(pmix_status_t status, void *cbdata);

/* define a callback function for processing pending connections */
typedef void (*pmix_ptl_pending_cbfunc_t)(int sd, short args, void *cbdata);

/* structure for sending a message */
typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    pmix_ptl_hdr_t hdr;
    pmix_buffer_t *data;
    bool hdr_sent;
    char *sdptr;
    size_t sdbytes;
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
    volatile bool active;
    pmix_event_t ev;
    struct pmix_peer_t *peer;
    pmix_buffer_t *buf;
    pmix_ptl_tag_t tag;
} pmix_ptl_queue_t;
PMIX_CLASS_DECLARATION(pmix_ptl_queue_t);

/* define listener protocol types */
typedef uint16_t pmix_listener_protocol_t;
#define PMIX_PROTOCOL_UNDEF 0
#define PMIX_PROTOCOL_V1    1 // legacy usock
#define PMIX_PROTOCOL_V2    2 // tcp

/* connection support */
typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    pmix_listener_protocol_t protocol;
    int sd;
    bool need_id;
    pmix_rnd_flag_t flag;
    pmix_proc_t proc;
    pmix_info_t *info;
    size_t ninfo;
    pmix_status_t status;
    struct sockaddr_storage addr;
    struct pmix_peer_t *peer;
    char *version;
    char *bfrops;
    char *psec;
    char *gds;
    pmix_bfrop_buffer_type_t buffer_type;
    char *cred;
    size_t len;
    uid_t uid;
    gid_t gid;
    pmix_proc_type_t proc_type;
} pmix_pending_connection_t;
PMIX_CLASS_DECLARATION(pmix_pending_connection_t);

/* listener objects */
typedef struct pmix_listener_t {
    pmix_list_item_t super;
    pmix_event_t ev;
    pmix_atomic_bool_t active;
    pmix_listener_protocol_t protocol;
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

#define PMIX_LISTENER_STATIC_INIT           \
{                                           \
    .super = PMIX_LIST_ITEM_STATIC_INIT,    \
    .active = false,                        \
    .protocol = PMIX_PROTOCOL_UNDEF,        \
    .socket = 0,                            \
    .varname = NULL,                        \
    .uri = NULL,                            \
    .owner = 0,                             \
    .owner_given = false,                   \
    .group = 0,                             \
    .mode = 0,                              \
    .cbfunc = NULL                          \
}

/* provide a backdoor to the framework output for debugging */
PMIX_EXPORT extern int pmix_ptl_base_output;

#define PMIX_ACTIVATE_POST_MSG(ms)                                        \
    do {                                                                  \
        pmix_event_assign(&((ms)->ev), pmix_globals.evbase, -1, EV_WRITE, \
                          pmix_ptl_base_process_msg, (ms));               \
        PMIX_POST_OBJECT(ms);                                             \
        pmix_event_active(&((ms)->ev), EV_WRITE, 1);                      \
    } while (0)

#define PMIX_SND_CADDY(c, h, s)                                 \
    do {                                                        \
        (c) = PMIX_NEW(pmix_server_caddy_t);                    \
        (void) memcpy(&(c)->hdr, &(h), sizeof(pmix_ptl_hdr_t)); \
        PMIX_RETAIN((s));                                       \
        (c)->snd = (s);                                         \
    } while (0)

/* queue a message to be sent to one of our procs - must
 * provide the following params:
 * p - pmix_peer_t of target recipient
 * t - tag to be sent to
 * b - buffer to be sent
 */
#define PMIX_SERVER_QUEUE_REPLY(r, p, t, b)                                                     \
    do {                                                                                        \
        pmix_ptl_send_t *snd;                                                                   \
        uint32_t nbytes;                                                                        \
        pmix_output_verbose(5, pmix_ptl_base_output,                                            \
                            "[%s:%d] queue callback called: reply to %s:%d on tag %d size %d",  \
                            __FILE__, __LINE__, (p)->info->pname.nspace, (p)->info->pname.rank, \
                            (t), (int) (b)->bytes_used);                                        \
        if ((p)->finalized) {                                                                   \
            (r) = PMIX_ERR_UNREACH;                                                             \
        } else {                                                                                \
            snd = PMIX_NEW(pmix_ptl_send_t);                                                    \
            snd->hdr.pindex = htonl(pmix_globals.pindex);                                       \
            snd->hdr.tag = htonl(t);                                                            \
            nbytes = (b)->bytes_used;                                                           \
            snd->hdr.nbytes = htonl(nbytes);                                                    \
            snd->data = (b);                                                                    \
            /* always start with the header */                                                  \
            snd->sdptr = (char *) &snd->hdr;                                                    \
            snd->sdbytes = sizeof(pmix_ptl_hdr_t);                                              \
            /* if there is no message on-deck, put this one there */                            \
            if (NULL == (p)->send_msg) {                                                        \
                (p)->send_msg = snd;                                                            \
            } else {                                                                            \
                /* add it to the queue */                                                       \
                pmix_list_append(&(p)->send_queue, &snd->super);                                \
            }                                                                                   \
            /* ensure the send event is active */                                               \
            if (!(p)->send_ev_active && 0 <= (p)->sd) {                                         \
                (p)->send_ev_active = true;                                                     \
                PMIX_POST_OBJECT(snd);                                                          \
                pmix_event_add(&(p)->send_event, 0);                                            \
            }                                                                                   \
            (r) = PMIX_SUCCESS;                                                                 \
        }                                                                                       \
    } while (0)

#define CLOSE_THE_SOCKET(s)   \
    do {                      \
        if (0 <= (s)) {       \
            shutdown((s), 2); \
            close((s));       \
            (s) = -1;         \
        }                     \
    } while (0)

END_C_DECLS

#endif /* PMIX_PTL_TYPES_H */
