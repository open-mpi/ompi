/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2016 Intel, Inc. All rights reserved
 * Copyright (c) 2015      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 */

#ifndef PMIX_SERVER_OPS_H
#define PMIX_SERVER_OPS_H

#include <src/include/pmix_config.h>

#include <pmix_common.h>
#include <src/class/pmix_ring_buffer.h>
#include <pmix_server.h>
#include "src/usock/usock.h"
#include "src/util/hash.h"

typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    pmix_server_trkr_t *trk;
} pmix_trkr_caddy_t;
PMIX_CLASS_DECLARATION(pmix_trkr_caddy_t);

typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    volatile bool active;
    pmix_status_t status;
    pmix_proc_t proc;
    uid_t uid;
    gid_t gid;
    void *server_object;
    int nlocalprocs;
    pmix_info_t *info;
    size_t ninfo;
    pmix_op_cbfunc_t opcbfunc;
    pmix_dmodex_response_fn_t cbfunc;
    void *cbdata;
} pmix_setup_caddy_t;
PMIX_CLASS_DECLARATION(pmix_setup_caddy_t);

typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    volatile bool active;
    pmix_status_t status;
    pmix_proc_t source;
    pmix_data_range_t range;
    bool nondefault;
    pmix_info_t *info;
    size_t ninfo;
    pmix_buffer_t *buf;
    pmix_op_cbfunc_t cbfunc;
    void *cbdata;
} pmix_notify_caddy_t;
PMIX_CLASS_DECLARATION(pmix_notify_caddy_t);

typedef struct {
    pmix_list_item_t super;
    pmix_setup_caddy_t *cd;
} pmix_dmdx_remote_t;
PMIX_CLASS_DECLARATION(pmix_dmdx_remote_t);

typedef struct {
    pmix_list_item_t super;
    pmix_modex_cbfunc_t cbfunc;     // cbfunc to be executed when data is available
    void *cbdata;
} pmix_dmdx_request_t;
PMIX_CLASS_DECLARATION(pmix_dmdx_request_t);

typedef struct {
    pmix_list_item_t super;
    pmix_proc_t proc;               // id of proc whose data is being requested
    pmix_list_t loc_reqs;           // list of pmix_dmdx_request_t elem's keeping track of
                                    // all local ranks that are interested in this namespace-rank
    pmix_info_t *info;              // array of info structs for this request
    size_t ninfo;                   // number of info structs
} pmix_dmdx_local_t;
PMIX_CLASS_DECLARATION(pmix_dmdx_local_t);

/* define listener protocol types */
typedef uint16_t pmix_listener_protocol_t;
#define PMIX_PROTOCOL_V1        0
#define PMIX_PROTOCOL_TOOL      1
#define PMIX_PROTOCOL_V2        2

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
    pmix_bfrop_buffer_type_t buffer_type;
    char *cred;
    uid_t uid;
    gid_t gid;
} pmix_pending_connection_t;
PMIX_CLASS_DECLARATION(pmix_pending_connection_t);

/* event/error registration book keeping */
typedef struct {
    pmix_list_item_t super;
    pmix_peer_t *peer;
    bool enviro_events;
} pmix_peer_events_info_t;
PMIX_CLASS_DECLARATION(pmix_peer_events_info_t);

typedef struct {
    pmix_list_item_t super;
    pmix_list_t peers;              // list of pmix_prevents_info_t
    int code;
} pmix_regevents_info_t;
PMIX_CLASS_DECLARATION(pmix_regevents_info_t);

/* listener objects */
typedef struct pmix_listener_t {
    pmix_list_item_t super;
    pmix_listener_protocol_t protocol;
    int socket;
    struct sockaddr_un address;
    char *varname;
    char *uri;
    uint32_t owner;
    bool owner_given;
    uint32_t group;
    bool group_given;
    uint32_t mode;
} pmix_listener_t;
PMIX_CLASS_DECLARATION(pmix_listener_t);

typedef struct {
    pmix_pointer_array_t clients;           // array of pmix_peer_t local clients
    pmix_list_t collectives;                // list of active pmix_server_trkr_t
    pmix_list_t remote_pnd;                 // list of pmix_dmdx_remote_t awaiting arrival of data fror servicing remote req's
    pmix_list_t local_reqs;                 // list of pmix_dmdx_local_t awaiting arrival of data from local neighbours
    volatile bool listen_thread_active;     // listen thread is running
    pmix_list_t listeners;                  // list of pmix_listener_t
    int stop_thread[2];                     // pipe used to stop listener thread
    pmix_buffer_t gdata;                    // cache of data given to me for passing to all clients
    pmix_list_t events;                     // list of pmix_regevents_info_t registered events
    pmix_ring_buffer_t notifications;       // ring buffer of pending notifications
    bool tool_connections_allowed;
} pmix_server_globals_t;

typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    pmix_peer_t *peer;
    pmix_buffer_t *buf;
    uint32_t tag;
} pmix_usock_queue_t;
PMIX_CLASS_DECLARATION(pmix_usock_queue_t);

#define PMIX_PEER_CADDY(c, p, t)                \
    do {                                        \
        (c) = PMIX_NEW(pmix_server_caddy_t);    \
        (c)->hdr.tag = (t);                     \
        PMIX_RETAIN((p));                       \
        (c)->peer = (p);                        \
    } while (0)

#define PMIX_SND_CADDY(c, h, s)                                         \
    do {                                                                \
        (c) = PMIX_NEW(pmix_server_caddy_t);                            \
        (void)memcpy(&(c)->hdr, &(h), sizeof(pmix_usock_hdr_t));        \
        PMIX_RETAIN((s));                                               \
        (c)->snd = (s);                                                 \
    } while (0)

#define PMIX_SETUP_COLLECTIVE(c, t)             \
    do {                                        \
        (c) = PMIX_NEW(pmix_trkr_caddy_t);      \
        (c)->trk = (t);                         \
    } while (0)

#define PMIX_EXECUTE_COLLECTIVE(c, t, f)                        \
    do {                                                        \
        PMIX_SETUP_COLLECTIVE(c, t);                            \
        event_assign(&((c)->ev), pmix_globals.evbase, -1,       \
                     EV_WRITE, (f), (c));                       \
        event_active(&((c)->ev), EV_WRITE, 1);                  \
    } while (0)


/* queue a message to be sent to one of our procs - must
 * provide the following params:
 *
 * t - tag to be sent to
 * b - buffer to be sent
 */
#define PMIX_SERVER_QUEUE_REPLY(p, t, b)                                \
    do {                                                                \
        pmix_usock_queue_t *queue;                                      \
        queue = PMIX_NEW(pmix_usock_queue_t);                           \
        queue->peer = (p);                                              \
        queue->buf  = (b);                                              \
        queue->tag  = (t);                                              \
        pmix_output_verbose(2, pmix_globals.debug_output,               \
                        "[%s:%d] queue reply to %s:%d on tag %d",       \
                        __FILE__, __LINE__,                             \
                        (queue->peer)->info->nptr->nspace,              \
                        (queue->peer)->info->rank, (queue->tag));       \
        event_assign(&queue->ev, pmix_globals.evbase, -1,               \
                       EV_WRITE, pmix_server_queue_message, queue);     \
        event_priority_set(&queue->ev, 0);                              \
        event_active(&queue->ev, EV_WRITE, 1);                          \
    } while (0)

pmix_status_t pmix_prepare_listening(pmix_listener_t *lt, bool *need_listener);
pmix_status_t pmix_start_listening(void);
void pmix_stop_listening(void);

bool pmix_server_trk_update(pmix_server_trkr_t *trk);

void pmix_pending_nspace_requests(pmix_nspace_t *nptr);
pmix_status_t pmix_pending_resolve(pmix_nspace_t *nptr, pmix_rank_t rank,
                                   pmix_status_t status, pmix_dmdx_local_t *lcd);


pmix_status_t pmix_server_abort(pmix_peer_t *peer, pmix_buffer_t *buf,
                                pmix_op_cbfunc_t cbfunc, void *cbdata);

pmix_status_t pmix_server_commit(pmix_peer_t *peer, pmix_buffer_t *buf);

pmix_status_t pmix_server_fence(pmix_server_caddy_t *cd,
                                pmix_buffer_t *buf,
                                pmix_modex_cbfunc_t modexcbfunc,
                                pmix_op_cbfunc_t opcbfunc);

pmix_status_t pmix_server_get(pmix_buffer_t *buf,
                              pmix_modex_cbfunc_t cbfunc,
                              void *cbdata);

pmix_status_t pmix_server_publish(pmix_peer_t *peer,
                                  pmix_buffer_t *buf,
                                  pmix_op_cbfunc_t cbfunc,
                                  void *cbdata);

pmix_status_t pmix_server_lookup(pmix_peer_t *peer,
                                 pmix_buffer_t *buf,
                                 pmix_lookup_cbfunc_t cbfunc,
                                 void *cbdata);

pmix_status_t pmix_server_unpublish(pmix_peer_t *peer,
                                    pmix_buffer_t *buf,
                                    pmix_op_cbfunc_t cbfunc,
                                    void *cbdata);

pmix_status_t pmix_server_spawn(pmix_peer_t *peer,
                                pmix_buffer_t *buf,
                                pmix_spawn_cbfunc_t cbfunc,
                                void *cbdata);

pmix_status_t pmix_server_connect(pmix_server_caddy_t *cd,
                                  pmix_buffer_t *buf, bool disconnect,
                                  pmix_op_cbfunc_t cbfunc);

void pmix_pack_proc_map(pmix_buffer_t *buf,
                        char **nodes, char **procs);
pmix_status_t pmix_regex_parse_nodes(const char *regexp, char ***names);
pmix_status_t pmix_regex_parse_procs(const char *regexp, char ***procs);

pmix_status_t pmix_server_notify_error(pmix_status_t status,
                                       pmix_proc_t procs[], size_t nprocs,
                                       pmix_proc_t error_procs[], size_t error_nprocs,
                                       pmix_info_t info[], size_t ninfo,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata);

pmix_status_t pmix_server_register_events(pmix_peer_t *peer,
                                          pmix_buffer_t *buf,
                                          pmix_op_cbfunc_t cbfunc,
                                          void *cbdata);

void pmix_server_deregister_events(pmix_peer_t *peer,
                                   pmix_buffer_t *buf);

pmix_status_t pmix_server_query(pmix_peer_t *peer,
                                pmix_buffer_t *buf,
                                pmix_info_cbfunc_t cbfunc,
                                void *cbdata);

pmix_status_t pmix_server_log(pmix_peer_t *peer,
                              pmix_buffer_t *buf,
                              pmix_op_cbfunc_t cbfunc,
                              void *cbdata);

pmix_status_t pmix_server_event_recvd_from_client(pmix_peer_t *peer,
                                                  pmix_buffer_t *buf,
                                                  pmix_op_cbfunc_t cbfunc,
                                                  void *cbdata);
void pmix_server_execute_collective(int sd, short args, void *cbdata);

void pmix_server_queue_message(int fd, short args, void *cbdata);

extern pmix_server_module_t pmix_host_server;
extern pmix_server_globals_t pmix_server_globals;

#endif // PMIX_SERVER_OPS_H
