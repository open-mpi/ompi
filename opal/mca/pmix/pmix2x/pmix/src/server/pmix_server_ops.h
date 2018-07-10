/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2018 Intel, Inc. All rights reserved.
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
#include "src/include/types.h"
#include <pmix_common.h>

#include <src/class/pmix_ring_buffer.h>
#include <pmix_server.h>
#include "src/threads/threads.h"
#include "src/include/pmix_globals.h"
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
    pmix_lock_t lock;
    char *nspace;
    pmix_status_t status;
    pmix_status_t *codes;
    size_t ncodes;
    pmix_proc_t proc;
    pmix_proc_t *procs;
    size_t nprocs;
    uid_t uid;
    gid_t gid;
    void *server_object;
    int nlocalprocs;
    pmix_info_t *info;
    size_t ninfo;
    char **keys;
    pmix_app_t *apps;
    size_t napps;
    pmix_op_cbfunc_t opcbfunc;
    pmix_dmodex_response_fn_t cbfunc;
    pmix_setup_application_cbfunc_t setupcbfunc;
    pmix_lookup_cbfunc_t lkcbfunc;
    pmix_spawn_cbfunc_t spcbfunc;
    void *cbdata;
} pmix_setup_caddy_t;
PMIX_CLASS_DECLARATION(pmix_setup_caddy_t);

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

typedef struct {
    pmix_list_t nspaces;                    // list of pmix_nspace_t for the nspaces we know about
    pmix_pointer_array_t clients;           // array of pmix_peer_t local clients
    pmix_list_t collectives;                // list of active pmix_server_trkr_t
    pmix_list_t remote_pnd;                 // list of pmix_dmdx_remote_t awaiting arrival of data fror servicing remote req's
    pmix_list_t local_reqs;                 // list of pmix_dmdx_local_t awaiting arrival of data from local neighbours
    pmix_list_t gdata;                      // cache of data given to me for passing to all clients
    pmix_list_t events;                     // list of pmix_regevents_info_t registered events
    bool tool_connections_allowed;
} pmix_server_globals_t;

#define PMIX_GDS_CADDY(c, p, t)                \
    do {                                        \
        (c) = PMIX_NEW(pmix_server_caddy_t);    \
        (c)->hdr.tag = (t);                     \
        PMIX_RETAIN((p));                       \
        (c)->peer = (p);                        \
    } while (0)

#define PMIX_SETUP_COLLECTIVE(c, t)             \
    do {                                        \
        (c) = PMIX_NEW(pmix_trkr_caddy_t);      \
        (c)->trk = (t);                         \
    } while (0)

#define PMIX_EXECUTE_COLLECTIVE(c, t, f)                        \
    do {                                                        \
        PMIX_SETUP_COLLECTIVE(c, t);                            \
        pmix_event_assign(&((c)->ev), pmix_globals.evbase, -1,  \
                          EV_WRITE, (f), (c));                  \
        pmix_event_active(&((c)->ev), EV_WRITE, 1);             \
    } while (0)



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

pmix_status_t pmix_server_alloc(pmix_peer_t *peer,
                                pmix_buffer_t *buf,
                                pmix_info_cbfunc_t cbfunc,
                                void *cbdata);

pmix_status_t pmix_server_job_ctrl(pmix_peer_t *peer,
                                   pmix_buffer_t *buf,
                                   pmix_info_cbfunc_t cbfunc,
                                   void *cbdata);

pmix_status_t pmix_server_monitor(pmix_peer_t *peer,
                                  pmix_buffer_t *buf,
                                  pmix_info_cbfunc_t cbfunc,
                                  void *cbdata);

pmix_status_t pmix_server_event_recvd_from_client(pmix_peer_t *peer,
                                                  pmix_buffer_t *buf,
                                                  pmix_op_cbfunc_t cbfunc,
                                                  void *cbdata);
void pmix_server_execute_collective(int sd, short args, void *cbdata);

PMIX_EXPORT extern pmix_server_module_t pmix_host_server;
PMIX_EXPORT extern pmix_server_globals_t pmix_server_globals;

#endif // PMIX_SERVER_OPS_H
