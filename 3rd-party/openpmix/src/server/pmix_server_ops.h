/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016-2020 IBM Corporation.  All rights reserved.
 * Copyright (c) 2016-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 */

#ifndef PMIX_SERVER_OPS_H
#define PMIX_SERVER_OPS_H

#include <unistd.h>
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif

#include "src/include/pmix_config.h"
#include "pmix_common.h"
#include "pmix_server.h"

#include "src/class/pmix_hotel.h"
#include "src/include/pmix_globals.h"
#include "src/include/pmix_types.h"
#include "src/threads/pmix_threads.h"
#include "src/util/pmix_hash.h"

#define PMIX_IOF_HOTEL_SIZE 256
#define PMIX_IOF_MAX_STAY   300000000

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
    pmix_peer_t *peer;
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
    bool copied;
    char **keys;
    pmix_app_t *apps;
    size_t napps;
    pmix_iof_channel_t channels;
    pmix_iof_flags_t flags;
    pmix_byte_object_t *bo;
    size_t nbo;
    /* timestamp receipt of the notification so we
     * can evict the oldest one if we get overwhelmed */
    time_t ts;
    /* what room of the hotel they are in */
    int room;
    pmix_op_cbfunc_t opcbfunc;
    pmix_dmodex_response_fn_t cbfunc;
    pmix_setup_application_cbfunc_t setupcbfunc;
    pmix_lookup_cbfunc_t lkcbfunc;
    pmix_spawn_cbfunc_t spcbfunc;
    void *cbdata;
} pmix_setup_caddy_t;
PMIX_CLASS_DECLARATION(pmix_setup_caddy_t);

/* define a callback function returning inventory */
typedef void (*pmix_inventory_cbfunc_t)(pmix_status_t status, pmix_list_t *inventory, void *cbdata);

/* define an object for rolling up the inventory*/
typedef struct {
    pmix_object_t super;
    pmix_lock_t lock;
    pmix_event_t ev;
    pmix_status_t status;
    int requests;
    int replies;
    pmix_list_t payload; // list of pmix_kval_t containing the replies
    pmix_info_t *info;
    size_t ninfo;
    pmix_inventory_cbfunc_t cbfunc;
    pmix_info_cbfunc_t infocbfunc;
    pmix_op_cbfunc_t opcbfunc;
    void *cbdata;
} pmix_inventory_rollup_t;
PMIX_CLASS_DECLARATION(pmix_inventory_rollup_t);

typedef struct {
    pmix_list_item_t super;
    pmix_setup_caddy_t *cd;
} pmix_dmdx_remote_t;
PMIX_CLASS_DECLARATION(pmix_dmdx_remote_t);

typedef struct {
    pmix_list_item_t super;
    pmix_proc_t proc;     // id of proc whose data is being requested
    pmix_list_t loc_reqs; // list of pmix_dmdx_request_t elem is keeping track of
                          // all local ranks that are interested in this namespace-rank
    pmix_info_t *info;    // array of info structs for this request
    size_t ninfo;         // number of info structs
} pmix_dmdx_local_t;
PMIX_CLASS_DECLARATION(pmix_dmdx_local_t);

typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    bool event_active;
    pmix_dmdx_local_t *lcd;
    char *key;
    pmix_modex_cbfunc_t cbfunc; // cbfunc to be executed when data is available
    void *cbdata;
} pmix_dmdx_request_t;
PMIX_CLASS_DECLARATION(pmix_dmdx_request_t);

/* event/error registration book keeping */
typedef struct {
    pmix_list_item_t super;
    pmix_peer_t *peer;
    bool enviro_events;
    pmix_proc_t *affected;
    size_t naffected;
} pmix_peer_events_info_t;
PMIX_CLASS_DECLARATION(pmix_peer_events_info_t);

typedef struct {
    pmix_list_item_t super;
    pmix_list_t peers; // list of pmix_peer_events_info_t
    int code;
} pmix_regevents_info_t;
PMIX_CLASS_DECLARATION(pmix_regevents_info_t);

typedef struct {
    pmix_list_item_t super;
    pmix_group_t *grp;
    pmix_rank_t rank;
    size_t idx;
} pmix_group_caddy_t;
PMIX_CLASS_DECLARATION(pmix_group_caddy_t);

typedef struct {
    pmix_list_item_t super;
    pmix_proc_t source;
    pmix_iof_channel_t channel;
    pmix_byte_object_t *bo;
    pmix_info_t *info;
    size_t ninfo;
} pmix_iof_cache_t;
PMIX_CLASS_DECLARATION(pmix_iof_cache_t);

typedef struct {
    pmix_list_item_t super;
    char *name;
    pmix_proc_t *members;
    size_t nmembers;
} pmix_pset_t;
PMIX_CLASS_DECLARATION(pmix_pset_t);

typedef struct {
    pmix_list_t nspaces;          // list of pmix_nspace_t for the nspaces we know about
    pmix_pointer_array_t clients; // array of pmix_peer_t local clients
    pmix_list_t collectives;      // list of active pmix_server_trkr_t
    pmix_list_t remote_pnd; // list of pmix_dmdx_remote_t awaiting arrival of data fror servicing
                            // remote req's
    pmix_list_t local_reqs;     // list of pmix_dmdx_local_t awaiting arrival of data from local neighbours
    pmix_list_t gdata;  // cache of data given to me for passing to all clients
    char **genvars;     // argv array of envars given to me for passing to all clients
    pmix_list_t events; // list of pmix_regevents_info_t registered events
    char **failedgrps;    // group IDs that failed to construct
    pmix_list_t iof;    // IO to be forwarded to clients
    pmix_list_t iof_residuals;  // leftover bytes waiting for newline
    pmix_list_t psets;  // list of known psets and memberships
    size_t max_iof_cache; // max number of IOF messages to cache
    bool tool_connections_allowed;
    char *tmpdir;             // temporary directory for this server
    char *system_tmpdir;      // system tmpdir
    bool fence_localonly_opt; // local-only fence optimization
    // verbosity for server get operations
    int get_output;
    int get_verbose;
    // verbosity for server connect operations
    int connect_output;
    int connect_verbose;
    // verbosity for server fence operations
    int fence_output;
    int fence_verbose;
    // verbosity for server pub operations
    int pub_output;
    int pub_verbose;
    // verbosity for server spawn operations
    int spawn_output;
    int spawn_verbose;
    // verbosity for server event operations
    int event_output;
    int event_verbose;
    // verbosity for server iof operations
    int iof_output;
    int iof_verbose;
    // verbosity for basic server functions
    int base_output;
    int base_verbose;
    // verbosity for server group operations
    int group_output;
    int group_verbose;
} pmix_server_globals_t;

#define PMIX_GDS_CADDY(c, p, t)              \
    do {                                     \
        (c) = PMIX_NEW(pmix_server_caddy_t); \
        (c)->hdr.tag = (t);                  \
        PMIX_RETAIN((p));                    \
        (c)->peer = (p);                     \
    } while (0)

#define PMIX_SETUP_COLLECTIVE(c, t)        \
    do {                                   \
        (c) = PMIX_NEW(pmix_trkr_caddy_t); \
        (c)->trk = (t);                    \
    } while (0)

#define PMIX_EXECUTE_COLLECTIVE(c, t, f)                                            \
    do {                                                                            \
        PMIX_SETUP_COLLECTIVE(c, t);                                                \
        pmix_event_assign(&((c)->ev), pmix_globals.evbase, -1, EV_WRITE, (f), (c)); \
        pmix_event_active(&((c)->ev), EV_WRITE, 1);                                 \
    } while (0)

PMIX_EXPORT bool pmix_server_trk_update(pmix_server_trkr_t *trk);

PMIX_EXPORT void pmix_pending_nspace_requests(pmix_namespace_t *nptr);
PMIX_EXPORT pmix_status_t pmix_pending_resolve(pmix_namespace_t *nptr, pmix_rank_t rank,
                                               pmix_status_t status, pmix_scope_t scope,
                                               pmix_dmdx_local_t *lcd);

PMIX_EXPORT pmix_status_t pmix_server_abort(pmix_peer_t *peer, pmix_buffer_t *buf,
                                            pmix_op_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_commit(pmix_peer_t *peer, pmix_buffer_t *buf);

PMIX_EXPORT pmix_status_t pmix_server_fence(pmix_server_caddy_t *cd, pmix_buffer_t *buf,
                                            pmix_modex_cbfunc_t modexcbfunc,
                                            pmix_op_cbfunc_t opcbfunc);

PMIX_EXPORT pmix_status_t pmix_server_get(pmix_buffer_t *buf, pmix_modex_cbfunc_t cbfunc,
                                          void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_publish(pmix_peer_t *peer, pmix_buffer_t *buf,
                                              pmix_op_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_lookup(pmix_peer_t *peer, pmix_buffer_t *buf,
                                             pmix_lookup_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_unpublish(pmix_peer_t *peer, pmix_buffer_t *buf,
                                                pmix_op_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_spawn(pmix_peer_t *peer, pmix_buffer_t *buf,
                                            pmix_spawn_cbfunc_t cbfunc, void *cbdata);
PMIX_EXPORT void pmix_server_spawn_parser(pmix_peer_t *peer, pmix_setup_caddy_t *cd);
PMIX_EXPORT void pmix_server_spcbfunc(pmix_status_t status, char nspace[], void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_connect(pmix_server_caddy_t *cd, pmix_buffer_t *buf,
                                              pmix_op_cbfunc_t cbfunc);

PMIX_EXPORT pmix_status_t pmix_server_disconnect(pmix_server_caddy_t *cd, pmix_buffer_t *buf,
                                                 pmix_op_cbfunc_t cbfunc);

PMIX_EXPORT pmix_status_t pmix_server_notify_error(pmix_status_t status, pmix_proc_t procs[],
                                                   size_t nprocs, pmix_proc_t error_procs[],
                                                   size_t error_nprocs, pmix_info_t info[],
                                                   size_t ninfo, pmix_op_cbfunc_t cbfunc,
                                                   void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_register_events(pmix_peer_t *peer, pmix_buffer_t *buf,
                                                      pmix_op_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT void pmix_server_deregister_events(pmix_peer_t *peer, pmix_buffer_t *buf);

PMIX_EXPORT pmix_status_t pmix_server_query(pmix_peer_t *peer, pmix_buffer_t *buf,
                                            pmix_info_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_log(pmix_peer_t *peer, pmix_buffer_t *buf,
                                          pmix_op_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_alloc(pmix_peer_t *peer, pmix_buffer_t *buf,
                                            pmix_info_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_job_ctrl(pmix_peer_t *peer, pmix_buffer_t *buf,
                                               pmix_info_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_monitor(pmix_peer_t *peer, pmix_buffer_t *buf,
                                              pmix_info_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_get_credential(pmix_peer_t *peer, pmix_buffer_t *buf,
                                                     pmix_credential_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_validate_credential(pmix_peer_t *peer, pmix_buffer_t *buf,
                                                          pmix_validation_cbfunc_t cbfunc,
                                                          void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_iofreg(pmix_peer_t *peer, pmix_buffer_t *buf,
                                             pmix_op_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_iofstdin(pmix_peer_t *peer, pmix_buffer_t *buf,
                                               pmix_op_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_iofdereg(pmix_peer_t *peer, pmix_buffer_t *buf,
                                               pmix_op_cbfunc_t cbfunc, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_grpconstruct(pmix_server_caddy_t *cd, pmix_buffer_t *buf);

PMIX_EXPORT pmix_status_t pmix_server_grpdestruct(pmix_server_caddy_t *cd, pmix_buffer_t *buf);

PMIX_EXPORT pmix_status_t pmix_server_event_recvd_from_client(pmix_peer_t *peer, pmix_buffer_t *buf,
                                                              pmix_op_cbfunc_t cbfunc,
                                                              void *cbdata);
PMIX_EXPORT void pmix_server_execute_collective(int sd, short args, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_server_initialize(void);

PMIX_EXPORT void pmix_server_message_handler(struct pmix_peer_t *pr, pmix_ptl_hdr_t *hdr,
                                             pmix_buffer_t *buf, void *cbdata);

PMIX_EXPORT void pmix_server_purge_events(pmix_peer_t *peer, pmix_proc_t *proc);

PMIX_EXPORT pmix_status_t pmix_server_fabric_register(pmix_server_caddy_t *cd, pmix_buffer_t *buf,
                                                      pmix_info_cbfunc_t cbfunc);

PMIX_EXPORT pmix_status_t pmix_server_fabric_update(pmix_server_caddy_t *cd, pmix_buffer_t *buf,
                                                    pmix_info_cbfunc_t cbfunc);

PMIX_EXPORT pmix_status_t pmix_server_fabric_get_vertex_info(pmix_server_caddy_t *cd,
                                                             pmix_buffer_t *buf,
                                                             pmix_info_cbfunc_t cbfunc);

PMIX_EXPORT pmix_status_t pmix_server_fabric_get_device_index(pmix_server_caddy_t *cd,
                                                              pmix_buffer_t *buf,
                                                              pmix_info_cbfunc_t cbfunc);

PMIX_EXPORT pmix_status_t pmix_server_device_dists(pmix_server_caddy_t *cd,
                                                   pmix_buffer_t *buf,
                                                   pmix_device_dist_cbfunc_t cbfunc);

PMIX_EXPORT pmix_status_t pmix_server_refresh_cache(pmix_server_caddy_t *cd,
                                                    pmix_buffer_t *buf,
                                                    pmix_op_cbfunc_t cbfunc);

PMIX_EXPORT void pmix_server_query_cbfunc(pmix_status_t status,
                                          pmix_info_t *info, size_t ninfo, void *cbdata,
                                          pmix_release_cbfunc_t release_fn, void *release_cbdata);

PMIX_EXPORT extern pmix_server_module_t pmix_host_server;
PMIX_EXPORT extern pmix_server_globals_t pmix_server_globals;

static inline pmix_peer_t* pmix_get_peer_object(const pmix_proc_t *proc)
{
    pmix_peer_t *peer;
    int n;

    for (n=0; n < pmix_server_globals.clients.size; n++) {
        peer = (pmix_peer_t *) pmix_pointer_array_get_item(&pmix_server_globals.clients, n);
        if (NULL == peer) {
            continue;
        }
        if (PMIX_CHECK_NSPACE(proc->nspace, peer->info->pname.nspace) &&
            proc->rank == peer->info->pname.rank) {
            return peer;
        }
    }
    return NULL;
}


#endif // PMIX_SERVER_OPS_H
