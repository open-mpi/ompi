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
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _PMIX_SERVER_INTERNAL_H_
#define _PMIX_SERVER_INTERNAL_H_

#include "prte_config.h"
#include "types.h"

#ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#    include <sys/un.h>
#endif
#include <pmix_server.h>

#include "src/class/pmix_hotel.h"
#include "src/event/event-internal.h"
#include "src/mca/base/pmix_base.h"
#include "src/pmix/pmix-internal.h"
#include "src/include/pmix_atomic.h"
#include "src/util/pmix_printf.h"
#include "src/util/proc_info.h"
#include "types.h"

#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"

BEGIN_C_DECLS

/* object for tracking requests so we can
 * correctly route the eventual reply */
typedef struct {
    pmix_object_t super;
    prte_event_t ev;
    bool event_active;
    prte_event_t cycle;
    bool cycle_active;
    bool inprogress;
    bool timed_out;
    char *operation;
    char *cmdline;
    char *key;
    int status;
    pmix_status_t pstatus;
    int timeout;
    int local_index;
    int remote_index;
    bool flag;
    bool launcher;
    bool scheduler;
    uid_t uid;
    gid_t gid;
    pid_t pid;
    pmix_info_t *info;
    size_t ninfo;
    char *data;
    size_t sz;
    pmix_data_range_t range;
    pmix_proc_t proxy;
    pmix_proc_t target;
    pmix_proc_t tproc;
    prte_job_t *jdata;
    pmix_data_buffer_t msg;
    pmix_op_cbfunc_t opcbfunc;
    pmix_modex_cbfunc_t mdxcbfunc;
    pmix_spawn_cbfunc_t spcbfunc;
    pmix_lookup_cbfunc_t lkcbfunc;
    pmix_release_cbfunc_t rlcbfunc;
    pmix_tool_connection_cbfunc_t toolcbfunc;
    pmix_info_cbfunc_t infocbfunc;
    void *cbdata;
} pmix_server_req_t;
PMIX_CLASS_DECLARATION(pmix_server_req_t);

/* object for thread-shifting server operations */
typedef struct {
    pmix_object_t super;
    prte_event_t ev;
    pmix_status_t status;
    pmix_status_t *codes;
    size_t ncodes;
    pmix_proc_t proc;
    const char *msg;
    void *server_object;
    pmix_proc_t proct;
    pmix_proc_t *procs;
    size_t nprocs;
    pmix_proc_t *eprocs;
    size_t neprocs;
    pmix_info_t *info;
    size_t ninfo;
    pmix_info_t *directives;
    size_t ndirs;
    pmix_app_t *apps;
    size_t napps;
    pmix_query_t *queries;
    size_t nqueries;
    pmix_alloc_directive_t allocdir;
    uint32_t sessionID;
    pmix_op_cbfunc_t cbfunc;
    pmix_info_cbfunc_t infocbfunc;
    pmix_tool_connection_cbfunc_t toolcbfunc;
    pmix_spawn_cbfunc_t spcbfunc;
    void *cbdata;
} prte_pmix_server_op_caddy_t;
PMIX_CLASS_DECLARATION(prte_pmix_server_op_caddy_t);

typedef struct {
    pmix_list_item_t super;
    pmix_proc_t name;
} prte_pmix_tool_t;
PMIX_CLASS_DECLARATION(prte_pmix_tool_t);

#define PRTE_IO_OP(t, nt, b, fn, cfn, cbd)                                         \
    do {                                                                           \
        prte_pmix_server_op_caddy_t *_cd;                                          \
        _cd = PMIX_NEW(prte_pmix_server_op_caddy_t);                               \
        _cd->procs = (pmix_proc_t *) (t);                                          \
        _cd->nprocs = (nt);                                                        \
        _cd->server_object = (void *) (b);                                         \
        _cd->cbfunc = (cfn);                                                       \
        _cd->cbdata = (cbd);                                                       \
        prte_event_set(prte_event_base, &(_cd->ev), -1, PRTE_EV_WRITE, (fn), _cd); \
        PMIX_POST_OBJECT(_cd);                                                     \
        prte_event_active(&(_cd->ev), PRTE_EV_WRITE, 1);                           \
    } while (0);

#define PRTE_DMX_REQ(p, i, ni, cf, ocf, ocd)                                         \
    do {                                                                             \
        pmix_server_req_t *_req;                                                     \
        _req = PMIX_NEW(pmix_server_req_t);                                          \
        pmix_asprintf(&_req->operation, "DMDX: %s:%d", __FILE__, __LINE__);          \
        memcpy(&_req->tproc, (p), sizeof(pmix_proc_t));                              \
        _req->info = (pmix_info_t *) (i);                                            \
        _req->ninfo = (ni);                                                          \
        _req->mdxcbfunc = (ocf);                                                     \
        _req->cbdata = (ocd);                                                        \
        prte_event_set(prte_event_base, &(_req->ev), -1, PRTE_EV_WRITE, (cf), _req); \
        PMIX_POST_OBJECT(_req);                                                      \
        prte_event_active(&(_req->ev), PRTE_EV_WRITE, 1);                            \
    } while (0);

#define PRTE_SPN_REQ(j, cf, ocf, ocd)                                                \
    do {                                                                             \
        pmix_server_req_t *_req;                                                     \
        _req = PMIX_NEW(pmix_server_req_t);                                          \
        pmix_asprintf(&_req->operation, "SPAWN: %s:%d", __FILE__, __LINE__);         \
        _req->jdata = (j);                                                           \
        _req->spcbfunc = (ocf);                                                      \
        _req->cbdata = (ocd);                                                        \
        prte_event_set(prte_event_base, &(_req->ev), -1, PRTE_EV_WRITE, (cf), _req); \
        PMIX_POST_OBJECT(_req);                                                      \
        prte_event_active(&(_req->ev), PRTE_EV_WRITE, 1);                            \
    } while (0);

#define PRTE_PMIX_OPERATION(p, np, i, ni, fn, cf, cb)                              \
    do {                                                                           \
        prte_pmix_server_op_caddy_t *_cd;                                          \
        _cd = PMIX_NEW(prte_pmix_server_op_caddy_t);                               \
        _cd->procs = (pmix_proc_t *) (p);                                          \
        _cd->nprocs = (np);                                                        \
        _cd->info = (pmix_info_t *) (i);                                           \
        _cd->ninfo = (ni);                                                         \
        _cd->cbfunc = (cf);                                                        \
        _cd->cbdata = (cb);                                                        \
        prte_event_set(prte_event_base, &(_cd->ev), -1, PRTE_EV_WRITE, (fn), _cd); \
        PMIX_POST_OBJECT(_cd);                                                     \
        prte_event_active(&(_cd->ev), PRTE_EV_WRITE, 1);                           \
    } while (0);

#define PRTE_SERVER_PMIX_THREADSHIFT(p, s, st, m, pl, pn, fn, cf, cb)                     \
    do {                                                                           \
        prte_pmix_server_op_caddy_t *_cd;                                          \
        _cd = PMIX_NEW(prte_pmix_server_op_caddy_t);                               \
        PMIX_LOAD_PROCID(&_cd->proc, (p)->nspace, (p)->rank);                      \
        _cd->server_object = (s);                                                  \
        _cd->status = (st);                                                        \
        _cd->msg = (m);                                                            \
        _cd->procs = (pl);                                                         \
        _cd->nprocs = (pn);                                                        \
        _cd->cbfunc = (cf);                                                        \
        _cd->cbdata = (cb);                                                        \
        prte_event_set(prte_event_base, &(_cd->ev), -1, PRTE_EV_WRITE, (fn), _cd); \
        PMIX_POST_OBJECT(_cd);                                                     \
        prte_event_active(&(_cd->ev), PRTE_EV_WRITE, 1);                           \
    } while (0);

/* define the server module functions */
PRTE_EXPORT extern pmix_status_t pmix_server_client_connected_fn(const pmix_proc_t *proc,
                                                                 void *server_object,
                                                                 pmix_op_cbfunc_t cbfunc,
                                                                 void *cbdata);
PRTE_EXPORT extern pmix_status_t pmix_server_client_finalized_fn(const pmix_proc_t *proc,
                                                                 void *server_object,
                                                                 pmix_op_cbfunc_t cbfunc,
                                                                 void *cbdata);
PRTE_EXPORT extern pmix_status_t pmix_server_abort_fn(const pmix_proc_t *proc, void *server_object,
                                                      int status, const char msg[],
                                                      pmix_proc_t procs[], size_t nprocs,
                                                      pmix_op_cbfunc_t cbfunc, void *cbdata);
PRTE_EXPORT extern pmix_status_t pmix_server_fencenb_fn(const pmix_proc_t procs[], size_t nprocs,
                                                        const pmix_info_t info[], size_t ninfo,
                                                        char *data, size_t ndata,
                                                        pmix_modex_cbfunc_t cbfunc, void *cbdata);
PRTE_EXPORT extern pmix_status_t pmix_server_dmodex_req_fn(const pmix_proc_t *proc,
                                                           const pmix_info_t info[], size_t ninfo,
                                                           pmix_modex_cbfunc_t cbfunc,
                                                           void *cbdata);
PRTE_EXPORT extern pmix_status_t pmix_server_publish_fn(const pmix_proc_t *proc,
                                                        const pmix_info_t info[], size_t ninfo,
                                                        pmix_op_cbfunc_t cbfunc, void *cbdata);
PRTE_EXPORT extern pmix_status_t pmix_server_lookup_fn(const pmix_proc_t *proc, char **keys,
                                                       const pmix_info_t info[], size_t ninfo,
                                                       pmix_lookup_cbfunc_t cbfunc, void *cbdata);
PRTE_EXPORT extern pmix_status_t pmix_server_unpublish_fn(const pmix_proc_t *proc, char **keys,
                                                          const pmix_info_t info[], size_t ninfo,
                                                          pmix_op_cbfunc_t cbfunc, void *cbdata);
PRTE_EXPORT extern pmix_status_t pmix_server_spawn_fn(const pmix_proc_t *proc,
                                                      const pmix_info_t job_info[], size_t ninfo,
                                                      const pmix_app_t apps[], size_t napps,
                                                      pmix_spawn_cbfunc_t cbfunc, void *cbdata);
PRTE_EXPORT extern pmix_status_t pmix_server_connect_fn(const pmix_proc_t procs[], size_t nprocs,
                                                        const pmix_info_t info[], size_t ninfo,
                                                        pmix_op_cbfunc_t cbfunc, void *cbdata);
PRTE_EXPORT extern pmix_status_t pmix_server_disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                                                           const pmix_info_t info[], size_t ninfo,
                                                           pmix_op_cbfunc_t cbfunc, void *cbdata);
PRTE_EXPORT extern pmix_status_t
pmix_server_register_events_fn(pmix_status_t *codes, size_t ncodes, const pmix_info_t info[],
                               size_t ninfo, pmix_op_cbfunc_t cbfunc, void *cbdata);
PRTE_EXPORT extern pmix_status_t pmix_server_deregister_events_fn(pmix_status_t *codes,
                                                                  size_t ncodes,
                                                                  pmix_op_cbfunc_t cbfunc,
                                                                  void *cbdata);
PRTE_EXPORT extern pmix_status_t
pmix_server_notify_event(pmix_status_t code, const pmix_proc_t *source, pmix_data_range_t range,
                         pmix_info_t info[], size_t ninfo, pmix_op_cbfunc_t cbfunc, void *cbdata);
PRTE_EXPORT extern pmix_status_t pmix_server_query_fn(pmix_proc_t *proct, pmix_query_t *queries,
                                                      size_t nqueries, pmix_info_cbfunc_t cbfunc,
                                                      void *cbdata);
PRTE_EXPORT extern void pmix_tool_connected_fn(pmix_info_t *info, size_t ninfo,
                                               pmix_tool_connection_cbfunc_t cbfunc, void *cbdata);

PRTE_EXPORT extern void pmix_server_log_fn(const pmix_proc_t *client, const pmix_info_t data[],
                                           size_t ndata, const pmix_info_t directives[],
                                           size_t ndirs, pmix_op_cbfunc_t cbfunc, void *cbdata);

PRTE_EXPORT extern pmix_status_t pmix_server_alloc_fn(const pmix_proc_t *client,
                                                      pmix_alloc_directive_t directive,
                                                      const pmix_info_t data[], size_t ndata,
                                                      pmix_info_cbfunc_t cbfunc, void *cbdata);

PRTE_EXPORT extern pmix_status_t
pmix_server_job_ctrl_fn(const pmix_proc_t *requestor, const pmix_proc_t targets[], size_t ntargets,
                        const pmix_info_t directives[], size_t ndirs, pmix_info_cbfunc_t cbfunc,
                        void *cbdata);

PRTE_EXPORT extern pmix_status_t pmix_server_iof_pull_fn(const pmix_proc_t procs[], size_t nprocs,
                                                         const pmix_info_t directives[],
                                                         size_t ndirs, pmix_iof_channel_t channels,
                                                         pmix_op_cbfunc_t cbfunc, void *cbdata);

PRTE_EXPORT extern pmix_status_t pmix_server_stdin_fn(const pmix_proc_t *source,
                                                      const pmix_proc_t targets[], size_t ntargets,
                                                      const pmix_info_t directives[], size_t ndirs,
                                                      const pmix_byte_object_t *bo,
                                                      pmix_op_cbfunc_t cbfunc, void *cbdata);

PRTE_EXPORT extern pmix_status_t pmix_server_group_fn(pmix_group_operation_t op, char *gpid,
                                                      const pmix_proc_t procs[], size_t nprocs,
                                                      const pmix_info_t directives[], size_t ndirs,
                                                      pmix_info_cbfunc_t cbfunc, void *cbdata);

/* declare the RML recv functions for responses */
PRTE_EXPORT extern void pmix_server_launch_resp(int status, pmix_proc_t *sender,
                                                pmix_data_buffer_t *buffer, prte_rml_tag_t tg,
                                                void *cbdata);

PRTE_EXPORT extern void pmix_server_keyval_client(int status, pmix_proc_t *sender,
                                                  pmix_data_buffer_t *buffer, prte_rml_tag_t tg,
                                                  void *cbdata);

PRTE_EXPORT extern void pmix_server_notify(int status, pmix_proc_t *sender,
                                           pmix_data_buffer_t *buffer, prte_rml_tag_t tg,
                                           void *cbdata);

PRTE_EXPORT extern void pmix_server_jobid_return(int status, pmix_proc_t *sender,
                                           pmix_data_buffer_t *buffer, prte_rml_tag_t tg,
                                           void *cbdata);

PRTE_EXPORT extern int prte_pmix_server_register_tool(pmix_nspace_t nspace);

PRTE_EXPORT extern int pmix_server_cache_job_info(prte_job_t *jdata, pmix_info_t *info);

#if PMIX_NUMERIC_VERSION >= 0x00050000
PRTE_EXPORT extern pmix_status_t
pmix_server_session_ctrl_fn(const pmix_proc_t *requestor,
                            uint32_t sessionID,
                            const pmix_info_t directives[], size_t ndirs,
                            pmix_info_cbfunc_t cbfunc, void *cbdata);
#endif

/* exposed shared variables */
typedef struct {
    pmix_list_item_t super;
    char *name;
    pmix_proc_t *members;
    size_t num_members;
} pmix_server_pset_t;
PMIX_CLASS_DECLARATION(pmix_server_pset_t);

typedef struct {
    bool initialized;
    int verbosity;
    int output;
    pmix_pointer_array_t remote_reqs;
    pmix_pointer_array_t local_reqs;
    int timeout;
    bool wait_for_server;
    pmix_proc_t server;
    pmix_list_t notifications;
    bool pubsub_init;
    bool session_server;
    bool system_server;
    bool system_controller;
    bool scheduler_connected;
    pmix_proc_t scheduler;
    bool scheduler_set_as_server;
    char *report_uri;
    char *singleton;
    pmix_device_type_t generate_dist;
    pmix_list_t tools;
    pmix_list_t psets;
    pmix_list_t groups;
} pmix_server_globals_t;

extern pmix_server_globals_t prte_pmix_server_globals;

END_C_DECLS

#endif /* PMIX_SERVER_INTERNAL_H_ */
