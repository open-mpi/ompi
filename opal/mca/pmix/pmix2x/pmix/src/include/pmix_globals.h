/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_GLOBALS_H
#define PMIX_GLOBALS_H

#include <src/include/pmix_config.h>

#include <src/include/types.h>

#include <unistd.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include PMIX_EVENT_HEADER

#include <pmix_common.h>

#include "src/buffer_ops/types.h"
#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_list.h"
#include "src/event/pmix_event.h"

#include "src/mca/psec/psec.h"
#include "src/mca/ptl/ptl.h"

BEGIN_C_DECLS

/* some limits */
#define PMIX_MAX_CRED_SIZE      131072              // set max at 128kbytes
#define PMIX_MAX_ERR_CONSTANT   INT_MIN


/****   ENUM DEFINITIONS    ****/
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
    PMIX_QUERY_CMD,
    PMIX_LOG_CMD,
    PMIX_ALLOC_CMD,
    PMIX_JOB_CONTROL_CMD,
    PMIX_MONITOR_CMD
} pmix_cmd_t;

/* provide a "pretty-print" function for cmds */
const char* pmix_command_string(pmix_cmd_t cmd);

/* define a set of flags to direct collection
 * of data during operations */
typedef enum {
    PMIX_COLLECT_INVALID = -1,
    PMIX_COLLECT_NO,
    PMIX_COLLECT_YES,
    PMIX_COLLECT_MAX
} pmix_collect_t;

/* define a process type */
typedef enum {
    PMIX_PROC_UNDEF,
    PMIX_PROC_CLIENT,
    PMIX_PROC_SERVER,
    PMIX_PROC_TOOL
} pmix_proc_type_t;

/* defins some convenience macros for testing proc type */
#define PMIX_PROC_IS_CLIENT     (PMIX_PROC_CLIENT == pmix_globals.proc_type)
#define PMIX_PROC_IS_SERVER     (PMIX_PROC_SERVER == pmix_globals.proc_type)
#define PMIX_PROC_IS_TOOL       (PMIX_PROC_TOOL == pmix_globals.proc_type)


/* internally used object for transferring data
 * to/from the server and for storing in the
 * hash tables */
typedef struct {
    pmix_list_item_t super;
    char *key;
    pmix_value_t *value;
} pmix_kval_t;
PMIX_CLASS_DECLARATION(pmix_kval_t);

// forward declaration
struct pmix_peer_t;

/****    PEER STRUCTURES    ****/
/* objects for tracking active nspaces */
typedef struct {
    pmix_object_t super;
    size_t nlocalprocs;
    bool all_registered;         // all local ranks have been defined
    pmix_buffer_t job_info;      // packed copy of the job-level info to be delivered to each proc
    pmix_list_t ranks;           // list of pmix_rank_info_t for connection support of my clients
    pmix_hash_table_t mylocal;   // hash_table for storing data PUT with local/global scope by my clients
    pmix_hash_table_t myremote;  // hash_table for storing data PUT with remote/global scope by my clients
    pmix_hash_table_t remote;    // hash_table for storing data PUT with remote/global scope recvd from remote clients via modex
} pmix_server_nspace_t;
PMIX_CLASS_DECLARATION(pmix_server_nspace_t);

typedef struct {
    pmix_list_item_t super;
    char nspace[PMIX_MAX_NSLEN+1];
    pmix_list_t nodes;               // list of pmix_nrec_t nodes that house procs in this nspace
    pmix_hash_table_t internal;      // hash_table for storing job-level/internal data related to this nspace
    pmix_hash_table_t modex;         // hash_table of received modex data
    pmix_server_nspace_t *server;    // isolate these so the client doesn't instantiate them
} pmix_nspace_t;
PMIX_CLASS_DECLARATION(pmix_nspace_t);

typedef struct pmix_rank_info_t {
    pmix_list_item_t super;
    pmix_nspace_t *nptr;
    pmix_rank_t rank;
    uid_t uid;
    gid_t gid;
    bool modex_recvd;
    int proc_cnt;              // #clones of this rank we know about
    void *server_object;       // pointer to rank-specific object provided by server
} pmix_rank_info_t;
PMIX_CLASS_DECLARATION(pmix_rank_info_t);

/* define a structure for holding personality pointers
 * to plugins for cross-version support */
typedef struct pmix_personality_t {
    pmix_psec_module_t *psec;
    pmix_ptl_module_t *ptl;
} pmix_personality_t;

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
    pmix_event_t send_event;        /**< registration with event thread for send events */
    bool send_ev_active;
    pmix_event_t recv_event;        /**< registration with event thread for recv events */
    bool recv_ev_active;
    pmix_list_t send_queue;         /**< list of messages to send */
    pmix_ptl_send_t *send_msg;      /**< current send in progress */
    pmix_ptl_recv_t *recv_msg;      /**< current recv in progress */
    pmix_personality_t compat;
} pmix_peer_t;
PMIX_CLASS_DECLARATION(pmix_peer_t);


typedef struct {
    pmix_list_item_t super;
    char *name;              // name of the node
    char *procs;             // comma-separated list of proc ranks on that node
} pmix_nrec_t;
PMIX_CLASS_DECLARATION(pmix_nrec_t);

/* define an object for moving a send
 * request into the server's event base */
typedef struct {
    pmix_object_t super;
    int sd;
} pmix_snd_caddy_t;
PMIX_CLASS_DECLARATION(pmix_snd_caddy_t);

/* define an object for moving a send
 * request into the server's event base */
typedef struct {
    pmix_list_item_t super;
    pmix_ptl_hdr_t hdr;
    pmix_peer_t *peer;
    pmix_snd_caddy_t snd;
} pmix_server_caddy_t;
PMIX_CLASS_DECLARATION(pmix_server_caddy_t);

/* caddy for query requests */
typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    volatile bool active;
    pmix_status_t status;
    pmix_query_t *queries;
    size_t nqueries;
    pmix_proc_t *targets;
    size_t ntargets;
    pmix_info_t *info;
    size_t ninfo;
    pmix_info_cbfunc_t cbfunc;
    pmix_release_cbfunc_t relcbfunc;
    void *cbdata;
} pmix_query_caddy_t;
PMIX_CLASS_DECLARATION(pmix_query_caddy_t);

/* define a tracker for collective operations */
typedef struct {
    pmix_list_item_t super;
    pmix_cmd_t type;
    pmix_proc_t *pcs;               // copy of the original array of participants
    size_t   npcs;                  // number of procs in the array
    volatile bool active;           // flag for waiting for completion
    bool def_complete;              // all local procs have been registered and the trk definition is complete
    pmix_list_t ranks;              // list of pmix_rank_info_t of the local participants
    pmix_list_t local_cbs;          // list of pmix_server_caddy_t for sending result to the local participants
    uint32_t nlocal;                // number of local participants
    uint32_t local_cnt;             // number of local participants who have contributed
    pmix_info_t *info;              // array of info structs
    size_t ninfo;                   // number of info structs in array
    pmix_collect_t collect_type;    // whether or not data is to be returned at completion
    pmix_modex_cbfunc_t modexcbfunc;
    pmix_op_cbfunc_t op_cbfunc;
} pmix_server_trkr_t;
PMIX_CLASS_DECLARATION(pmix_server_trkr_t);

typedef int (*pmix_store_dstor_cbfunc_t)(const char *nsname,
                                         pmix_rank_t rank, pmix_kval_t *kv);
typedef int (*pmix_store_hash_cbfunc_t)(pmix_hash_table_t *table,
                                         pmix_rank_t rank, pmix_kval_t *kv);

typedef struct {
    pmix_object_t super;
    pmix_nspace_t *nsptr;
    pmix_buffer_t *job_data;
    pmix_store_dstor_cbfunc_t dstore_fn;
    pmix_store_hash_cbfunc_t hstore_fn;
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    /* array of buffers per rank */
    pmix_value_array_t *bufs;
#endif
} pmix_job_data_caddy_t;
PMIX_CLASS_DECLARATION(pmix_job_data_caddy_t);

/****    THREAD-RELATED    ****/
 /* define a caddy for thread-shifting operations */
 typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    volatile bool active;
    pmix_status_t status;
    pmix_status_t *codes;
    size_t ncodes;
    const char *nspace;
    pmix_rank_t rank;
    const char *data;
    size_t ndata;
    const char *key;
    pmix_info_t *info;
    size_t ninfo;
    pmix_info_t *directives;
    size_t ndirs;
    pmix_notification_fn_t evhdlr;
    pmix_kval_t *kv;
    pmix_value_t *vptr;
    pmix_server_caddy_t *cd;
    pmix_server_trkr_t *tracker;
    bool enviro;
    union {
       pmix_release_cbfunc_t relfn;
       pmix_evhdlr_reg_cbfunc_t evregcbfn;
       pmix_op_cbfunc_t opcbfn;
       pmix_evhdlr_reg_cbfunc_t errregcbfn;
    }cbfunc;
    void *cbdata;
    size_t ref;
 } pmix_shift_caddy_t;
PMIX_CLASS_DECLARATION(pmix_shift_caddy_t);

/* struct for tracking ops */
typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    volatile bool active;
    bool checked;
    int status;
    pmix_status_t pstatus;
    pmix_scope_t scope;
    pmix_buffer_t data;
    pmix_ptl_cbfunc_t cbfunc;
    pmix_op_cbfunc_t op_cbfunc;
    pmix_value_cbfunc_t value_cbfunc;
    pmix_lookup_cbfunc_t lookup_cbfunc;
    pmix_spawn_cbfunc_t spawn_cbfunc;
    pmix_evhdlr_reg_cbfunc_t errreg_cbfunc;
    size_t errhandler_ref;
    void *cbdata;
    char nspace[PMIX_MAX_NSLEN+1];
    pmix_rank_t rank;
    char *key;
    pmix_value_t *value;
    pmix_proc_t *procs;
    pmix_info_t *info;
    size_t ninfo;
    size_t nvals;
} pmix_cb_t;
PMIX_CLASS_DECLARATION(pmix_cb_t);

/* define a very simple caddy for dealing with pmix_info_t
 * objects when transferring portions of arrays */
typedef struct {
    pmix_list_item_t super;
    pmix_info_t *info;
} pmix_info_caddy_t;
PMIX_CLASS_DECLARATION(pmix_info_caddy_t);

#define PMIX_THREADSHIFT(r, c)                              \
 do {                                                       \
    (r)->active = true;                                     \
    pmix_event_assign(&((r)->ev), pmix_globals.evbase,      \
                      -1, EV_WRITE, (c), (r));              \
    pmix_event_active(&((r)->ev), EV_WRITE, 1);             \
} while (0)


#define PMIX_WAIT_FOR_COMPLETION(a)             \
    do {                                        \
        while ((a)) {                           \
            usleep(10);                         \
        }                                       \
    } while (0)


/****    GLOBAL STORAGE    ****/
/* define a global construct that includes values that must be shared
 * between various parts of the code library. Both the client
 * and server libraries must instance this structure */
typedef struct {
    int init_cntr;                       // #times someone called Init - #times called Finalize
    pmix_proc_t myid;
    pmix_peer_t *mypeer;                 // my own peer object
    pmix_proc_type_t proc_type;
    uid_t uid;                           // my effective uid
    gid_t gid;                           // my effective gid
    int pindex;
    pmix_event_base_t *evbase;
    bool external_evbase;
    int debug_output;
    pmix_events_t events;                // my event handler registrations.
    bool connected;
    pmix_list_t nspaces;                 // list of pmix_nspace_t for the nspaces we know about
    pmix_buffer_t *cache_local;          // data PUT by me to local scope
    pmix_buffer_t *cache_remote;         // data PUT by me to remote scope
} pmix_globals_t;


PMIX_EXPORT extern pmix_globals_t pmix_globals;

END_C_DECLS

#endif /* PMIX_GLOBALS_H */
