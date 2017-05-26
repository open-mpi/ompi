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

#include "src/mca/bfrops/bfrops.h"
#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_list.h"
#include "src/class/pmix_ring_buffer.h"
#include "src/event/pmix_event.h"

#include "src/mca/gds/gds.h"
#include "src/mca/psec/psec.h"
#include "src/mca/ptl/ptl.h"

BEGIN_C_DECLS

/* some limits */
#define PMIX_MAX_CRED_SIZE      131072              // set max at 128kbytes
#define PMIX_MAX_ERR_CONSTANT   INT_MIN


/* define an internal-only process name that has
 * a dynamically-sized nspace field to save memory */
typedef struct {
    char *nspace;
    pmix_rank_t rank;
} pmix_name_t;

/* define an internal-only object for creating
 * lists of names */
typedef struct {
    pmix_list_item_t super;
    pmix_name_t *pname;
} pmix_namelist_t;
PMIX_CLASS_DECLARATION(pmix_namelist_t);

/* define a command type for communicating to the
 * pmix server */
typedef uint32_t pmix_cmd_t;
#define PMIX_CMD PMIX_UINT32

/* define some commands */
#define PMIX_REQ_CMD             0
#define PMIX_ABORT_CMD           1
#define PMIX_COMMIT_CMD          2
#define PMIX_FENCENB_CMD         3
#define PMIX_GETNB_CMD           4
#define PMIX_FINALIZE_CMD        5
#define PMIX_PUBLISHNB_CMD       6
#define PMIX_LOOKUPNB_CMD        7
#define PMIX_UNPUBLISHNB_CMD     8
#define PMIX_SPAWNNB_CMD         9
#define PMIX_CONNECTNB_CMD      10
#define PMIX_DISCONNECTNB_CMD   11
#define PMIX_NOTIFY_CMD         12
#define PMIX_REGEVENTS_CMD      13
#define PMIX_DEREGEVENTS_CMD    14
#define PMIX_QUERY_CMD          15
#define PMIX_LOG_CMD            16
#define PMIX_ALLOC_CMD          17
#define PMIX_JOB_CONTROL_CMD    18
#define PMIX_MONITOR_CMD        19

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


/****    PEER STRUCTURES    ****/

/* clients can only talk to their server, and servers are
 * assumed to all have the same personality. Thus, each
 * process only needs to track a single set of personality
 * modules. All interactions between a client and its local
 * server, or between servers, are done thru these modules */
typedef struct pmix_personality_t {
    pmix_bfrop_buffer_type_t type;
    pmix_bfrops_module_t *bfrops;
    pmix_psec_module_t *psec;
    pmix_ptl_module_t *ptl;
    pmix_gds_base_module_t *gds;
} pmix_personality_t;

/* objects used by servers for tracking active nspaces */
typedef struct {
    pmix_list_item_t super;
    char *nspace;
    size_t nlocalprocs;
    bool all_registered;         // all local ranks have been defined
    pmix_info_t *jobinfo;        // copy of the job-level info to be delivered to each proc
    size_t njobinfo;
    pmix_buffer_t *jobbkt;       // packed version of jobinfo
    size_t ndelivered;           // count of #local clients that have received the jobinfo
    pmix_list_t ranks;           // list of pmix_rank_info_t for connection support of my clients
    /* all members of an nspace are required to have the
     * same personality, but it can differ between nspaces.
     * Since servers may support clients from multiple nspaces,
     * track their respective compatibility modules here */
    pmix_personality_t compat;
} pmix_nspace_t;
PMIX_CLASS_DECLARATION(pmix_nspace_t);

/* define a caddy for quickly creating a list of pmix_nspace_t
 * objects for local, dedicated purposes */
typedef struct {
    pmix_list_item_t super;
    pmix_nspace_t *ns;
} pmix_nspace_caddy_t;
PMIX_CLASS_DECLARATION(pmix_nspace_caddy_t);

typedef struct pmix_rank_info_t {
    pmix_list_item_t super;
    int peerid;                 // peer object index into the local clients array on the server
    pmix_name_t pname;
    uid_t uid;
    gid_t gid;
    bool modex_recvd;
    int proc_cnt;              // #clones of this rank we know about
    void *server_object;       // pointer to rank-specific object provided by server
} pmix_rank_info_t;
PMIX_CLASS_DECLARATION(pmix_rank_info_t);

/* object for tracking peers - each peer can have multiple
 * connections. This can occur if the initial app executes
 * a fork/exec, and the child initiates its own connection
 * back to the PMIx server. Thus, the trackers should be "indexed"
 * by the socket, not the process nspace/rank */
typedef struct pmix_peer_t {
    pmix_object_t super;
    pmix_nspace_t *nptr;            // point to the nspace object for this process
    pmix_rank_info_t *info;
    int proc_cnt;
    int index;                      // index into the local clients array on the server
    int sd;
    bool finalized;                 // peer has called finalize
    pmix_event_t send_event;        /**< registration with event thread for send events */
    bool send_ev_active;
    pmix_event_t recv_event;        /**< registration with event thread for recv events */
    bool recv_ev_active;
    pmix_list_t send_queue;         /**< list of messages to send */
    pmix_ptl_send_t *send_msg;      /**< current send in progress */
    pmix_ptl_recv_t *recv_msg;      /**< current recv in progress */
} pmix_peer_t;
PMIX_CLASS_DECLARATION(pmix_peer_t);


/* define an object for moving a send
 * request into the server's event base
 * - instanced in pmix_server_ops.c */
typedef struct {
    pmix_list_item_t super;
    pmix_ptl_hdr_t hdr;
    pmix_peer_t *peer;
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

/* define a tracker for collective operations
 * - instanced in pmix_server_ops.c */
typedef struct {
    pmix_list_item_t super;
    pmix_cmd_t type;
    bool hybrid;                    // true if participating procs are from more than one nspace
    pmix_proc_t *pcs;               // copy of the original array of participants
    size_t   npcs;                  // number of procs in the array
    volatile bool active;           // flag for waiting for completion
    bool def_complete;              // all local procs have been registered and the trk definition is complete
    pmix_list_t local_cbs;          // list of pmix_server_caddy_t for sending result to the local participants
                                    //    Note: there may be multiple entries for a given proc if that proc
                                    //    has fork/exec'd clones that are also participating
    uint32_t nlocal;                // number of local participants
    uint32_t local_cnt;             // number of local participants who have contributed
    pmix_info_t *info;              // array of info structs
    size_t ninfo;                   // number of info structs in array
    pmix_collect_t collect_type;    // whether or not data is to be returned at completion
    pmix_modex_cbfunc_t modexcbfunc;
    pmix_op_cbfunc_t op_cbfunc;
} pmix_server_trkr_t;
PMIX_CLASS_DECLARATION(pmix_server_trkr_t);

/****    THREAD-RELATED    ****/
 /* define a caddy for thread-shifting operations */
 typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    volatile bool active;
    pmix_status_t status;
    pmix_status_t *codes;
    size_t ncodes;
    pmix_name_t pname;
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
    } cbfunc;
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
    union {
        pmix_ptl_cbfunc_t ptlfn;
        pmix_op_cbfunc_t opfn;
        pmix_value_cbfunc_t valuefn;
        pmix_lookup_cbfunc_t lookupfn;
        pmix_spawn_cbfunc_t spawnfn;
        pmix_evhdlr_reg_cbfunc_t errregfn;
    } cbfunc;
    size_t errhandler_ref;
    void *cbdata;
    pmix_name_t pname;
    char *key;
    pmix_value_t *value;
    pmix_proc_t *proc;
    pmix_proc_t *procs;
    size_t nprocs;
    pmix_info_t *info;
    size_t ninfo;
    size_t nvals;
    pmix_list_t kvs;
    bool copy;
    bool timer_running;
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
 * between various parts of the code library. The client, tool,
 * and server libraries must instance this structure */
typedef struct {
    int init_cntr;                      // #times someone called Init - #times called Finalize
    pmix_proc_t myid;
    pmix_peer_t *mypeer;                // my own peer object
    pmix_proc_type_t proc_type;
    uid_t uid;                          // my effective uid
    gid_t gid;                          // my effective gid
    int pindex;
    pmix_event_base_t *evbase;
    bool external_evbase;
    int debug_output;
    pmix_events_t events;               // my event handler registrations.
    bool connected;
    bool commits_pending;
    pmix_ring_buffer_t notifications;   // ring buffer of pending notifications
    /* processes also need a place where they can store
     * their own internal data - e.g., data provided by
     * the user via the store_internal interface, as well
     * as caching their own data obtained thru the "put"
     * interface so that other parts of the process can
     * look them up */
    pmix_gds_base_module_t *mygds;
} pmix_globals_t;


PMIX_EXPORT extern pmix_globals_t pmix_globals;

END_C_DECLS

#endif /* PMIX_GLOBALS_H */
