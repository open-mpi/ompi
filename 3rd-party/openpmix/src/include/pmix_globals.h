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
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2019      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_GLOBALS_H
#define PMIX_GLOBALS_H

#include "src/include/pmix_config.h"
#include "src/include/pmix_types.h"

#include <unistd.h>
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#include <event.h>

#include "pmix.h"
#include "pmix_common.h"
#include "pmix_tool.h"

#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_hotel.h"
#include "src/class/pmix_list.h"
#include "src/event/pmix_event.h"
#include "src/runtime/pmix_init_util.h"
#include "src/threads/pmix_threads.h"

#include "src/mca/bfrops/bfrops.h"
#include "src/mca/gds/gds.h"
#include "src/mca/psec/psec.h"
#include "src/mca/ptl/ptl.h"

#include "src/util/pmix_name_fns.h"

BEGIN_C_DECLS

/* some limits */
#define PMIX_MAX_CRED_SIZE    131072 // set max at 128kbytes
#define PMIX_MAX_ERR_CONSTANT INT_MIN

/* internal-only attributes */
#define PMIX_BFROPS_MODULE \
    "pmix.bfrops.mod" // (char*) name of bfrops plugin in-use by a given nspace
#define PMIX_PNET_SETUP_APP \
    "pmix.pnet.setapp" // (pmix_byte_object_t) blob containing info to be given to
                       //      pnet framework on remote nodes

// define some bit handling macros
#define PMIX_SET_BIT(a, f) \
    (a) |= (f)

#define PMIX_UNSET_BIT(a, f) \
    (a) &= ~(f)

#define PMIX_CHECK_BIT_IS_SET(a, f) \
    ((a) & (f))

#define PMIX_CHECK_BIT_NOT_SET(a, f) \
    !PMIX_CHECK_BIT_IS_SET(a, f)

#define PMIX_INFO_OP_COMPLETE       0x80000000
#define PMIX_INFO_OP_COMPLETED(m) \
      PMIX_SET_BIT((m)->flags, PMIX_INFO_OP_COMPLETE)
#define PMIX_INFO_OP_IS_COMPLETE(m) \
      PMIX_CHECK_BIT_IS_SET((m)->flags, PMIX_INFO_OP_COMPLETE)


/* define an internal-only object for creating
 * lists of names */
typedef struct {
    pmix_list_item_t super;
    pmix_name_t *pname;
} pmix_namelist_t;
PMIX_CLASS_DECLARATION(pmix_namelist_t);

/* define a struct for holding entries in the
 * dictionary of attributes */
typedef struct {
    uint32_t index;
    char *name;
    char *string;
    pmix_data_type_t type;
    char **description;
} pmix_regattr_input_t;
#define PMIX_REGATTR_INPUT_NEW(a, i, n, s, t, d)                            \
do {                                                                        \
    (a) = (pmix_regattr_input_t*)pmix_malloc(sizeof(pmix_regattr_input_t)); \
    if (NULL != (a)) {                                                      \
        memset((a), 0, sizeof(pmix_regattr_input_t));                       \
        (a)->index = (i);                                                   \
        if (NULL != (n)) {                                                    \
            (a)->name = strdup((n));                                       \
        }                                                                   \
        if (NULL != (s)) {                                                    \
            (a)->string = strdup((s));                                        \
        }                                                                   \
        (a)->type = (t);                                                    \
        if (NULL != (d)) {                                                    \
            (a)->description = PMIx_Argv_copy((d));                           \
        }                                                                   \
    }                                                                       \
} while(0)
#define PMIX_REGATTR_INPUT_FREE(a)      \
do {                                    \
    if (NULL != (a)) {                  \
        if (NULL != (a)->name) {        \
            free((a)->name);            \
        }                               \
        if (NULL != (a)->string)        \
            free((a)->string);          \
        }                               \
        if (NULL != (a)->description) { \
            free((a)->description);     \
        }                               \
    }                                   \
} while(0)

/* define a struct for holding entries in the
 * dictionary of event strings */
typedef struct {
    uint32_t index;
    char *name;
    int32_t code;
} pmix_event_string_t;

/* define a struct for storing data in memory */
typedef struct {
    uint32_t index;
    pmix_value_t *value;
} pmix_qual_t;
#define PMIX_QUAL_NEW(d, k)                                 \
do {                                                        \
    (d) = (pmix_qual_t*)pmix_malloc(sizeof(pmix_qual_t));   \
    if (NULL != (d)) {                                      \
        (d)->index = k;                                     \
        (d)->value = NULL;                                  \
    }                                                       \
} while(0)
#define PMIX_QUAL_RELEASE(d)            \
do {                                    \
    if (NULL != (d)->value) {           \
        PMIX_VALUE_RELEASE((d)->value); \
    }                                   \
} while(0)

typedef struct {
    uint32_t index;
    uint32_t qualindex;
    pmix_value_t *value;
} pmix_dstor_t;

PMIX_EXPORT pmix_dstor_t *
pmix_dstor_new_tma(
    uint32_t index,
    pmix_tma_t *tma
);

PMIX_EXPORT void
pmix_dstor_release_tma(
    pmix_dstor_t *d,
    pmix_tma_t *tma
);

#define PMIX_DSTOR_NEW(d, k)                                \
do {                                                        \
    (d) = pmix_dstor_new_tma((k), NULL);                    \
} while(0)

#define PMIX_DSTOR_RELEASE(d)           \
do {                                    \
    pmix_dstor_release_tma((d), NULL);  \
} while(0)

/* define a struct for passing topology objects */
typedef struct {
    pmix_object_t super;
    char *source;
    void *object;
} pmix_topo_obj_t;
PMIX_CLASS_DECLARATION(pmix_topo_obj_t);

/* define a command type for communicating to the
 * pmix server */
typedef uint8_t pmix_cmd_t;

/* define some commands */
#define PMIX_REQ_CMD                      0
#define PMIX_ABORT_CMD                    1
#define PMIX_COMMIT_CMD                   2
#define PMIX_FENCENB_CMD                  3
#define PMIX_GETNB_CMD                    4
#define PMIX_FINALIZE_CMD                 5
#define PMIX_PUBLISHNB_CMD                6
#define PMIX_LOOKUPNB_CMD                 7
#define PMIX_UNPUBLISHNB_CMD              8
#define PMIX_SPAWNNB_CMD                  9
#define PMIX_CONNECTNB_CMD                10
#define PMIX_DISCONNECTNB_CMD             11
#define PMIX_NOTIFY_CMD                   12
#define PMIX_REGEVENTS_CMD                13
#define PMIX_DEREGEVENTS_CMD              14
#define PMIX_QUERY_CMD                    15
#define PMIX_LOG_CMD                      16
#define PMIX_ALLOC_CMD                    17
#define PMIX_JOB_CONTROL_CMD              18
#define PMIX_MONITOR_CMD                  19
#define PMIX_GET_CREDENTIAL_CMD           20
#define PMIX_VALIDATE_CRED_CMD            21
#define PMIX_IOF_PULL_CMD                 22
#define PMIX_IOF_PUSH_CMD                 23
#define PMIX_GROUP_CONSTRUCT_CMD          24
#define PMIX_GROUP_JOIN_CMD               25
#define PMIX_GROUP_INVITE_CMD             26
#define PMIX_GROUP_LEAVE_CMD              27
#define PMIX_GROUP_DESTRUCT_CMD           28
#define PMIX_IOF_DEREG_CMD                29
#define PMIX_FABRIC_REGISTER_CMD          30
#define PMIX_FABRIC_UPDATE_CMD            31
#define PMIX_COMPUTE_DEVICE_DISTANCES_CMD 32
#define PMIX_REFRESH_CACHE                33

/* provide a "pretty-print" function for cmds */
const char *pmix_command_string(pmix_cmd_t cmd);

/* provide a hook to init tool data */
PMIX_EXPORT extern pmix_status_t pmix_tool_init_info(void);

/* define a set of flags to direct collection
 * of data during operations */
typedef enum {
    PMIX_COLLECT_INVALID = -1,
    PMIX_COLLECT_NO,
    PMIX_COLLECT_YES,
    PMIX_COLLECT_MAX
} pmix_collect_t;

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
    pmix_gds_base_module_t *gds;
} pmix_personality_t;

/* define a set of structs for tracking post-termination cleanup */
typedef struct pmix_epilog_t {
    uid_t uid;
    gid_t gid;
    pmix_list_t cleanup_dirs;
    pmix_list_t cleanup_files;
    pmix_list_t ignores;
} pmix_epilog_t;

typedef struct {
    pmix_list_item_t super;
    char *path;
} pmix_cleanup_file_t;
PMIX_CLASS_DECLARATION(pmix_cleanup_file_t);

typedef struct {
    pmix_list_item_t super;
    char *path;
    bool recurse;
    bool leave_topdir;
} pmix_cleanup_dir_t;
PMIX_CLASS_DECLARATION(pmix_cleanup_dir_t);

/* define a struct to hold booleans controlling the
 * format/contents of the output */
typedef struct {
    bool set;
    bool xml;
    bool timestamp;
    bool tag;
    bool tag_detailed;
    bool tag_fullname;
    bool rank;
    char *file;
    char *directory;
    bool nocopy;
    bool merge;
    bool local_output;
    bool local_output_given;
    bool pattern;
    bool raw;
} pmix_iof_flags_t;

#define PMIX_IOF_FLAGS_STATIC_INIT  \
{                                   \
    .set = false,                   \
    .xml = false,                   \
    .timestamp = false,             \
    .tag = false,                   \
    .tag_detailed = false,          \
    .tag_fullname = false,          \
    .rank = false,                  \
    .file = NULL,                   \
    .directory = NULL,              \
    .nocopy = false,                \
    .merge = false,                 \
    .local_output = false,          \
    .local_output_given = false,    \
    .pattern = false,               \
    .raw = false                    \
}

/* objects used by servers for tracking active nspaces */
typedef struct {
    pmix_list_item_t super;
    char *nspace;
    struct {
        uint8_t major;
        uint8_t minor;
        uint8_t release;
    } version;
    pmix_rank_t nprocs; // num procs in this nspace
    size_t nlocalprocs;
    size_t num_waiting;    // number of local procs waiting for debugger attach/release
    bool all_registered;   // all local ranks have been defined
    bool version_stored;   // the version string used by this nspace has been stored
    pmix_buffer_t *jobbkt; // packed version of jobinfo
    size_t ndelivered;     // count of #local clients that have received the jobinfo
    size_t nfinalized;     // count of #local clients that have finalized
    pmix_list_t ranks;     // list of pmix_rank_info_t for connection support of my clients
    /* all members of an nspace are required to have the
     * same personality, but it can differ between nspaces.
     * Since servers may support clients from multiple nspaces,
     * track their respective compatibility modules here */
    pmix_personality_t compat;
    pmix_epilog_t epilog;   // things to do upon termination of all local clients
                            // from this nspace
    pmix_list_t setup_data; // list of pmix_kval_t containing info structs having blobs
                            // for setting up the local node for this nspace/application
    pmix_iof_flags_t iof_flags;   // output formatting flags
    pmix_list_t sinks;   // IOF write events for output to files or directories
} pmix_namespace_t;
PMIX_CLASS_DECLARATION(pmix_namespace_t);

/* define a caddy for quickly creating a list of pmix_namespace_t
 * objects for local, dedicated purposes */
typedef struct {
    pmix_list_item_t super;
    pmix_namespace_t *ns;
} pmix_nspace_caddy_t;
PMIX_CLASS_DECLARATION(pmix_nspace_caddy_t);

typedef struct {
    pmix_list_item_t super;
    pmix_namespace_t *ns;
    pmix_list_t envars;
} pmix_nspace_env_cache_t;
PMIX_CLASS_DECLARATION(pmix_nspace_env_cache_t);

typedef struct {
    pmix_list_item_t super;
    pmix_envar_t envar;
} pmix_envar_list_item_t;
PMIX_CLASS_DECLARATION(pmix_envar_list_item_t);


typedef struct pmix_rank_info_t {
    pmix_list_item_t super;
    int peerid; // peer object index into the local clients array on the server
    pmix_name_t pname;
    uid_t uid;
    gid_t gid;
    bool modex_recvd;
    int proc_cnt;        // #clones of this rank we know about
    void *server_object; // pointer to rank-specific object provided by server
} pmix_rank_info_t;
PMIX_CLASS_DECLARATION(pmix_rank_info_t);

/* define a very simple caddy for dealing with pmix_info_t
 * and pmix_query_t objects when transferring portions of arrays */
typedef struct {
    pmix_list_item_t super;
    pmix_info_t *info;
    size_t ninfo;
} pmix_info_caddy_t;
PMIX_CLASS_DECLARATION(pmix_info_caddy_t);

typedef struct {
    pmix_list_item_t super;
    pmix_info_t info;
} pmix_infolist_t;
PMIX_CLASS_DECLARATION(pmix_infolist_t);

typedef struct {
    pmix_list_item_t super;
    pmix_query_t query;
} pmix_querylist_t;
PMIX_CLASS_DECLARATION(pmix_querylist_t);

typedef struct {
    pmix_list_item_t super;
    pmix_proc_t proc;
} pmix_proclist_t;
PMIX_CLASS_DECLARATION(pmix_proclist_t);

/* object for tracking peers - each peer can have multiple
 * connections. This can occur if the initial app executes
 * a fork/exec, and the child initiates its own connection
 * back to the PMIx server. Thus, the trackers should be "indexed"
 * by the socket, not the process nspace/rank */
typedef struct pmix_peer_t {
    pmix_object_t super;
    pmix_namespace_t *nptr; // point to the nspace object for this process
    pmix_rank_info_t *info;
    pmix_proc_type_t proc_type;
    pmix_listener_protocol_t protocol;
    int proc_cnt;
    int index; // index into the local clients array on the server
    int sd;
    bool finalized;          // peer has called finalize
    pmix_event_t send_event; /**< registration with event thread for send events */
    bool send_ev_active;
    pmix_event_t recv_event; /**< registration with event thread for recv events */
    bool recv_ev_active;
    pmix_list_t send_queue;    /**< list of messages to send */
    pmix_ptl_send_t *send_msg; /**< current send in progress */
    pmix_ptl_recv_t *recv_msg; /**< current recv in progress */
    int commit_cnt;
    pmix_epilog_t epilog; /**< things to be performed upon
                               termination of this peer */
} pmix_peer_t;
PMIX_CLASS_DECLARATION(pmix_peer_t);

/* tracker for IOF requests */
typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    pmix_peer_t *requestor;
    size_t local_id;
    size_t remote_id;
    pmix_proc_t *procs;
    size_t nprocs;
    pmix_iof_channel_t channels;
    pmix_iof_cbfunc_t cbfunc;
    pmix_hdlr_reg_cbfunc_t regcbfunc;
    void *cbdata;
} pmix_iof_req_t;
PMIX_CLASS_DECLARATION(pmix_iof_req_t);

typedef void (*pmix_pstrg_query_cbfunc_t)(pmix_status_t status, pmix_list_t *results, void *cbdata);

/* caddy for query requests */
typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    pmix_lock_t lock;
    bool host_called;
    pmix_status_t status;
    pmix_query_t *queries;
    size_t nqueries;
    pmix_proc_t *targets;
    size_t ntargets;
    pmix_info_t *info;
    pmix_info_t *dirs;
    size_t ninfo;
    size_t ndirs;
    pmix_list_t results;
    size_t nreplies;
    size_t nrequests;
    pmix_byte_object_t bo;
    pmix_info_cbfunc_t cbfunc;
    pmix_value_cbfunc_t valcbfunc;
    pmix_release_cbfunc_t relcbfunc;
    pmix_credential_cbfunc_t credcbfunc;
    pmix_validation_cbfunc_t validcbfunc;
    pmix_pstrg_query_cbfunc_t stqcbfunc;
    void *cbdata;
} pmix_query_caddy_t;
PMIX_CLASS_DECLARATION(pmix_query_caddy_t);

typedef struct {
    pmix_list_item_t super;
    char *grpid;
    pmix_proc_t *members;
    size_t nmbrs;
} pmix_group_t;
PMIX_CLASS_DECLARATION(pmix_group_t);

typedef struct {
    pmix_list_item_t super;
    pmix_proc_t proc;
    pmix_byte_object_t blob;  // packed blob of info provided by this proc
} pmix_grpinfo_t;
PMIX_CLASS_DECLARATION(pmix_grpinfo_t);

/* define a tracker for collective operations
 * - instanced in pmix_server_ops.c */
typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    bool event_active;
    bool host_called; // tracker has been passed up to host
    bool local;       // operation is strictly local
    char *id;         // string identifier for the collective
    pmix_cmd_t type;
    pmix_proc_t pname;
    bool hybrid;            // true if participating procs are from more than one nspace
    pmix_proc_t *pcs;       // copy of the original array of participants
    size_t npcs;            // number of procs in the array
    pmix_list_t nslist;     // unique nspace list of participants
    pmix_lock_t lock;       // flag for waiting for completion
    bool def_complete;      // all local procs have been registered and the trk definition is complete
    pmix_list_t local_cbs;  // list of pmix_server_caddy_t for sending result to the local participants
                            //    Note: there may be multiple entries for a given proc if that proc
                            //    has fork/exec'd clones that are also participating
    uint32_t nlocal;        // number of local participants
    uint32_t local_cnt;     // number of local participants who have contributed
    pmix_info_t *info;      // array of info structs
    size_t ninfo;           // number of info structs in array
    pmix_list_t grpinfo;    // list of group info to be distributed
    int grpop;              // the group operation being tracked
    pmix_collect_t collect_type; // whether or not data is to be returned at completion
    pmix_modex_cbfunc_t modexcbfunc;
    pmix_op_cbfunc_t op_cbfunc;
    void *cbdata;
} pmix_server_trkr_t;
PMIX_CLASS_DECLARATION(pmix_server_trkr_t);

/* define an object for moving a send
 * request into the server's event base and
 * dealing with some request timeouts
 * - instanced in pmix_server_ops.c */
typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    bool event_active;
    pmix_server_trkr_t *trk;
    pmix_ptl_hdr_t hdr;
    pmix_peer_t *peer;
    pmix_info_t *info;
    size_t ninfo;
} pmix_server_caddy_t;
PMIX_CLASS_DECLARATION(pmix_server_caddy_t);

/****    THREAD-RELATED    ****/
/* define a caddy for thread-shifting operations */
typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    pmix_lock_t lock;
    pmix_status_t status;
    pmix_status_t *codes;
    size_t ncodes;
    uint32_t sessionid;
    pmix_name_t pname;
    pmix_proc_t *proc;
    pmix_peer_t *peer;
    const char *data;
    size_t ndata;
    const char *key;
    pmix_info_t *info;
    size_t ninfo;
    pmix_info_t *directives;
    size_t ndirs;
    pmix_notification_fn_t evhdlr;
    pmix_iof_req_t *iofreq;
    pmix_kval_t *kv;
    pmix_value_t *vptr;
    pmix_server_caddy_t *cd;
    pmix_server_trkr_t *tracker;
    bool enviro;
    union {
        pmix_release_cbfunc_t relfn;
        pmix_hdlr_reg_cbfunc_t hdlrregcbfn;
        pmix_op_cbfunc_t opcbfn;
        pmix_modex_cbfunc_t modexcbfunc;
        pmix_info_cbfunc_t infocbfunc;
    } cbfunc;
    void *cbdata;
    size_t ref;
} pmix_shift_caddy_t;
PMIX_CLASS_DECLARATION(pmix_shift_caddy_t);

typedef struct {
    pmix_object_t super;
    pmix_proc_t p;
    bool pntrval;
    bool stval;
    bool optional;
    bool immediate;
    bool add_immediate;
    bool refresh_cache;
    pmix_scope_t scope;
    bool sessioninfo;
    bool sessiondirective;
    uint32_t sessionid;
    bool nodeinfo;
    bool nodedirective;
    char *hostname;
    uint32_t nodeid;
    bool appinfo;
    bool appdirective;
    uint32_t appnum;
} pmix_get_logic_t;
PMIX_CLASS_DECLARATION(pmix_get_logic_t);

/* struct for tracking ops */
typedef struct {
    pmix_list_item_t super;
    pmix_event_t ev;
    pmix_lock_t lock;
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
        pmix_hdlr_reg_cbfunc_t hdlrregfn;
        pmix_info_cbfunc_t infofn;
        pmix_device_dist_cbfunc_t distfn;
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
    pmix_device_distance_t *dist;
    bool infocopy;
    size_t nvals;
    pmix_list_t kvs;
    bool copy;
    pmix_get_logic_t *lg;
    bool timer_running;
    pmix_fabric_t *fabric;
    pmix_topology_t *topo;
} pmix_cb_t;
PMIX_CLASS_DECLARATION(pmix_cb_t);

#define PMIX_THREADSHIFT(r, c)                                                      \
    do {                                                                            \
        pmix_event_assign(&((r)->ev), pmix_globals.evbase, -1, EV_WRITE, (c), (r)); \
        PMIX_POST_OBJECT((r));                                                      \
        pmix_event_active(&((r)->ev), EV_WRITE, 1);                                 \
    } while (0)

#define PMIX_THREADSHIFT_DELAY(r, c, t)                                  \
    do {                                                                 \
        struct timeval _tv = {0, 0};                                     \
        pmix_event_evtimer_set(pmix_globals.evbase, &(r)->ev, (c), (r)); \
        _tv.tv_sec = (int) (t);                                          \
        _tv.tv_usec = ((t) -_tv.tv_sec) * 1000000.0;                     \
        PMIX_POST_OBJECT((r));                                           \
        pmix_event_evtimer_add(&(r)->ev, &_tv);                          \
    } while (0)

typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    pmix_lock_t lock;
    /* timestamp receipt of the notification so we
     * can evict the oldest one if we get overwhelmed */
    time_t ts;
    /* what room of the hotel they are in */
    int room;
    pmix_status_t status;
    pmix_proc_t source;
    pmix_data_range_t range;
    /* For notification, we use the targets field to track
     * any custom range of procs that are to receive the
     * event.
     */
    pmix_proc_t *targets;
    size_t ntargets;
    size_t nleft; // number of targets left to be notified
    /* When generating a notification, the originator can
     * specify the range of procs affected by this event.
     * For example, when creating a JOB_TERMINATED event,
     * the RM can specify the nspace of the job that has
     * ended, thus allowing users to provide a different
     * callback object based on the nspace being monitored.
     * We use the "affected" field to track these values
     * when processing the event chain.
     */
    pmix_proc_t *affected;
    size_t naffected;
    /* track if the event generator stipulates that default
     * event handlers are/are not to be given the event */
    bool nondefault;
    /* carry along any other provided info so the individual
     * handlers can look at it */
    pmix_info_t *info;
    size_t ninfo;
    /* allow for a buffer to be carried across internal processing */
    pmix_buffer_t *buf;
    /* the final callback to be executed upon completion of the event */
    pmix_op_cbfunc_t cbfunc;
    void *cbdata;
} pmix_notify_caddy_t;
PMIX_CLASS_DECLARATION(pmix_notify_caddy_t);

typedef struct {
    pmix_object_t super;
    /** Points to key <--> index translation table. */
    pmix_pointer_array_t *table;
    /** Stores the next ID. */
    uint32_t next_id;
} pmix_keyindex_t;
PMIX_CLASS_DECLARATION(pmix_keyindex_t);

#define PMIX_KEYINDEX_STATIC_INIT                 \
{                                                 \
    .super = PMIX_OBJ_STATIC_INIT(pmix_object_t), \
    .table = NULL,                                \
    .next_id = PMIX_INDEX_BOUNDARY                \
}

/****    GLOBAL STORAGE    ****/
/* define a global construct that includes values that must be shared
 * between various parts of the code library. The client, tool,
 * and server libraries must instance this structure */
typedef struct {
    int init_cntr; // #times someone called Init - #times called Finalize
    pmix_proc_t myid;
    pmix_value_t myidval;
    pmix_value_t myrankval;
    pmix_peer_t *mypeer; // my own peer object
    uid_t uid;           // my effective uid
    gid_t gid;           // my effective gid
    char *hostname;      // my hostname
    uint32_t appnum;     // my appnum
    pid_t pid;           // my local pid
    uint32_t nodeid;     // my nodeid, if given
    uint32_t sessionid;  // my sessionid, if given
    int pindex;
    pmix_event_base_t *evbase;
    pmix_event_base_t *evauxbase;
    int debug_output;
    pmix_events_t events; // my event handler registrations.
    bool connected;
    bool commits_pending;
    struct timeval event_window;
    pmix_list_t cached_events;         // events waiting in the window prior to processing
    pmix_pointer_array_t iof_requests; // array of pmix_iof_req_t IOF requests
    int max_events;                    // size of the notifications hotel
    int event_eviction_time;           // max time to cache notifications
    pmix_hotel_t notifications;        // hotel of pending notifications
    /* IOF controls */
    bool pushstdin;
    pmix_list_t stdin_targets; // list of pmix_namelist_t
    bool tag_output;
    bool xml_output;
    bool timestamp_output;
    size_t output_limit;
    pmix_list_t nspaces;
    pmix_topology_t topology;
    pmix_cpuset_t cpuset;
    bool external_topology;
    bool external_progress;
    pmix_iof_flags_t iof_flags;
    pmix_keyindex_t keyindex;
} pmix_globals_t;

/* provide access to a function to cleanup epilogs */
PMIX_EXPORT void pmix_execute_epilog(pmix_epilog_t *ep);

PMIX_EXPORT pmix_status_t pmix_notify_event_cache(pmix_notify_caddy_t *cd);

PMIX_EXPORT extern pmix_globals_t pmix_globals;
PMIX_EXPORT extern pmix_lock_t pmix_global_lock;
PMIX_EXPORT extern const char* PMIX_PROXY_VERSION;
PMIX_EXPORT extern const char* PMIX_PROXY_BUGREPORT;

PMIX_EXPORT void pmix_log_local_op(int sd, short args, void *cbdata_);

static inline bool pmix_check_node_info(const char *key)
{
    char *keys[] = {
        PMIX_HOSTNAME,                  PMIX_HOSTNAME_ALIASES,
        PMIX_NODEID,                    PMIX_AVAIL_PHYS_MEMORY,
        PMIX_LOCAL_PEERS,               PMIX_LOCAL_PROCS,
        PMIX_LOCAL_CPUSETS,             PMIX_LOCAL_SIZE,
        PMIX_NODE_SIZE,                 PMIX_LOCALLDR,
        PMIX_NODE_OVERSUBSCRIBED,       PMIX_FABRIC_DEVICES,
        PMIX_FABRIC_COORDINATES,        PMIX_FABRIC_DEVICE,
        PMIX_FABRIC_DEVICE_INDEX,       PMIX_FABRIC_DEVICE_NAME,
        PMIX_FABRIC_DEVICE_VENDOR,      PMIX_FABRIC_DEVICE_BUS_TYPE,
        PMIX_FABRIC_DEVICE_VENDORID,    PMIX_FABRIC_DEVICE_DRIVER,
        PMIX_FABRIC_DEVICE_FIRMWARE,    PMIX_FABRIC_DEVICE_ADDRESS,
        PMIX_FABRIC_DEVICE_MTU,         PMIX_FABRIC_DEVICE_COORDINATES,
        PMIX_FABRIC_DEVICE_SPEED,       PMIX_FABRIC_DEVICE_STATE,
        PMIX_FABRIC_DEVICE_TYPE,        PMIX_FABRIC_DEVICE_PCI_DEVID,
        NULL
    };
    size_t n;

    for (n = 0; NULL != keys[n]; n++) {
        if (0 == strncmp(key, keys[n], PMIX_MAX_KEYLEN)) {
            return true;
        }
    }
    return false;
}

static inline bool pmix_check_app_info(const char *key)
{
    char *keys[] = {
        PMIX_APP_SIZE,  PMIX_APPLDR,       PMIX_APP_ARGV,      PMIX_WDIR,
        PMIX_PSET_NAME, PMIX_PSET_MEMBERS, PMIX_APP_MAP_TYPE,  PMIX_APP_MAP_REGEX,
        NULL
    };
    size_t n;

    for (n = 0; NULL != keys[n]; n++) {
        if (0 == strncmp(key, keys[n], PMIX_MAX_KEYLEN)) {
            return true;
        }
    }
    return false;
}

static inline bool pmix_check_session_info(const char *key)
{
    char *keys[] = {
        PMIX_SESSION_ID, PMIX_CLUSTER_ID,   PMIX_UNIV_SIZE,
        PMIX_TMPDIR,     PMIX_TDIR_RMCLEAN, PMIX_HOSTNAME_KEEP_FQDN,
        PMIX_RM_NAME,    PMIX_RM_VERSION,
        NULL
    };
    size_t n;

    for (n = 0; NULL != keys[n]; n++) {
        if (0 == strncmp(key, keys[n], PMIX_MAX_KEYLEN)) {
            return true;
        }
    }
    return false;
}

static inline bool pmix_check_special_key(const char *key)
{
    char *keys[] = {
        PMIX_GROUP_CONTEXT_ID,
        PMIX_GROUP_LOCAL_CID,
        NULL
    };
    size_t n;

    for (n = 0; NULL != keys[n]; n++) {
        if (0 == strncmp(key, keys[n], PMIX_MAX_KEYLEN)) {
            return true;
        }
    }
    return false;
}

#if PMIX_PICKY_COMPILERS
#define PMIX_HIDE_UNUSED_PARAMS(...)                \
    do {                                            \
        int __x = 3;                                \
        pmix_hide_unused_params(__x, __VA_ARGS__);  \
    } while(0)

PMIX_EXPORT void pmix_hide_unused_params(int x, ...);

#else
#define PMIX_HIDE_UNUSED_PARAMS(...)
#endif

#define PMIX_TRACE_KEY_ACTUAL(s, k, v)                  \
do {                                                    \
    if (0 == strcmp(s, k)) {                            \
        char *_v = PMIx_Value_string(v);                \
        pmix_output(0, "[%s:%s:%d] %s\n%s\n",           \
                    __FILE__, __func__, __LINE__,       \
                    PMIx_Get_attribute_name(k), _v);    \
        free(_v);                                       \
    }                                                   \
} while(0)

#define PMIX_TRACE_KEY(c, s, k, v)                          \
do {                                                        \
    if (0 == strcasecmp(c, "SERVER") &&                     \
        PMIX_PEER_IS_SERVER(pmix_globals.mypeer)) {         \
        PMIX_TRACE_KEY_ACTUAL(s, k, v);                     \
    } else if (0 == strcasecmp(c, "CLIENT") &&              \
           !PMIX_PEER_IS_SERVER(pmix_globals.mypeer)) {     \
           PMIX_TRACE_KEY_ACTUAL(s, k, v);                  \
    }                                                       \
} while (0)

END_C_DECLS

#endif /* PMIX_GLOBALS_H */
