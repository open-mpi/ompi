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
 * Copyright (c) 2014-2016 Intel, Inc. All rights reserved.
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

#include <pmix/pmix_common.h>

#include "src/buffer_ops/types.h"
#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_list.h"

BEGIN_C_DECLS

#define PMIX_MAX_CRED_SIZE  131072              // set max at 128kbytes
#define PMIX_MAX_ERROR_REGISTRATIONS    128     // maximum number of error handlers that can be registered

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
} pmix_cmd_t;

/* define a set of flags to direct collection
 * of data during operations */
typedef enum {
    PMIX_COLLECT_INVALID = -1,
    PMIX_COLLECT_NO,
    PMIX_COLLECT_YES,
    PMIX_COLLECT_MAX
} pmix_collect_t;


/****    MESSAGING STRUCTURES    ****/
/* header for messages */
typedef struct {
    int pindex;
    uint32_t tag;
    size_t nbytes;
} pmix_usock_hdr_t;

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
    int rank;
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


/* define a structure for tracking error registrations */
typedef struct {
    pmix_object_t super;
    bool sglhdlr;                      // registers a specific error status handler
    pmix_notification_fn_t errhandler; /* registered err handler callback fn */
    pmix_info_t *info;                 /* error info keys registered with the handler */
    size_t ninfo;                      /* size of info */
} pmix_error_reg_info_t;
PMIX_CLASS_DECLARATION(pmix_error_reg_info_t);

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
    pmix_usock_hdr_t hdr;
    pmix_peer_t *peer;
    pmix_snd_caddy_t snd;
} pmix_server_caddy_t;
PMIX_CLASS_DECLARATION(pmix_server_caddy_t);

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


/****    THREAD-RELATED    ****/
 /* define a caddy for thread-shifting operations */
 typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    volatile bool active;
    pmix_status_t status;
    const char *nspace;
    int rank;
    const char *data;
    size_t ndata;
    const char *key;
    pmix_info_t *info;
    size_t ninfo;
    pmix_notification_fn_t err;
    pmix_kval_t *kv;
    pmix_value_t *vptr;
    pmix_server_caddy_t *cd;
    pmix_server_trkr_t *tracker;
    union {
       pmix_release_cbfunc_t relfn;
       pmix_errhandler_reg_cbfunc_t errregcbfn;
       pmix_op_cbfunc_t opcbfn;
    }cbfunc;
    void *cbdata;
    int ref;
 } pmix_shift_caddy_t;
PMIX_CLASS_DECLARATION(pmix_shift_caddy_t);

#define PMIX_THREADSHIFT(r, c)                       \
 do {                                                 \
    (r)->active = true;                               \
    event_assign(&((r)->ev), pmix_globals.evbase,     \
                 -1, EV_WRITE, (c), (r));             \
    event_active(&((r)->ev), EV_WRITE, 1);            \
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
    uid_t uid;                           // my effective uid
    gid_t gid;                           // my effective gid
    int pindex;
    pmix_event_base_t *evbase;
    bool external_evbase;
    int debug_output;
    pmix_pointer_array_t errregs;        // my error handler registrations.
    bool server;
    bool connected;
    pmix_list_t nspaces;                 // list of pmix_nspace_t for the nspaces we know about
    pmix_buffer_t *cache_local;          // data PUT by me to local scope
    pmix_buffer_t *cache_remote;         // data PUT by me to remote scope
} pmix_globals_t;


/* initialize the pmix_global structure */
void pmix_globals_init(void);

/*  finalize the pmix_global structure */
void pmix_globals_finalize(void);

extern pmix_globals_t pmix_globals;

END_C_DECLS

#endif /* PMIX_GLOBALS_H */
