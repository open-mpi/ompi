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

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>

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

#define PMIX_MAX_CRED_SIZE  131072           // set max at 128kbytes
#define PMIX_MAX_ERROR_REGISTRATIONS    5    // maximum number of error handlers that can be registered

/* define a structure for tracking error registrations */
typedef struct {
    pmix_object_t super;
    bool sglhdlr;                      // registers a specific error status handler
    pmix_notification_fn_t errhandler; /* registered err handler callback fn */
    pmix_info_t *info;                 /* error info keys registered with the handler */
    size_t ninfo;                      /* size of info */
} pmix_error_reg_info_t;
PMIX_CLASS_DECLARATION(pmix_error_reg_info_t);

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
    int debug_output;
    pmix_pointer_array_t errregs;        // my error handler registrations.
    bool server;
    bool connected;
    pmix_list_t nspaces;                 // list of pmix_nspace_t for the nspaces we know about
    pmix_buffer_t *cache_local;          // data PUT by me to local scope
    pmix_buffer_t *cache_remote;         // data PUT by me to remote scope
} pmix_globals_t;

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

typedef struct {
    pmix_list_item_t super;
    char *name;              // name of the node
    char *procs;             // comma-separated list of proc ranks on that node
} pmix_nrec_t;
PMIX_CLASS_DECLARATION(pmix_nrec_t);

/* initialize the pmix_global structure */
void pmix_globals_init(void);

/*  finalize the pmix_global structure */
void pmix_globals_finalize(void);

extern pmix_globals_t pmix_globals;

END_C_DECLS

#endif /* PMIX_GLOBALS_H */
