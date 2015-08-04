/*
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef PMIX_CLIENT_OPS_H
#define PMIX_CLIENT_OPS_H

#include <private/autogen/config.h>
#include <pmix/rename.h>

#include "src/buffer_ops/buffer_ops.h"
#include "src/class/pmix_hash_table.h"
#include "src/usock/usock.h"

BEGIN_C_DECLS

typedef struct {
    int init_cntr;                  // #times someone called PMIx_Init - #times called PMIx_Finalize
    pmix_peer_t myserver;           // messaging support to/from my server
    pmix_buffer_t *cache_local;     // data PUT by me to local scope
    pmix_buffer_t *cache_remote;    // data PUT by me to remote scope
    pmix_list_t nspaces;            // list of pmix_nsrec_t of nspaces I know about
    pmix_list_t pending_requests;   // list of pmix_cb_t pending data requests
} pmix_client_globals_t;

typedef struct {
    pmix_list_item_t super;
    char nspace[PMIX_MAX_NSLEN+1];
    pmix_list_t nodes;               // list of pmix_nrec_t
    pmix_hash_table_t data;          // hash_table for data provided at job-level exchange
    pmix_hash_table_t modex;         // hash_table of received modex data
} pmix_nsrec_t;
PMIX_CLASS_DECLARATION(pmix_nsrec_t);

typedef struct {
    pmix_list_item_t super;
    char *name;              // name of the node
    char *procs;             // comma-separated list of proc ranks on that node
} pmix_nrec_t;
PMIX_CLASS_DECLARATION(pmix_nrec_t);

extern pmix_client_globals_t pmix_client_globals;

void pmix_client_process_nspace_blob(const char *nspace, pmix_buffer_t *bptr);

END_C_DECLS

#endif /* PMIX_CLIENT_OPS_H */
