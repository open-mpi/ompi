/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_CLIENT_OPS_H
#define PMIX_CLIENT_OPS_H

#include "src/include/pmix_config.h"

#include "src/class/pmix_list.h"
#include "src/class/pmix_pointer_array.h"
#include "src/common/pmix_iof.h"
#include "src/include/pmix_globals.h"
#include "src/threads/pmix_threads.h"

BEGIN_C_DECLS

typedef struct {
    pmix_peer_t *myserver;        // messaging support to/from my server
    bool singleton;               // no server
    pmix_list_t pending_requests; // list of pmix_cb_t pending data requests
    pmix_pointer_array_t peers;   // array of pmix_peer_t cached for data ops
    pmix_list_t groups;           // list of groups this client is part of
    // verbosity for client get operations
    int get_output;
    int get_verbose;
    // verbosity for client connect operations
    int connect_output;
    int connect_verbose;
    // verbosity for client fence operations
    int fence_output;
    int fence_verbose;
    // verbosity for client pub operations
    int pub_output;
    int pub_verbose;
    // verbosity for client spawn operations
    int spawn_output;
    int spawn_verbose;
    // verbosity for client event operations
    int event_output;
    int event_verbose;
    // verbosity for client iof operations
    int iof_output;
    int iof_verbose;
    // verbosity for basic client functions
    int base_output;
    int base_verbose;
    /* IOF output sinks */
    pmix_iof_sink_t iof_stdout;
    pmix_iof_sink_t iof_stderr;
} pmix_client_globals_t;

PMIX_EXPORT extern pmix_client_globals_t pmix_client_globals;

PMIX_EXPORT void pmix_parse_localquery(int sd, short args, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_client_convert_group_procs(const pmix_proc_t *inprocs, size_t insize,
                                                          pmix_proc_t **outprocs, size_t *outsize);

END_C_DECLS

#endif /* PMIX_CLIENT_OPS_H */
