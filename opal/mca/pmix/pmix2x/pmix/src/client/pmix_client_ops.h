/*
 * Copyright (c) 2015-2016 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_CLIENT_OPS_H
#define PMIX_CLIENT_OPS_H

#include <src/include/pmix_config.h>


#include "src/buffer_ops/buffer_ops.h"
#include "src/class/pmix_hash_table.h"

BEGIN_C_DECLS

typedef struct {
    pmix_peer_t myserver;           // messaging support to/from my server
    pmix_list_t pending_requests;   // list of pmix_cb_t pending data requests
    bool wait_for_debugger;         // stop at the end of PMIx_Init and wait for notification of debugger release
} pmix_client_globals_t;

extern pmix_client_globals_t pmix_client_globals;

END_C_DECLS

#endif /* PMIX_CLIENT_OPS_H */
