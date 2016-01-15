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
    pmix_peer_t myserver;           // messaging support to/from my server
    pmix_list_t pending_requests;   // list of pmix_cb_t pending data requests
} pmix_client_globals_t;

extern pmix_client_globals_t pmix_client_globals;

void pmix_client_process_nspace_blob(const char *nspace, pmix_buffer_t *bptr);

END_C_DECLS

#endif /* PMIX_CLIENT_OPS_H */
