/*
 * Copyright (c) 2015-2018 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "utils.h"
#include "cli_stages.h"
#include "pmix_server.h"
#include "test_common.h"
#include "test_server.h"

void set_client_argv(test_params *params, char ***argv)
{
    PMIx_Argv_append_nosize(argv, params->binary);
    PMIx_Argv_append_nosize(argv, "-n");
    if (NULL == params->np) {
        PMIx_Argv_append_nosize(argv, "1");
    } else {
        PMIx_Argv_append_nosize(argv, params->np);
    }
    if (params->verbose) {
        PMIx_Argv_append_nosize(argv, "-v");
    }
    if (NULL != params->prefix) {
        PMIx_Argv_append_nosize(argv, "-o");
        PMIx_Argv_append_nosize(argv, params->prefix);
    }
    if (params->early_fail) {
        PMIx_Argv_append_nosize(argv, "--early-fail");
    }
    if (NULL != params->fences) {
        PMIx_Argv_append_nosize(argv, "--fence");
        PMIx_Argv_append_nosize(argv, params->fences);
        if (params->use_same_keys) {
            PMIx_Argv_append_nosize(argv, "--use-same-keys");
        }
    }
    if (params->test_job_fence) {
        PMIx_Argv_append_nosize(argv, "--job-fence");
        if (params->nonblocking) {
            PMIx_Argv_append_nosize(argv, "-nb");
        }
        if (params->collect) {
            PMIx_Argv_append_nosize(argv, "-c");
        }
        if (params->collect_bad) {
            PMIx_Argv_append_nosize(argv, "--collect-corrupt");
        }
    }
    if (NULL != params->noise) {
        PMIx_Argv_append_nosize(argv, "--noise");
        PMIx_Argv_append_nosize(argv, params->noise);
    }
    if (NULL != params->ns_dist) {
        PMIx_Argv_append_nosize(argv, "--ns-dist");
        PMIx_Argv_append_nosize(argv, params->ns_dist);
    }
    if (params->test_publish) {
        PMIx_Argv_append_nosize(argv, "--test-publish");
    }
    if (params->test_spawn) {
        PMIx_Argv_append_nosize(argv, "--test-spawn");
    }
    if (params->test_connect) {
        PMIx_Argv_append_nosize(argv, "--test-connect");
    }
    if (params->test_resolve_peers) {
        PMIx_Argv_append_nosize(argv, "--test-resolve-peers");
    }
    if (params->test_error) {
        PMIx_Argv_append_nosize(argv, "--test-error");
    }
    if (params->key_replace) {
        PMIx_Argv_append_nosize(argv, "--test-replace");
        PMIx_Argv_append_nosize(argv, params->key_replace);
    }
    if (params->test_internal) {
        char tmp[32];
        snprintf(tmp, 32, "%d", params->test_internal);
        PMIx_Argv_append_nosize(argv, "--test-internal");
        PMIx_Argv_append_nosize(argv, tmp);
    }
    if (params->gds_mode) {
        PMIx_Argv_append_nosize(argv, "--gds");
        PMIx_Argv_append_nosize(argv, params->gds_mode);
    }
}
