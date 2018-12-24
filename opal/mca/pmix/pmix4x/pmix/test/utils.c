/*
 * Copyright (c) 2015-2018 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "utils.h"
#include "test_common.h"
#include "pmix_server.h"
#include "cli_stages.h"
#include "test_server.h"

void set_client_argv(test_params *params, char ***argv)
{
    pmix_argv_append_nosize(argv, params->binary);
    pmix_argv_append_nosize(argv, "-n");
    if (NULL == params->np) {
        pmix_argv_append_nosize(argv, "1");
    } else {
        pmix_argv_append_nosize(argv, params->np);
    }
    if( params->verbose ){
        pmix_argv_append_nosize(argv, "-v");
    }
    if (NULL != params->prefix) {
        pmix_argv_append_nosize(argv, "-o");
        pmix_argv_append_nosize(argv, params->prefix);
    }
    if( params->early_fail ){
        pmix_argv_append_nosize(argv, "--early-fail");
    }
    if (NULL != params->fences) {
        pmix_argv_append_nosize(argv, "--fence");
        pmix_argv_append_nosize(argv, params->fences);
        if (params->use_same_keys) {
            pmix_argv_append_nosize(argv, "--use-same-keys");
        }
    }
    if (params->test_job_fence) {
        pmix_argv_append_nosize(argv, "--job-fence");
        if (params->nonblocking) {
            pmix_argv_append_nosize(argv, "-nb");
        }
        if (params->collect) {
            pmix_argv_append_nosize(argv, "-c");
        }
        if (params->collect_bad) {
            pmix_argv_append_nosize(argv, "--collect-corrupt");
        }
    }
    if (NULL != params->noise) {
        pmix_argv_append_nosize(argv, "--noise");
        pmix_argv_append_nosize(argv, params->noise);
    }
    if (NULL != params->ns_dist) {
        pmix_argv_append_nosize(argv, "--ns-dist");
        pmix_argv_append_nosize(argv, params->ns_dist);
    }
    if (params->test_publish) {
        pmix_argv_append_nosize(argv, "--test-publish");
    }
    if (params->test_spawn) {
        pmix_argv_append_nosize(argv, "--test-spawn");
    }
    if (params->test_connect) {
        pmix_argv_append_nosize(argv, "--test-connect");
    }
    if (params->test_resolve_peers) {
        pmix_argv_append_nosize(argv, "--test-resolve-peers");
    }
    if (params->test_error) {
        pmix_argv_append_nosize(argv, "--test-error");
    }
    if (params->key_replace) {
        pmix_argv_append_nosize(argv, "--test-replace");
        pmix_argv_append_nosize(argv, params->key_replace);
    }
    if (params->test_internal) {
        char tmp[32];
        snprintf(tmp, 32, "%d", params->test_internal);
        pmix_argv_append_nosize(argv, "--test-internal");
        pmix_argv_append_nosize(argv, tmp);
    }
    if (params->gds_mode) {
        pmix_argv_append_nosize(argv, "--gds");
        pmix_argv_append_nosize(argv, params->gds_mode);
    }
}
