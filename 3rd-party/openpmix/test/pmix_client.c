/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2018 Mellanox Technologies, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#include "src/include/pmix_config.h"
#include "include/pmix.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "src/class/pmix_object.h"
#include "test_cd.h"
#include "test_common.h"
#include "test_error.h"
#include "test_fence.h"
#include "test_internal.h"
#include "test_publish.h"
#include "test_replace.h"
#include "test_resolve_peers.h"
#include "test_spawn.h"

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    test_params params;
    INIT_TEST_PARAMS(params);
    pmix_proc_t myproc, proc;

    parse_cmd(argc, argv, &params);

    // We don't know rank at this place!
    TEST_VERBOSE(("Client %s:%d started PID:%d", params.nspace, params.rank, getpid()));

    /* handle early-fail test case */
    if (1 == params.early_fail && 0 == params.rank) {
        exit(1);
    }

    /* init us */
    pmix_info_t info[1];
    size_t ninfo = 0;
    if (NULL != params.gds_mode) {
        pmix_strncpy(info[0].key, PMIX_GDS_MODULE, PMIX_MAX_KEYLEN);
        info[0].value.type = PMIX_STRING;
        info[0].value.data.string = strdup(params.gds_mode);
        ninfo = 1;
    }
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, info, ninfo))) {
        TEST_ERROR(("Client ns %s rank %d: PMIx_Init failed: %s", params.nspace, params.rank,
                    PMIx_Error_string(rc)));
        FREE_TEST_PARAMS(params);
        exit(rc);
    }
    if (myproc.rank != params.rank) {
        TEST_ERROR(("Client ns %s Rank returned in PMIx_Init %d does not match to rank from "
                    "command line %d.",
                    myproc.nspace, myproc.rank, params.rank));
        FREE_TEST_PARAMS(params);
        exit(1);
    }
    if (NULL != params.prefix && -1 != params.ns_id) {
        TEST_SET_FILE(params.prefix, params.ns_id, params.rank);
    }
    TEST_VERBOSE((" Client ns %s rank %d: PMIx_Init success", myproc.nspace, myproc.rank));

    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_UNIV_SIZE, NULL, 0, &val))) {
        TEST_ERROR(
            ("rank %d: PMIx_Get universe size failed: %s", myproc.rank, PMIx_Error_string(rc)));
        FREE_TEST_PARAMS(params);
        exit(rc);
    }
    if (NULL == val) {
        TEST_ERROR(("rank %d: PMIx_Get universe size returned NULL value", myproc.rank));
        FREE_TEST_PARAMS(params);
        exit(1);
    }
    if (val->type != PMIX_UINT32 || val->data.uint32 != (uint32_t) params.ns_size) {
        TEST_ERROR(("rank %d: Universe size value or type mismatch,"
                    " want %d(%d) get %d(%d)",
                    myproc.rank, params.ns_size, PMIX_UINT32, val->data.integer, val->type));
        FREE_TEST_PARAMS(params);
        exit(1);
    }

    TEST_VERBOSE(("rank %d: Universe size check: PASSED", myproc.rank));

    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_HOSTNAME, NULL, 0, &val))) {
        TEST_ERROR(("rank %d: PMIx_Get hostname failed: %s", myproc.rank, PMIx_Error_string(rc)));
        FREE_TEST_PARAMS(params);
        exit(rc);
    }
    if (NULL == val) {
        TEST_ERROR(("rank %d: PMIx_Get hostname returned NULL value", myproc.rank));
        FREE_TEST_PARAMS(params);
        exit(1);
    }
    if (val->type != PMIX_STRING) {
        TEST_ERROR(
            ("rank %d: Hostname type mismatch: %s", myproc.rank, PMIx_Data_type_string(val->type)));
        FREE_TEST_PARAMS(params);
        exit(1);
    }

    TEST_VERBOSE(("rank %d: Hostname check: PASSED", myproc.rank));

    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_NODEID, NULL, 0, &val))) {
        TEST_ERROR(("rank %d: PMIx_Get nodeid failed: %s", myproc.rank, PMIx_Error_string(rc)));
        FREE_TEST_PARAMS(params);
        exit(rc);
    }
    if (NULL == val) {
        TEST_ERROR(("rank %d: PMIx_Get nodeid returned NULL value", myproc.rank));
        FREE_TEST_PARAMS(params);
        exit(1);
    }
    if (val->type != PMIX_UINT32) {
        TEST_ERROR(
            ("rank %d: NodeID type mismatch: %s", myproc.rank, PMIx_Data_type_string(val->type)));
        FREE_TEST_PARAMS(params);
        exit(1);
    }

    TEST_VERBOSE(("rank %d: NodeID check: PASSED", myproc.rank));

    if (NULL != params.nspace && 0 != strcmp(myproc.nspace, params.nspace)) {
        TEST_ERROR(("rank %d: Bad nspace!", myproc.rank));
        FREE_TEST_PARAMS(params);
        exit(1);
    }

    if (NULL != params.fences) {
        rc = test_fence(params, myproc.nspace, myproc.rank);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(
                ("%s:%d Fence test failed: %s", myproc.nspace, myproc.rank, PMIx_Error_string(rc)));
            exit(rc);
        }
    }

    if (0 != params.test_job_fence) {
        rc = test_job_fence(params, myproc.nspace, myproc.rank);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d Job fence test failed: %s", myproc.nspace, myproc.rank,
                        PMIx_Error_string(rc)));
            exit(rc);
        }
    }

    if (0 != params.test_publish) {
        rc = test_publish_lookup(myproc.nspace, myproc.rank);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d Publish/Lookup test failed: %s", myproc.nspace, myproc.rank,
                        PMIx_Error_string(rc)));
            exit(rc);
        }
    }

    if (0 != params.test_spawn) {
        rc = test_spawn(myproc.nspace, myproc.rank);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(
                ("%s:%d Spawn test failed: %s", myproc.nspace, myproc.rank, PMIx_Error_string(rc)));
            exit(rc);
        }
    }

    if (0 != params.test_connect) {
        rc = test_connect_disconnect(myproc.nspace, myproc.rank);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d Connect/Disconnect test failed: %s", myproc.nspace, myproc.rank,
                        PMIx_Error_string(rc)));
            exit(rc);
        }
    }

    if (0 != params.test_resolve_peers) {
        rc = test_resolve_peers(myproc.nspace, myproc.rank, params);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d Resolve peers test failed: %s", myproc.nspace, myproc.rank,
                        PMIx_Error_string(rc)));
            exit(rc);
        }
    }

    if (0 != params.test_error) {
        rc = test_error(myproc.nspace, myproc.rank, params);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d error registration and event handling test failed: %s",
                        myproc.nspace, myproc.rank, PMIx_Error_string(rc)));
            exit(rc);
        }
    }

    if (NULL != params.key_replace) {
        rc = test_replace(myproc.nspace, myproc.rank, params);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d error key replace test failed: %s", myproc.nspace, myproc.rank,
                        PMIx_Error_string(rc)));
            exit(rc);
        }
    }

    if (params.test_internal) {
        rc = test_internal(myproc.nspace, myproc.rank, params);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d error key store internal test failed: %s", myproc.nspace,
                        myproc.rank, PMIx_Error_string(rc)));
            exit(rc);
        }
    }

    TEST_VERBOSE(("Client ns %s rank %d: PASSED", myproc.nspace, myproc.rank));

    /* In case of direct modex we want to delay Finalize
       until everybody has finished. Otherwise some processes
       will fail to get data from others who already exited */
    PMIx_Fence(NULL, 0, NULL, 0);

    /* finalize us */
    TEST_VERBOSE(("Client ns %s rank %d: Finalizing", myproc.nspace, myproc.rank));
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        TEST_ERROR(("Client ns %s rank %d:PMIx_Finalize failed: %s", myproc.nspace, myproc.rank,
                    PMIx_Error_string(rc)));
        exit(rc);
    } else {
        TEST_VERBOSE(("Client ns %s rank %d:PMIx_Finalize successfully completed", myproc.nspace,
                      myproc.rank));
    }

    TEST_VERBOSE(("Client %s:%d finished PID:%d", params.nspace, params.rank, getpid()));
    TEST_OUTPUT_CLEAR(("OK\n"));
    TEST_CLOSE_FILE();
    FREE_TEST_PARAMS(params);
    exit(0);
}
