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
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#include <private/autogen/config.h>
#include <pmix.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include "src/class/pmix_object.h"
#include "src/buffer_ops/types.h"
#include "test_common.h"
#include "test_fence.h"
#include "test_publish.h"
#include "test_spawn.h"
#include "test_cd.h"
#include "test_resolve_peers.h"
#include "test_error.h"


static void errhandler(pmix_status_t status,
                pmix_proc_t procs[], size_t nprocs,
                pmix_info_t info[], size_t ninfo)
{
    TEST_ERROR(("PMIX client: Error handler with status = %d", status))
}

static void op_callbk(pmix_status_t status,
               void *cbdata)
{
    TEST_VERBOSE(( "OP CALLBACK CALLED WITH STATUS %d", status));
}

static void errhandler_reg_callbk (pmix_status_t status,
                            int errhandler_ref,
                            void *cbdata)
{
    TEST_VERBOSE(("PMIX client ERRHANDLER REGISTRATION CALLBACK CALLED WITH STATUS %d, ref=%d",
                  status, errhandler_ref));
}

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    test_params params;
    INIT_TEST_PARAMS(params);
    pmix_proc_t myproc;

    parse_cmd(argc, argv, &params);

    // We don't know rank at this place!
    TEST_VERBOSE(("Client ns %s rank %d: Start", params.nspace, params.rank));

    /* handle early-fail test case */
    if (1 == params.early_fail && 0 == params.rank) {
        exit(0);
    }

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc))) {
        TEST_ERROR(("Client ns %s rank %d: PMIx_Init failed: %d", params.nspace, params.rank, rc));
        FREE_TEST_PARAMS(params);
        exit(0);
    }
    PMIx_Register_errhandler(NULL, 0, errhandler, errhandler_reg_callbk, NULL);
    if (myproc.rank != params.rank) {
        TEST_ERROR(("Client ns %s Rank returned in PMIx_Init %d does not match to rank from command line %d.", myproc.nspace, myproc.rank, params.rank));
        FREE_TEST_PARAMS(params);
        exit(0);
    }
    if ( NULL != params.prefix && -1 != params.ns_id) {
        TEST_SET_FILE(params.prefix, params.ns_id, params.rank);
    }
    TEST_VERBOSE((" Client ns %s rank %d: PMIx_Init success", myproc.nspace, myproc.rank));

    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc,PMIX_UNIV_SIZE,NULL, 0,&val))) {
        TEST_ERROR(("rank %d: PMIx_Get universe size failed: %d", myproc.rank, rc));
        FREE_TEST_PARAMS(params);
        exit(0);
    }
    if (NULL == val) {
        TEST_ERROR(("rank %d: PMIx_Get universe size returned NULL value", myproc.rank));
        FREE_TEST_PARAMS(params);
        exit(0);
    }
    if (val->type != PMIX_UINT32 || val->data.uint32 != (uint32_t)params.ns_size ) {
        TEST_ERROR(("rank %d: Universe size value or type mismatch,"
                    " want %d(%d) get %d(%d)",
                    myproc.rank, params.ns_size, PMIX_UINT32,
                    val->data.integer, val->type));
        FREE_TEST_PARAMS(params);
        exit(0);
    }

    TEST_VERBOSE(("rank %d: Universe size check: PASSED", myproc.rank));

    if( NULL != params.nspace && 0 != strcmp(myproc.nspace, params.nspace) ) {
        TEST_ERROR(("rank %d: Bad nspace!", myproc.rank));
        FREE_TEST_PARAMS(params);
        exit(0);
    }

    if (NULL != params.fences) {
        rc = test_fence(params, myproc.nspace, myproc.rank);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d Fence test failed: %d", myproc.nspace, myproc.rank, rc));
            exit(0);
        }
    }

    if (0 != params.test_job_fence) {
        rc = test_job_fence(params, myproc.nspace, myproc.rank);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d Job fence test failed: %d", myproc.nspace, myproc.rank, rc));
            exit(0);
        }
    }

    if (0 != params.test_publish) {
        rc = test_publish_lookup(myproc.nspace, myproc.rank);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d Publish/Lookup test failed: %d", myproc.nspace, myproc.rank, rc));
            exit(0);
        }
    }

    if (0 != params.test_spawn) {
        rc = test_spawn(myproc.nspace, myproc.rank);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d Spawn test failed: %d", myproc.nspace, myproc.rank, rc));
            exit(0);
        }
    }

    if (0 != params.test_connect) {
        rc = test_connect_disconnect(myproc.nspace, myproc.rank);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d Connect/Disconnect test failed: %d", myproc.nspace, myproc.rank, rc));
            exit(0);
        }
    }

    if (0 != params.test_resolve_peers) {
        rc = test_resolve_peers(myproc.nspace, myproc.rank, params);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d Resolve peers test failed: %d", myproc.nspace, myproc.rank, rc));
            exit(0);
        }
    }

    if (0 != params.test_error) {
        rc = test_error(myproc.nspace, myproc.rank, params);
        if (PMIX_SUCCESS != rc) {
            FREE_TEST_PARAMS(params);
            TEST_ERROR(("%s:%d error registration and event handling test failed: %d", myproc.nspace, myproc.rank, rc));
            exit(0);
        }
    }

    TEST_VERBOSE(("Client ns %s rank %d: PASSED", myproc.nspace, myproc.rank));
    PMIx_Deregister_errhandler(1, op_callbk, NULL);
    /* finalize us */
    TEST_VERBOSE(("Client ns %s rank %d: Finalizing", myproc.nspace, myproc.rank));
    if (PMIX_SUCCESS != (rc = PMIx_Finalize())) {
        TEST_ERROR(("Client ns %s rank %d:PMIx_Finalize failed: %d", myproc.nspace, myproc.rank, rc));
    } else {
        TEST_VERBOSE(("Client ns %s rank %d:PMIx_Finalize successfully completed", myproc.nspace, myproc.rank));
    }

    TEST_OUTPUT_CLEAR(("OK\n"));
    TEST_CLOSE_FILE();
    FREE_TEST_PARAMS(params);
    exit(0);
}
