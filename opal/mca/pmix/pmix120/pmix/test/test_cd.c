/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "test_cd.h"
#include <time.h>

typedef struct {
    int in_progress;
    int status;
} cd_cbdata;

static void cd_cb(pmix_status_t status, void *cbdata)
{
    cd_cbdata *cb = (cd_cbdata*)cbdata;

    cb->in_progress = 0;
    cb->status = status;
}

int test_cd_common(pmix_proc_t *procs, size_t nprocs, int blocking, int disconnect)
{
    int rc;
    if (blocking) {
        if (!disconnect) {
            rc = PMIx_Connect(procs, nprocs, NULL, 0);
        } else {
            rc = PMIx_Disconnect(procs, nprocs, NULL, 0);
        }
    } else {
        cd_cbdata cbdata;
        cbdata.in_progress = 1;
        if (!disconnect) {
            rc = PMIx_Connect_nb(procs, nprocs, NULL, 0, cd_cb, (void*)&cbdata);
        } else {
            rc = PMIx_Disconnect_nb(procs, nprocs, NULL, 0, cd_cb, (void*)&cbdata);
        }
        if (PMIX_SUCCESS == rc) {
            PMIX_WAIT_FOR_COMPLETION(cbdata.in_progress);
            rc = cbdata.status;
        }
    }
    /* the host server callback currently returns PMIX_EXISTS status for checking purposes */
    if (PMIX_EXISTS == rc) {
        rc = PMIX_SUCCESS;
    }
    return rc;
}

int test_connect_disconnect(char *my_nspace, int my_rank)
{
    int rc;
    pmix_proc_t proc;
    (void)strncpy(proc.nspace, my_nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    rc = test_cd_common(&proc, 1, 1, 0);
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: Connect blocking test failed.", my_nspace, my_rank));
        return PMIX_ERROR;
    }
    TEST_VERBOSE(("%s:%d: Connect blocking test succeded.", my_nspace, my_rank));
    rc = test_cd_common(&proc, 1, 1, 1);
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: Disconnect blocking test failed.", my_nspace, my_rank));
        return PMIX_ERROR;
    }
    TEST_VERBOSE(("%s:%d: Disconnect blocking test succeded.", my_nspace, my_rank));
    rc = test_cd_common(&proc, 1, 0, 0);
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: Connect non-blocking test failed.", my_nspace, my_rank));
        return PMIX_ERROR;
    }
    TEST_VERBOSE(("%s:%d: Connect non-blocking test succeded.", my_nspace, my_rank));
    rc = test_cd_common(&proc, 1, 0, 1);
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: Disconnect non-blocking test failed.", my_nspace, my_rank));
        return PMIX_ERROR;
    }
    TEST_VERBOSE(("%s:%d: Disconnect non-blocking test succeded.", my_nspace, my_rank));
    return PMIX_SUCCESS;
}

