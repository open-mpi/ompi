/*
 * Copyright (c) 2015-2017 Intel, Inc. All rights reserved.
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
    pmix_proc_t pname;
} cd_cbdata;

static void cd_cb(pmix_status_t status, void *cbdata)
{
    cd_cbdata *cb = (cd_cbdata*)cbdata;

    cb->status = status;
    cb->in_progress = 0;
}

static void cnct_cb(pmix_status_t status,
                    char nspace[], int rank,
                    void *cbdata)
{
    cd_cbdata *cb = (cd_cbdata*)cbdata;

    if (NULL != nspace) {
        (void)strncpy(cb->pname.nspace, nspace, PMIX_MAX_NSLEN);
    }
    cb->pname.rank = rank;
    cb->status = status;
    cb->in_progress = 0;
}

int test_connect_disconnect(char *my_nspace, int my_rank)
{
    int rc;
    pmix_proc_t proc;
    char nspace[PMIX_MAX_NSLEN+1];
    pmix_rank_t newrank;
    cd_cbdata cbdata;

    (void)strncpy(proc.nspace, my_nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;

    rc = PMIx_Connect(&proc, 1, NULL, 0, nspace, &newrank);
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: Connect blocking test failed.", my_nspace, my_rank));
        return PMIX_ERROR;
    }
    TEST_VERBOSE(("%s:%d: Connect blocking test succeded to nspace %s.", my_nspace, my_rank, nspace));

    rc = PMIx_Disconnect(nspace, NULL, 0);
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: Disconnect blocking test failed.", my_nspace, my_rank));
        return PMIX_ERROR;
    }
    TEST_VERBOSE(("%s:%d: Disconnect blocking test succeded.", my_nspace, my_rank));

    cbdata.in_progress = 1;
    rc = PMIx_Connect_nb(&proc, 1, NULL, 0, cnct_cb, &cbdata);
    if (PMIX_SUCCESS == rc) {
        PMIX_WAIT_FOR_COMPLETION(cbdata.in_progress);
        rc = cbdata.status;
    }
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: Connect non-blocking test failed.", my_nspace, my_rank));
        return PMIX_ERROR;
    }
    TEST_VERBOSE(("%s:%d: Connect non-blocking test succeded.", my_nspace, my_rank));

    cbdata.in_progress = 1;
    rc = PMIx_Disconnect_nb(nspace, NULL, 0, cd_cb, &cbdata);
    if (PMIX_SUCCESS == rc) {
        PMIX_WAIT_FOR_COMPLETION(cbdata.in_progress);
        rc = cbdata.status;
    }
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: Disconnect non-blocking test failed.", my_nspace, my_rank));
        return PMIX_ERROR;
    }
    TEST_VERBOSE(("%s:%d: Disconnect non-blocking test succeded.", my_nspace, my_rank));
    return PMIX_SUCCESS;
}
