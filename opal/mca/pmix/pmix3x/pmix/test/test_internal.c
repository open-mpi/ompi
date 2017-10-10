/*
 * Copyright (c) 2017      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "test_internal.h"

static void release_cb(pmix_status_t status, void *cbdata)
{
    int *ptr = (int*)cbdata;
    *ptr = 0;
}

static void get_cb(pmix_status_t status, pmix_value_t *kv, void *cbdata)
{
    get_cbdata *cb = (get_cbdata*)cbdata;
    if (PMIX_SUCCESS == status) {
        pmix_value_xfer(cb->kv, kv);
    }
    cb->in_progress = 0;
    cb->status = status;
}

int test_internal(char *my_nspace, pmix_rank_t my_rank, test_params params) {
    int idx;
    char sval[PMIX_MAX_NSLEN];
    char key[PMIX_MAX_KEYLEN];
    pmix_value_t value;
    pmix_proc_t proc;
    pmix_status_t rc;

    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, my_nspace, PMIX_MAX_NSLEN);
    proc.rank = my_rank;

    for (idx = 0; idx < params.test_internal; idx++) {
        memset(sval, 0, PMIX_MAX_NSLEN);
        sprintf(sval, "test_internal:%s:%d:%d", my_nspace, my_rank, idx);

        SET_KEY(key, 0, idx, 1);
        value.type = PMIX_STRING;
        value.data.string = sval;
        if (PMIX_SUCCESS != (rc = PMIx_Store_internal(&proc, key, &value))) {
            TEST_ERROR(("%s:%d: PMIx_Store_internal failed: %d", my_nspace, my_rank, rc));
            PMIX_PROC_DESTRUCT(&proc);
            return PMIX_ERROR;
        }
    }

    /* Submit the data */
    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        TEST_ERROR(("%s:%d: PMIx_Commit failed: %d", my_nspace, my_rank, rc));
        PMIX_PROC_DESTRUCT(&proc);
        return PMIX_ERROR;
    }

    proc.rank = PMIX_RANK_WILDCARD;
    FENCE(1, 1, (&proc), 1);
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: PMIx_Fence failed: %d", my_nspace, my_rank, rc));
        PMIX_PROC_DESTRUCT(&proc);
        return rc;
    }

    for (idx = 0; idx < params.test_internal; idx++) {
        memset(sval, 0, PMIX_MAX_NSLEN);
        sprintf(sval, "test_internal:%s:%d:%d", my_nspace, my_rank, idx);

        GET(string, sval, my_nspace, my_rank, 0, idx, 1, 1, 0);
        if (PMIX_SUCCESS != rc) {
            TEST_ERROR(("%s:%d: PMIx_Get of remote key on local proc", my_nspace, my_rank));
            PMIX_PROC_DESTRUCT(&proc);
            return PMIX_ERROR;
        }
    }

    PMIX_PROC_DESTRUCT(&proc);
    return PMIX_SUCCESS;
}
