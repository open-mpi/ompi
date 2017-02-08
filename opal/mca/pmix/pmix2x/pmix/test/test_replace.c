/*
 * Copyright (c) 2017      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "test_replace.h"

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

static int key_is_replace(int key_idx) {
    key_replace_t *item;

    PMIX_LIST_FOREACH(item, &key_replace, key_replace_t) {
        if (item->key_idx == key_idx)
            return 1;
    }
    return 0;
}

int test_replace(char *my_nspace, pmix_rank_t my_rank, test_params params) {
    int key_idx = 0;
    int key_cnt = 0;
    char sval[PMIX_MAX_NSLEN];
    pmix_proc_t proc;
    pmix_status_t rc;
    key_replace_t *item;

    PMIX_CONSTRUCT(&key_replace, pmix_list_t);
    parse_replace(params.key_replace, 1, &key_cnt);

    for (key_idx = 0; key_idx < key_cnt; key_idx++) {
        memset(sval, 0, PMIX_MAX_NSLEN);
        sprintf(sval, "test_replace:%s:%d:%d", my_nspace, my_rank, key_idx);

        PUT(string, sval, PMIX_GLOBAL, 0, key_idx, 1);
        if (PMIX_SUCCESS != rc) {
            TEST_ERROR(("%s:%d: PMIx_Put failed: %d", my_nspace, my_rank, rc));
            PMIX_LIST_DESTRUCT(&key_replace);
            return rc;
        }
    }

    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, my_nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;

    /* Submit the data */
    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        TEST_ERROR(("%s:%d: PMIx_Commit failed: %d", my_nspace, my_rank, rc));
        PMIX_LIST_DESTRUCT(&key_replace);
        PMIX_PROC_DESTRUCT(&proc);
        return PMIX_ERROR;
    }

    FENCE(1, 1, (&proc), 1);
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: PMIx_Fence failed: %d", my_nspace, my_rank, rc));
        PMIX_LIST_DESTRUCT(&key_replace);
        PMIX_PROC_DESTRUCT(&proc);
        return rc;
    }

    PMIX_LIST_FOREACH(item, &key_replace, key_replace_t) {
        memset(sval, 0, PMIX_MAX_NSLEN);
        sprintf(sval, "test_replace:%s:%d:%d: replaced key", my_nspace, my_rank, item->key_idx);

        PUT(string, sval, PMIX_GLOBAL, 0, item->key_idx, 1);
        if (PMIX_SUCCESS != rc) {
            TEST_ERROR(("%s:%d: PMIx_Put failed: %d", my_nspace, my_rank, rc));
            PMIX_LIST_DESTRUCT(&key_replace);
            PMIX_PROC_DESTRUCT(&proc);
            return rc;
        }
    }


    /* Submit the data */
    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        TEST_ERROR(("%s:%d: PMIx_Commit failed: %d", my_nspace, my_rank, rc));
        PMIX_LIST_DESTRUCT(&key_replace);
        PMIX_PROC_DESTRUCT(&proc);
        return PMIX_ERROR;
    }

    FENCE(1, 1, (&proc), 1);
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: PMIx_Fence failed: %d", my_nspace, my_rank, rc));
        PMIX_LIST_DESTRUCT(&key_replace);
        PMIX_PROC_DESTRUCT(&proc);
        return rc;
    }

    for (key_idx = 0; key_idx < key_cnt; key_idx++) {
        memset(sval, 0, PMIX_MAX_NSLEN);

        if (key_is_replace(key_idx)) {
            sprintf(sval, "test_replace:%s:%d:%d: replaced key", my_nspace, my_rank, key_idx);
        } else {
            sprintf(sval, "test_replace:%s:%d:%d", my_nspace, my_rank, key_idx);
        }


        GET(string, sval, my_nspace, my_rank, 0, key_idx, 1, 1, 0);
        if (PMIX_SUCCESS != rc) {
            TEST_ERROR(("%s:%d: PMIx_Get of remote key on local proc", my_nspace, my_rank));
            PMIX_LIST_DESTRUCT(&key_replace);
            PMIX_PROC_DESTRUCT(&proc);
            return PMIX_ERROR;
        }
    }

    PMIX_LIST_DESTRUCT(&key_replace);
    PMIX_PROC_DESTRUCT(&proc);
    return PMIX_SUCCESS;
}
