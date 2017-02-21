/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include <src/include/pmix_config.h>
#include <pmix_server.h>
#include <pmix/pmix_common.h>
#include "src/include/pmix_globals.h"
#include "src/class/pmix_value_array.h"
#include "src/util/error.h"
#include "src/buffer_ops/internal.h"
#include "src/util/argv.h"
#include "src/util/hash.h"
#include "src/include/pmix_jobdata.h"

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
#include "src/dstore/pmix_dstore.h"
#endif

static inline int _add_key_for_rank(int rank, pmix_kval_t *kv, void *cbdata);
static inline pmix_status_t _job_data_store(const char *nspace, void *cbdata);

static inline int _add_key_for_rank(int rank, pmix_kval_t *kv, void *cbdata)
{
    pmix_job_data_caddy_t *cb = (pmix_job_data_caddy_t*)(cbdata);
    pmix_status_t rc = PMIX_SUCCESS;
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    int i, size;
    pmix_buffer_t *tmp = NULL;
    int cur_rank;

    if (NULL != cb->dstore_fn) {
        /* rank WILDCARD contained in the 0 item */
        cur_rank = PMIX_RANK_WILDCARD == rank ? 0 : rank + 1;
        size = pmix_value_array_get_size(cb->bufs);

        if ((cur_rank + 1) <= size) {
            tmp = &(PMIX_VALUE_ARRAY_GET_ITEM(cb->bufs, pmix_buffer_t, cur_rank));
            pmix_bfrop.pack(tmp, kv, 1, PMIX_KVAL);
            return rc;
        }
        if (PMIX_SUCCESS != (rc = pmix_value_array_set_size(cb->bufs, cur_rank + 1))) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        for (i = size; i < (cur_rank + 1); i++) {
            tmp = &(PMIX_VALUE_ARRAY_GET_ITEM(cb->bufs, pmix_buffer_t, i));
            PMIX_CONSTRUCT(tmp, pmix_buffer_t);
        }
        pmix_bfrop.pack(tmp, kv, 1, PMIX_KVAL);
    }
#endif
    if (cb->hstore_fn) {
        if (PMIX_SUCCESS != (rc = cb->hstore_fn(&cb->nsptr->internal, rank, kv))) {
            PMIX_ERROR_LOG(rc);
        }
    }
    return rc;
}

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
static inline int _rank_key_dstore_store(void *cbdata)
{
    int rc = PMIX_SUCCESS;
    uint32_t i, size;
    pmix_buffer_t *tmp;
    pmix_job_data_caddy_t *cb = (pmix_job_data_caddy_t*)cbdata;
    int rank;
    pmix_kval_t *kv = NULL;

    if (NULL == cb->bufs) {
        rc = PMIX_ERR_BAD_PARAM;
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    kv = PMIX_NEW(pmix_kval_t);
    kv->key = strdup("jobinfo");
    PMIX_VALUE_CREATE(kv->value, 1);
    kv->value->type = PMIX_BYTE_OBJECT;

    size = pmix_value_array_get_size(cb->bufs);
    for (i = 0; i < size; i++) {
        tmp = &(PMIX_VALUE_ARRAY_GET_ITEM(cb->bufs, pmix_buffer_t, i));
        rank = 0 == i ? PMIX_RANK_WILDCARD : i - 1;
        PMIX_UNLOAD_BUFFER(tmp, kv->value->data.bo.bytes, kv->value->data.bo.size);
        if (PMIX_SUCCESS != (rc = cb->dstore_fn(cb->nsptr->nspace, rank, kv))) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

exit:
    if (NULL != kv) {
        PMIX_RELEASE(kv);
    }
    return rc;
}
#endif

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
pmix_status_t pmix_job_data_dstore_store(const char *nspace, pmix_buffer_t *bptr)
{
    pmix_job_data_caddy_t *cd = PMIX_NEW(pmix_job_data_caddy_t);

    cd->job_data = bptr;
    cd->dstore_fn = pmix_dstore_store;

    return _job_data_store(nspace, cd);
}
#endif

pmix_status_t pmix_job_data_htable_store(const char *nspace, pmix_buffer_t *bptr)
{
    pmix_job_data_caddy_t *cb = PMIX_NEW(pmix_job_data_caddy_t);

    cb->job_data = bptr;
    cb->hstore_fn = pmix_hash_store;

    return _job_data_store(nspace, cb);
}

static inline pmix_status_t _job_data_store(const char *nspace, void *cbdata)
{
    pmix_buffer_t *job_data = ((pmix_job_data_caddy_t*)(cbdata))->job_data;
    pmix_job_data_caddy_t *cb = (pmix_job_data_caddy_t*)(cbdata);
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_nspace_t *nsptr = NULL, *nsptr2 = NULL;
    pmix_kval_t *kptr, *kp2, kv;
    int32_t cnt;
    size_t nnodes;
    uint32_t i;
#if !(defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1))
    uint32_t j;
#endif
    pmix_nrec_t *nrec, *nr2;
    char **procs = NULL;
    pmix_byte_object_t *bo;
    pmix_buffer_t buf2;
    int rank;
    char *proc_type_str = pmix_globals.server ?
                            "server" : "client";

    pmix_output_verbose(10, pmix_globals.debug_output,
                    "pmix:%s pmix_jobdata_store %s", proc_type_str, nspace);

    /* check buf data */
    if ((NULL == job_data) && (0 != job_data->bytes_used)) {
        rc = PMIX_ERR_BAD_PARAM;
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    PMIX_LIST_FOREACH(nsptr2, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(nsptr2->nspace, nspace)) {
            nsptr = nsptr2;
            break;
        }
    }
    if (NULL == nsptr) {
        /* we don't know this nspace - add it */
        nsptr = PMIX_NEW(pmix_nspace_t);
        (void)strncpy(nsptr->nspace, nspace, PMIX_MAX_NSLEN);
        pmix_list_append(&pmix_globals.nspaces, &nsptr->super);
    }
    cb->nsptr = nsptr;

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    if (NULL == (cb->bufs = PMIX_NEW(pmix_value_array_t))) {
        rc = PMIX_ERR_OUT_OF_RESOURCE;
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    if (PMIX_SUCCESS != (rc = pmix_value_array_init(cb->bufs, sizeof(pmix_buffer_t)))) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
#endif
    cnt = 1;
    kptr = PMIX_NEW(pmix_kval_t);
    while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(job_data, kptr, &cnt, PMIX_KVAL)))
    {
        if (0 == strcmp(kptr->key, PMIX_PROC_BLOB)) {
            bo = &(kptr->value->data.bo);
            PMIX_CONSTRUCT(&buf2, pmix_buffer_t);
            PMIX_LOAD_BUFFER(&buf2, bo->bytes, bo->size);
            /* start by unpacking the rank */
            cnt = 1;
            if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(&buf2, &rank, &cnt, PMIX_INT))) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&buf2);
                goto exit;
            }
            kp2 = PMIX_NEW(pmix_kval_t);
            kp2->key = strdup(PMIX_RANK);
            PMIX_VALUE_CREATE(kp2->value, 1);
            kp2->value->type = PMIX_INT;
            kp2->value->data.integer = rank;
            if (PMIX_SUCCESS != (rc = _add_key_for_rank(rank, kp2, cb))) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kp2);
                PMIX_DESTRUCT(&buf2);
                goto exit;
            }
            PMIX_RELEASE(kp2); // maintain accounting
            cnt = 1;
            kp2 = PMIX_NEW(pmix_kval_t);
            while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(&buf2, kp2, &cnt, PMIX_KVAL))) {
                /* this is data provided by a job-level exchange, so store it
                 * in the job-level data hash_table */
                if (PMIX_SUCCESS != (rc = _add_key_for_rank(rank, kp2, cb))) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(kp2);
                    PMIX_DESTRUCT(&buf2);
                    goto exit;
                }
                PMIX_RELEASE(kp2); // maintain accounting
                kp2 = PMIX_NEW(pmix_kval_t);
            }
            /* cleanup */
            PMIX_DESTRUCT(&buf2);  // releases the original kptr data
            PMIX_RELEASE(kp2);
        } else if (0 == strcmp(kptr->key, PMIX_MAP_BLOB)) {
            /* transfer the byte object for unpacking */
            bo = &(kptr->value->data.bo);
            PMIX_CONSTRUCT(&buf2, pmix_buffer_t);
            PMIX_LOAD_BUFFER(&buf2, bo->bytes, bo->size);
            /* start by unpacking the number of nodes */
            cnt = 1;
            if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(&buf2, &nnodes, &cnt, PMIX_SIZE))) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&buf2);
                goto exit;
            }
            /* unpack the list of procs on each node */
            for (i=0; i < nnodes; i++) {
                cnt = 1;
                PMIX_CONSTRUCT(&kv, pmix_kval_t);
                if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(&buf2, &kv, &cnt, PMIX_KVAL))) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DESTRUCT(&buf2);
                    PMIX_DESTRUCT(&kv);
                    goto exit;
                }
                /* the name of the node is in the key, and the value is
                 * a comma-delimited list of procs on that node. See if we already
                 * have this node */
                nrec = NULL;
                PMIX_LIST_FOREACH(nr2, &nsptr->nodes, pmix_nrec_t) {
                    if (0 == strcmp(nr2->name, kv.key)) {
                        nrec = nr2;
                        break;
                    }
                }
                if (NULL == nrec) {
                    /* Create a node record and store that list */
                    nrec = PMIX_NEW(pmix_nrec_t);
                    nrec->name = strdup(kv.key);
                    pmix_list_append(&nsptr->nodes, &nrec->super);
                } else {
                    /* refresh the list */
                    if (NULL != nrec->procs) {
                        free(nrec->procs);
                    }
                }
                nrec->procs = strdup(kv.value->data.string);
                /* split the list of procs so we can store their
                 * individual location data */
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
                if (PMIX_SUCCESS != (rc = _add_key_for_rank(PMIX_RANK_WILDCARD, &kv, cb))) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DESTRUCT(&kv);
                    PMIX_DESTRUCT(&buf2);
                    pmix_argv_free(procs);
                    goto exit;
                }
#else
                procs = pmix_argv_split(nrec->procs, ',');
                for (j=0; NULL != procs[j]; j++) {
                    /* store the hostname for each proc - again, this is
                     * data obtained via a job-level exchange, so store it
                     * in the job-level data hash_table */
                    kp2 = PMIX_NEW(pmix_kval_t);
                    kp2->key = strdup(PMIX_HOSTNAME);
                    kp2->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
                    kp2->value->type = PMIX_STRING;
                    kp2->value->data.string = strdup(nrec->name);
                    rank = strtol(procs[j], NULL, 10);
                    if (PMIX_SUCCESS != (rc = _add_key_for_rank(rank, kp2, cb))) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_RELEASE(kp2);
                        PMIX_DESTRUCT(&kv);
                        PMIX_DESTRUCT(&buf2);
                        pmix_argv_free(procs);
                        goto exit;
                    }
                    PMIX_RELEASE(kp2);
                }
                pmix_argv_free(procs);
#endif
                PMIX_DESTRUCT(&kv);
            }
            /* cleanup */
            PMIX_DESTRUCT(&buf2);
        } else {
            if (PMIX_SUCCESS != (rc = _add_key_for_rank(PMIX_RANK_WILDCARD, kptr, cb))) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kptr);
                goto exit;
            }
        }
        PMIX_RELEASE(kptr);
        kptr = PMIX_NEW(pmix_kval_t);
        cnt = 1;
    }
    /* need to release the leftover kptr */
    PMIX_RELEASE(kptr);

    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    rc = PMIX_SUCCESS;

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    if (NULL != cb->dstore_fn) {
        if (PMIX_SUCCESS != (rc = _rank_key_dstore_store(cbdata))) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }
#endif
exit:
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    if (NULL != cb->bufs) {
        PMIX_RELEASE(cb->bufs);
    }
#endif
    PMIX_RELEASE(cb);

    /* reset buf unpack ptr */
    job_data->unpack_ptr = job_data->base_ptr;

    return rc;
}
