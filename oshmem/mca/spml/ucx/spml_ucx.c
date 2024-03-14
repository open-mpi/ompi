/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016      ARM, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
 
#define _GNU_SOURCE
#include <stdio.h>

#include <sys/types.h>
#include <unistd.h>
#include <stdint.h>

#include "oshmem_config.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/mca/common/ucx/common_ucx.h"
#include "opal/util/opal_environ.h"
#include "opal/util/minmax.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/pml/pml.h"


#include "oshmem/mca/spml/ucx/spml_ucx.h"
#include "oshmem/include/shmem.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/proc/proc.h"
#include "oshmem/mca/spml/base/base.h"
#include "oshmem/mca/spml/base/spml_base_putreq.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/runtime/runtime.h"

#include "oshmem/mca/spml/ucx/spml_ucx_component.h"
#include "oshmem/mca/sshmem/ucx/sshmem_ucx.h"

/* Turn ON/OFF debug output from build (default 0) */
#ifndef SPML_UCX_PUT_DEBUG
#define SPML_UCX_PUT_DEBUG    0
#endif

mca_spml_ucx_t mca_spml_ucx = {
    .super = {
        /* Init mca_spml_base_module_t */
        .spml_add_procs     = mca_spml_ucx_add_procs,
        .spml_del_procs     = mca_spml_ucx_del_procs,
        .spml_enable        = mca_spml_ucx_enable,
        .spml_register      = mca_spml_ucx_register,
        .spml_deregister    = mca_spml_ucx_deregister,
        .spml_oob_get_mkeys = mca_spml_base_oob_get_mkeys,
        .spml_ctx_create    = mca_spml_ucx_ctx_create,
        .spml_ctx_destroy   = mca_spml_ucx_ctx_destroy,
        .spml_put           = mca_spml_ucx_put,
        .spml_put_nb        = mca_spml_ucx_put_nb,
        .spml_put_signal    = mca_spml_ucx_put_signal,
        .spml_put_signal_nb = mca_spml_ucx_put_signal_nb,
        .spml_get           = mca_spml_ucx_get,
        .spml_get_nb        = mca_spml_ucx_get_nb,
        .spml_recv          = mca_spml_ucx_recv,
        .spml_send          = mca_spml_ucx_send,
        .spml_fence         = mca_spml_ucx_fence,
        .spml_quiet         = mca_spml_ucx_quiet,
        .spml_rmkey_unpack  = mca_spml_ucx_rmkey_unpack,
        .spml_rmkey_free    = mca_spml_ucx_rmkey_free,
        .spml_rmkey_ptr     = mca_spml_ucx_rmkey_ptr,
        .spml_memuse_hook   = mca_spml_ucx_memuse_hook,
        .spml_put_all_nb    = mca_spml_ucx_put_all_nb,
        .spml_wait                      = mca_spml_base_wait,
        .spml_wait_nb                   = mca_spml_base_wait_nb,
        .spml_wait_until_all            = mca_spml_ucx_wait_until_all,
        .spml_wait_until_any            = mca_spml_ucx_wait_until_any,
        .spml_wait_until_some           = mca_spml_ucx_wait_until_some,
        .spml_wait_until_all_vector     = mca_spml_ucx_wait_until_all_vector,
        .spml_wait_until_any_vector     = mca_spml_ucx_wait_until_any_vector,
        .spml_wait_until_some_vector    = mca_spml_ucx_wait_until_some_vector,
        .spml_test                      = mca_spml_base_test,
        .spml_test_all          	= mca_spml_ucx_test_all,
        .spml_test_any          	= mca_spml_ucx_test_any,
        .spml_test_some         	= mca_spml_ucx_test_some,
        .spml_test_all_vector   	= mca_spml_ucx_test_all_vector,
        .spml_test_any_vector   	= mca_spml_ucx_test_any_vector,
        .spml_test_some_vector  	= mca_spml_ucx_test_some_vector,
        .spml_team_sync                 = mca_spml_ucx_team_sync,
        .spml_team_my_pe                = mca_spml_ucx_team_my_pe,
        .spml_team_n_pes                = mca_spml_ucx_team_n_pes,
        .spml_team_get_config           = mca_spml_ucx_team_get_config,
        .spml_team_translate_pe         = mca_spml_ucx_team_translate_pe,
        .spml_team_split_strided        = mca_spml_ucx_team_split_strided,
        .spml_team_split_2d             = mca_spml_ucx_team_split_2d,
        .spml_team_destroy              = mca_spml_ucx_team_destroy,
        .spml_team_get                  = mca_spml_ucx_team_get,
        .spml_team_create_ctx           = mca_spml_ucx_team_create_ctx,
        .spml_team_alltoall             = mca_spml_ucx_team_alltoall,
        .spml_team_alltoalls            = mca_spml_ucx_team_alltoalls,
        .spml_team_broadcast            = mca_spml_ucx_team_broadcast,
        .spml_team_collect              = mca_spml_ucx_team_collect,
        .spml_team_fcollect             = mca_spml_ucx_team_fcollect,
        .spml_team_reduce               = mca_spml_ucx_team_reduce,
        .self                           = (void*)&mca_spml_ucx
    },

    .ucp_context            = NULL,
    .num_disconnect         = 1,
    .heap_reg_nb            = 0,
    .enabled                = 0,
    .get_mkey_slow          = NULL
};

mca_spml_ucx_ctx_t mca_spml_ucx_ctx_default = {
    .ucp_worker         = NULL,
    .ucp_peers          = NULL,
    .options            = 0,
    .synchronized_quiet = false,
    .strong_sync        = SPML_UCX_STRONG_ORDERING_NONE
};

#ifdef HAVE_UCP_REQUEST_PARAM_T
static ucp_request_param_t mca_spml_ucx_request_param = {0};
static ucp_request_param_t mca_spml_ucx_request_param_b = {
    .op_attr_mask = UCP_OP_ATTR_FLAG_FAST_CMPL
};
#endif

unsigned
mca_spml_ucx_mem_map_flags_symmetric_rkey(struct mca_spml_ucx *spml_ucx)
{
#if HAVE_DECL_UCP_MEM_MAP_SYMMETRIC_RKEY
    if (spml_ucx->symmetric_rkey_max_count > 0) {
        return UCP_MEM_MAP_SYMMETRIC_RKEY;
    }
#endif

    return 0;
}

void mca_spml_ucx_rkey_store_init(mca_spml_ucx_rkey_store_t *store)
{
    store->array = NULL;
    store->count = 0;
    store->size  = 0;
}

void mca_spml_ucx_rkey_store_cleanup(mca_spml_ucx_rkey_store_t *store)
{
    int i;

    for (i = 0; i < store->count; i++) {
        if (store->array[i].refcnt != 0) {
            SPML_UCX_ERROR("rkey store destroy: %d/%d has refcnt %d > 0",
                           i, store->count, store->array[i].refcnt);
        }

        ucp_rkey_destroy(store->array[i].rkey);
    }

    free(store->array);
}

/**
 * Find position in sorted array for existing or future entry
 *
 * @param[in]  store  Store of the entries
 * @param[in]  worker Common worker for rkeys used
 * @param[in]  rkey   Remote key to search for
 * @param[out] index  Index of entry
 *
 * @return
 *   OSHMEM_ERR_NOT_FOUND: index contains the position where future element
 *                         should be inserted to keep array sorted
 *   OSHMEM_SUCCESS      : index contains the position of the element
 *   Other error         : index is not valid
 */
static int mca_spml_ucx_rkey_store_find(const mca_spml_ucx_rkey_store_t *store,
                                        const ucp_worker_h worker,
                                        const ucp_rkey_h rkey,
                                        int *index)
{
#if HAVE_DECL_UCP_RKEY_COMPARE
    ucp_rkey_compare_params_t params;
    int i, result, m, end;
    ucs_status_t status;

    for (i = 0, end = store->count; i < end;) {
        m = (i + end) / 2;

        params.field_mask = 0;
        status = ucp_rkey_compare(worker, store->array[m].rkey,
                                  rkey, &params, &result);
        if (status != UCS_OK) {
            return OSHMEM_ERROR;
        } else if (result == 0) {
            *index = m;
            return OSHMEM_SUCCESS;
        } else if (result > 0) {
            end = m;
        } else {
            i = m + 1;
        }
    }

    *index = i;
    return OSHMEM_ERR_NOT_FOUND;
#else
    return OSHMEM_ERROR;
#endif
}

static void mca_spml_ucx_rkey_store_insert(mca_spml_ucx_rkey_store_t *store,
                                           int i, ucp_rkey_h rkey)
{
    int size;
    mca_spml_ucx_rkey_t *tmp;

    if (store->count >= mca_spml_ucx.symmetric_rkey_max_count) {
        return;
    }

    if (store->count >= store->size) {
        size = opal_min(opal_max(store->size, 8) * 2,
                        mca_spml_ucx.symmetric_rkey_max_count);
        tmp  = realloc(store->array, size * sizeof(*store->array));
        if (tmp == NULL) {
            return;
        }

        store->array = tmp;
        store->size  = size;
    }

    memmove(&store->array[i + 1], &store->array[i],
            (store->count - i) * sizeof(*store->array));
    store->array[i].rkey   = rkey;
    store->array[i].refcnt = 1;
    store->count++;
    return;
}

/* Takes ownership of input ucp remote key */
static ucp_rkey_h mca_spml_ucx_rkey_store_get(mca_spml_ucx_rkey_store_t *store,
                                              ucp_worker_h worker,
                                              ucp_rkey_h rkey)
{
    int ret, i;

    if (mca_spml_ucx.symmetric_rkey_max_count == 0) {
        return rkey;
    }

    ret = mca_spml_ucx_rkey_store_find(store, worker, rkey, &i);
    if (ret == OSHMEM_SUCCESS) {
        ucp_rkey_destroy(rkey);
        store->array[i].refcnt++;
        return store->array[i].rkey;
    }

    if (ret == OSHMEM_ERR_NOT_FOUND) {
        mca_spml_ucx_rkey_store_insert(store, i, rkey);
    }

    return rkey;
}

static void mca_spml_ucx_rkey_store_put(mca_spml_ucx_rkey_store_t *store,
                                        ucp_worker_h worker,
                                        ucp_rkey_h rkey)
{
    mca_spml_ucx_rkey_t *entry;
    int ret, i;

    ret = mca_spml_ucx_rkey_store_find(store, worker, rkey, &i);
    if (ret != OSHMEM_SUCCESS) {
        goto out;
    }

    entry = &store->array[i];
    assert(entry->rkey == rkey);
    if (--entry->refcnt > 0) {
        return;
    }

    memmove(&store->array[i], &store->array[i + 1],
            (store->count - (i + 1)) * sizeof(*store->array));
    store->count--;

out:
    ucp_rkey_destroy(rkey);
}

int mca_spml_ucx_enable(bool enable)
{
    SPML_UCX_VERBOSE(50, "*** ucx ENABLED ****");
    if (false == enable) {
        return OSHMEM_SUCCESS;
    }

    mca_spml_ucx.enabled = true;

    return OSHMEM_SUCCESS;
}

/* initialize the mkey cache */
void mca_spml_ucx_peer_mkey_cache_init(mca_spml_ucx_ctx_t *ucx_ctx, int pe)
{
    ucx_ctx->ucp_peers[pe].mkeys = NULL;
    ucx_ctx->ucp_peers[pe].mkeys_cnt = 0;
}

/* add a new mkey and update the mkeys_cnt */
int mca_spml_ucx_peer_mkey_cache_add(ucp_peer_t *ucp_peer, int index)
{
    /* Allocate an array to hold the pointers to the ucx_cached_mkey */
    if (index >= (int)ucp_peer->mkeys_cnt){
        int old_size = ucp_peer->mkeys_cnt;
        ucp_peer->mkeys_cnt = index + 1;
        ucp_peer->mkeys = realloc(ucp_peer->mkeys, sizeof(ucp_peer->mkeys[0]) * ucp_peer->mkeys_cnt);
        if (NULL == ucp_peer->mkeys) {
            SPML_UCX_ERROR("Failed to obtain new mkey: OOM - failed to expand the descriptor buffer");
            return OSHMEM_ERR_OUT_OF_RESOURCE;
        }
        /* NOTE: release code checks for the rkey != NULL as a sign of used element:
        Account for the following scenario below by zero'ing the unused elements:
        |MKEY1|00000|MKEY2|??????|NEW-MKEY|
        |<--- old_size -->|
        */
        memset(ucp_peer->mkeys + old_size, 0, (ucp_peer->mkeys_cnt - old_size) * sizeof(ucp_peer->mkeys[0]));
    } else {
        /* Make sure we don't leak memory */
        assert(NULL == ucp_peer->mkeys[index]);
    }

    ucp_peer->mkeys[index] = (spml_ucx_cached_mkey_t *) malloc(sizeof(*ucp_peer->mkeys[0]));
    if (NULL == ucp_peer->mkeys[index]) {
        SPML_UCX_ERROR("Failed to obtain new ucx_cached_mkey: OOM - failed to expand the descriptor buffer");
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }
    return OSHMEM_SUCCESS;
}

/* Release individual mkeys */
int mca_spml_ucx_peer_mkey_cache_del(ucp_peer_t *ucp_peer, int segno)
{
    if (((int)ucp_peer->mkeys_cnt <= segno) || (segno < 0)) {
        return OSHMEM_ERR_NOT_AVAILABLE;
    }
    if (NULL != ucp_peer->mkeys[segno]) {
        free(ucp_peer->mkeys[segno]);
        ucp_peer->mkeys[segno] = NULL;
    }
    return OSHMEM_SUCCESS;
}

/* Release the memkey map from a ucp_peer if it has any element in memkey */
void mca_spml_ucx_peer_mkey_cache_release(ucp_peer_t *ucp_peer)
{
    size_t i;
    if (ucp_peer->mkeys_cnt) {
        for(i = 0; i < ucp_peer->mkeys_cnt; i++) {
            assert(NULL == ucp_peer->mkeys[i]);
        }
        free(ucp_peer->mkeys);
        ucp_peer->mkeys = NULL;
    }
}

int mca_spml_ucx_ctx_mkey_new(mca_spml_ucx_ctx_t *ucx_ctx, int pe, uint32_t segno, spml_ucx_mkey_t **mkey)
{
    ucp_peer_t *ucp_peer;
    spml_ucx_cached_mkey_t *ucx_cached_mkey;
    int rc;
    ucp_peer = &(ucx_ctx->ucp_peers[pe]);
    rc = mca_spml_ucx_peer_mkey_cache_add(ucp_peer, segno);
    if (OSHMEM_SUCCESS != rc) {
        return rc;
    }
    rc = mca_spml_ucx_peer_mkey_get(ucp_peer, segno, &ucx_cached_mkey);
    if (OSHMEM_SUCCESS != rc) {
        return rc;
    }
    *mkey = &(ucx_cached_mkey->key);
    return OSHMEM_SUCCESS;
}

int mca_spml_ucx_ctx_mkey_cache(mca_spml_ucx_ctx_t *ucx_ctx, sshmem_mkey_t *mkey, uint32_t segno, int dst_pe)
{
    ucp_peer_t *peer;
    spml_ucx_cached_mkey_t *ucx_cached_mkey;
    int rc;

    peer = &(ucx_ctx->ucp_peers[dst_pe]);
    rc = mca_spml_ucx_peer_mkey_get(peer, segno, &ucx_cached_mkey);
    if (OSHMEM_SUCCESS != rc) {
        SPML_UCX_ERROR("mca_spml_ucx_peer_mkey_get failed");
        return rc;
    }
    mkey_segment_init(&ucx_cached_mkey->super, mkey, segno);
    return OSHMEM_SUCCESS;
}

int mca_spml_ucx_ctx_mkey_add(mca_spml_ucx_ctx_t *ucx_ctx, int pe, uint32_t segno, sshmem_mkey_t *mkey, spml_ucx_mkey_t **ucx_mkey)
{
    int rc;
    ucs_status_t err;
    ucp_rkey_h rkey;

    rc = mca_spml_ucx_ctx_mkey_new(ucx_ctx, pe, segno, ucx_mkey);
    if (OSHMEM_SUCCESS != rc) {
        SPML_UCX_ERROR("mca_spml_ucx_ctx_mkey_new failed");
        return rc;
    }

    if (mkey->u.data) {
        err = ucp_ep_rkey_unpack(ucx_ctx->ucp_peers[pe].ucp_conn, mkey->u.data, &rkey);
        if (UCS_OK != err) {
            SPML_UCX_ERROR("failed to unpack rkey: %s", ucs_status_string(err));
            return OSHMEM_ERROR;
        }

        if (!oshmem_proc_on_local_node(pe)) {
            rkey = mca_spml_ucx_rkey_store_get(&ucx_ctx->rkey_store, ucx_ctx->ucp_worker[0], rkey);
        }

        (*ucx_mkey)->rkey = rkey;

        rc = mca_spml_ucx_ctx_mkey_cache(ucx_ctx, mkey, segno, pe);
        if (OSHMEM_SUCCESS != rc) {
            SPML_UCX_ERROR("mca_spml_ucx_ctx_mkey_cache failed");
            return rc;
        }
    }
    return OSHMEM_SUCCESS;
}

int mca_spml_ucx_ctx_mkey_del(mca_spml_ucx_ctx_t *ucx_ctx, int pe, uint32_t segno, spml_ucx_mkey_t *ucx_mkey)
{
    ucp_peer_t *ucp_peer;
    int rc;
    ucp_peer = &(ucx_ctx->ucp_peers[pe]);
    mca_spml_ucx_rkey_store_put(&ucx_ctx->rkey_store, ucx_ctx->ucp_worker[0], ucx_mkey->rkey);
    ucx_mkey->rkey = NULL;
    rc = mca_spml_ucx_peer_mkey_cache_del(ucp_peer, segno);
    if(OSHMEM_SUCCESS != rc){
        SPML_UCX_ERROR("mca_spml_ucx_peer_mkey_cache_del failed");
        return rc;
    }
    return OSHMEM_SUCCESS;
}

int mca_spml_ucx_del_procs(oshmem_group_t* group, size_t nprocs)
{
    size_t ucp_workers = mca_spml_ucx.ucp_workers;
    opal_common_ucx_del_proc_t *del_procs;
    size_t i, w, n;
    int ret;

    oshmem_shmem_barrier();

    if (!mca_spml_ucx_ctx_default.ucp_peers) {
        return OSHMEM_SUCCESS;
    }

    del_procs = malloc(sizeof(*del_procs) * nprocs);
    if (del_procs == NULL) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < nprocs; ++i) {
        del_procs[i].ep   = mca_spml_ucx_ctx_default.ucp_peers[i].ucp_conn;
        del_procs[i].vpid = i;

        /* mark peer as disconnected */
        mca_spml_ucx_ctx_default.ucp_peers[i].ucp_conn = NULL;
        /* release the cached_ep_mkey buffer */
        mca_spml_ucx_peer_mkey_cache_release(&(mca_spml_ucx_ctx_default.ucp_peers[i]));
    }

    ret = opal_common_ucx_del_procs_nofence(del_procs, nprocs, oshmem_my_proc_id(),
                                            mca_spml_ucx.num_disconnect,
                                            mca_spml_ucx_ctx_default.ucp_worker[0]);
    /* No need to barrier here - barrier is called in _shmem_finalize */
    free(del_procs);
    if (mca_spml_ucx.remote_addrs_tbl) {
        for (w = 0; w < ucp_workers; w++) {
            if (mca_spml_ucx.remote_addrs_tbl[w]) {
                for (n = 0; n < nprocs; n++) {
                    if (mca_spml_ucx.remote_addrs_tbl[w][n]) {
                        free(mca_spml_ucx.remote_addrs_tbl[w][n]);
                    }
                }
                free(mca_spml_ucx.remote_addrs_tbl[w]);
            }
        }
        free(mca_spml_ucx.remote_addrs_tbl);
    }

    free(mca_spml_ucx_ctx_default.ucp_peers);

    mca_spml_ucx_ctx_default.ucp_peers = NULL;

    return ret;
}

/* TODO: move func into common place, use it with rkey exchange too */
static int oshmem_shmem_xchng(
        void **local_data, unsigned int *local_size, int nprocs, int ucp_workers,
        void **rdata_p, unsigned int **roffsets_p, unsigned int **rsizes_p)
{
    unsigned int *rcv_sizes   = NULL;
    int *_rcv_sizes = NULL;
    unsigned int *rcv_offsets = NULL;
    int *_rcv_offsets = NULL; 
    void *rcv_buf       = NULL;
    int rc;
    int i,j,k;

    /* do allgatherv */
    rcv_offsets = calloc(ucp_workers * nprocs, sizeof(*rcv_offsets));
    if (NULL == rcv_offsets) {
        goto err;
    }

    /* todo: move into separate function. do allgatherv */
    rcv_sizes = calloc(ucp_workers * nprocs, sizeof(*rcv_sizes));
    if (NULL == rcv_sizes) {
        goto err;
    }
   
    rc = oshmem_shmem_allgather(local_size, rcv_sizes, ucp_workers * sizeof(*rcv_sizes));
    if (MPI_SUCCESS != rc) {
        goto err;
    }

    /* calculate displacements */
    rcv_offsets[0] = 0;
    for (i = 1; i < ucp_workers * nprocs; i++) {
        rcv_offsets[i] = rcv_offsets[i - 1] + rcv_sizes[i - 1];
    }

    rcv_buf = calloc(1, rcv_offsets[(ucp_workers * nprocs) - 1]
                        + rcv_sizes[(ucp_workers * nprocs) - 1]);
    if (NULL == rcv_buf) {
        goto err;
    }
   
    int _local_size = 0;
    for (i = 0; i < ucp_workers; i++) {
        _local_size += local_size[i];
    }
    _rcv_offsets = calloc(nprocs, sizeof(*rcv_offsets));
    _rcv_sizes = calloc(nprocs, sizeof(*rcv_sizes));

    k = 0;
    for (i = 0; i < nprocs; i++) {
        for (j = 0; j < ucp_workers; j++, k++) {
            _rcv_sizes[i] += rcv_sizes[k];
        }
    }

    _rcv_offsets[0] = 0;
    for (i = 1; i < nprocs; i++) {
        _rcv_offsets[i] = _rcv_offsets[i - 1] + _rcv_sizes[i - 1];
    }

    char *_local_data = calloc(_local_size, 1);
    int new_offset = 0;
    for (i = 0; i < ucp_workers; i++) {
        memcpy((char *) (_local_data+new_offset), (char *)local_data[i], local_size[i]);
        new_offset += local_size[i];
    }

    rc = oshmem_shmem_allgatherv(_local_data, rcv_buf, _local_size, _rcv_sizes, _rcv_offsets);
    if (MPI_SUCCESS != rc) {
        goto err;
    }

    free (_local_data);
    free (_rcv_sizes);
    free (_rcv_offsets);
    *rdata_p    = rcv_buf;
    *roffsets_p = rcv_offsets;
    *rsizes_p   = rcv_sizes;
    return OSHMEM_SUCCESS;

err:
    if (rcv_buf) 
        free(rcv_buf);
    if (rcv_offsets)
        free(rcv_offsets);
    if (rcv_sizes)
        free(rcv_sizes);
    return OSHMEM_ERROR;
}


int mca_spml_ucx_init_put_op_mask(mca_spml_ucx_ctx_t *ctx, size_t nprocs)
{
    int res;

    if (mca_spml_ucx_is_strong_ordering(ctx)) {
        ctx->put_proc_indexes = malloc(nprocs * sizeof(*ctx->put_proc_indexes));
        if (NULL == ctx->put_proc_indexes) {
            return OSHMEM_ERR_OUT_OF_RESOURCE;
        }

        OBJ_CONSTRUCT(&ctx->put_op_bitmap, opal_bitmap_t);
        res = opal_bitmap_init(&ctx->put_op_bitmap, nprocs);
        if (OPAL_SUCCESS != res) {
            free(ctx->put_proc_indexes);
            ctx->put_proc_indexes = NULL;
            return res;
        }

        ctx->put_proc_count = 0;
    }

    return OSHMEM_SUCCESS;
}

int mca_spml_ucx_clear_put_op_mask(mca_spml_ucx_ctx_t *ctx)
{
    if (mca_spml_ucx_is_strong_ordering(ctx) && ctx->put_proc_indexes) {
        OBJ_DESTRUCT(&ctx->put_op_bitmap);
        free(ctx->put_proc_indexes);
    }

    return OSHMEM_SUCCESS;
}

int mca_spml_ucx_add_procs(oshmem_group_t* group, size_t nprocs)
{
    int rc                  = OSHMEM_ERROR;
    int my_rank             = oshmem_my_proc_id();
    size_t ucp_workers      = mca_spml_ucx.ucp_workers;
    unsigned int *wk_roffs  = NULL;
    unsigned int *wk_rsizes = NULL;
    char *wk_raddrs         = NULL;
    size_t i, w, n;
    ucs_status_t err;
    ucp_address_t **wk_local_addr;
    unsigned int *wk_addr_len;
    ucp_ep_params_t ep_params;

    wk_local_addr = calloc(mca_spml_ucx.ucp_workers, sizeof(ucp_address_t *));
    wk_addr_len = calloc(mca_spml_ucx.ucp_workers, sizeof(size_t));

    mca_spml_ucx_ctx_default.ucp_peers = (ucp_peer_t *) calloc(nprocs, sizeof(*(mca_spml_ucx_ctx_default.ucp_peers)));
    if (NULL == mca_spml_ucx_ctx_default.ucp_peers) {
        goto error;
    }

    rc = mca_spml_ucx_init_put_op_mask(&mca_spml_ucx_ctx_default, nprocs);
    if (OSHMEM_SUCCESS != rc) {
        goto error;
    }

    for (i = 0; i < mca_spml_ucx.ucp_workers; i++) {
        size_t tmp_len;
        err = ucp_worker_get_address(mca_spml_ucx_ctx_default.ucp_worker[i], &wk_local_addr[i], &tmp_len);
        wk_addr_len[i] = (unsigned int)tmp_len;
        if (err != UCS_OK) {
            goto error;
        }
    }

    rc = oshmem_shmem_xchng((void **)wk_local_addr, wk_addr_len, nprocs, (int) mca_spml_ucx.ucp_workers,
                            (void **)&wk_raddrs, &wk_roffs, &wk_rsizes);
    if (rc != OSHMEM_SUCCESS) {
        goto error;
    }

    opal_progress_register(spml_ucx_default_progress);

    mca_spml_ucx.remote_addrs_tbl = (char ***)calloc(mca_spml_ucx.ucp_workers,
                                                     sizeof(mca_spml_ucx.remote_addrs_tbl[0]));
    for (w = 0; w < ucp_workers; w++) {
        mca_spml_ucx.remote_addrs_tbl[w] = (char **)calloc(nprocs, sizeof(mca_spml_ucx.remote_addrs_tbl[w][0]));
    }

    /* Store all remote addresses */
    int offset = 0;
    for (i = 0, n = 0; n < nprocs; n++) {
        for (w = 0; w < ucp_workers; w++, i++) {
            mca_spml_ucx.remote_addrs_tbl[w][n] = (char *)malloc(wk_rsizes[i]);
            memcpy(mca_spml_ucx.remote_addrs_tbl[w][n], (char *)(wk_raddrs + offset), wk_rsizes[i]);
            offset+=wk_rsizes[i];
        }
    }

    /* Get the EP connection requests for all the processes from modex */
    for (n = 0; n < nprocs; ++n) {
        i = (my_rank + n) % nprocs;

        ep_params.field_mask = UCP_EP_PARAM_FIELD_REMOTE_ADDRESS;
        ep_params.address    = (ucp_address_t *)mca_spml_ucx.remote_addrs_tbl[0][i];

        err = ucp_ep_create(mca_spml_ucx_ctx_default.ucp_worker[0], &ep_params,
                &mca_spml_ucx_ctx_default.ucp_peers[i].ucp_conn);
        if (UCS_OK != err) {
            SPML_UCX_ERROR("ucp_ep_create(proc=%zu/%zu) failed: %s", n, nprocs,
                    ucs_status_string(err));
            goto error2;
        }

        /* Initialize mkeys as NULL for all processes */
        mca_spml_ucx_peer_mkey_cache_init(&mca_spml_ucx_ctx_default, i);
    }

    for (i = 0; i < mca_spml_ucx.ucp_workers; i++) {
        ucp_worker_release_address(mca_spml_ucx_ctx_default.ucp_worker[i], wk_local_addr[i]);
    }

    free(wk_raddrs);
    free(wk_rsizes);
    free(wk_roffs);
    free(wk_addr_len);
    free(wk_local_addr);

    SPML_UCX_VERBOSE(50, "*** ADDED PROCS ***");

    opal_common_ucx_mca_proc_added();

    return OSHMEM_SUCCESS;

error2:
    for (i = 0; i < nprocs; ++i) {
         if (mca_spml_ucx_ctx_default.ucp_peers[i].ucp_conn) {
             ucp_ep_destroy(mca_spml_ucx_ctx_default.ucp_peers[i].ucp_conn);
         }
    }

    if (mca_spml_ucx.remote_addrs_tbl) {
        for (w = 0; w < ucp_workers; w++) {
            if (mca_spml_ucx.remote_addrs_tbl[w]) {
                for (n = 0; n < nprocs; n++) {
                    if (mca_spml_ucx.remote_addrs_tbl[w][n]) {
                        free(mca_spml_ucx.remote_addrs_tbl[w][n]);
                    }
                }
                free(mca_spml_ucx.remote_addrs_tbl[w]);
            }
        }
        free(mca_spml_ucx.remote_addrs_tbl);
    }

    mca_spml_ucx_clear_put_op_mask(&mca_spml_ucx_ctx_default);
    if (mca_spml_ucx_ctx_default.ucp_peers)
        free(mca_spml_ucx_ctx_default.ucp_peers);
    free(wk_raddrs);
    free(wk_rsizes);
    free(wk_roffs);
error:
    free(wk_addr_len);
    free(wk_local_addr);
    rc = OSHMEM_ERR_OUT_OF_RESOURCE;
    SPML_UCX_ERROR("add procs FAILED rc=%d", rc);
    return rc;

}

void mca_spml_ucx_rmkey_free(sshmem_mkey_t *mkey, int pe)
{
    spml_ucx_mkey_t   *ucx_mkey;
    uint32_t segno;
    int rc;

    if (!mkey->spml_context) {
        return;
    }
    segno = memheap_find_segnum(mkey->va_base, pe);
    if (MEMHEAP_SEG_INVALID == segno) {
        SPML_UCX_ERROR("mca_spml_ucx_rmkey_free failed because of invalid "
            "segment number: %d\n", segno);
        return;
    }

    ucx_mkey = (spml_ucx_mkey_t *)(mkey->spml_context);
    rc = mca_spml_ucx_ctx_mkey_del(&mca_spml_ucx_ctx_default, pe, segno, ucx_mkey);
    if (OSHMEM_SUCCESS != rc) {
        SPML_UCX_ERROR("mca_spml_ucx_ctx_mkey_del failed\n");
    }
}

void *mca_spml_ucx_rmkey_ptr(const void *dst_addr, sshmem_mkey_t *mkey, int pe)
{
#if (((UCP_API_MAJOR >= 1) && (UCP_API_MINOR >= 3)) || (UCP_API_MAJOR >= 2))
    void *rva;
    ucs_status_t err;
    spml_ucx_mkey_t *ucx_mkey = (spml_ucx_mkey_t *)(mkey->spml_context);

    err = ucp_rkey_ptr(ucx_mkey->rkey, (uint64_t)dst_addr, &rva);
    if (UCS_OK != err) {
        return NULL;
    }
    return rva;
#else
    return NULL;
#endif
}

void mca_spml_ucx_rmkey_unpack(shmem_ctx_t ctx, sshmem_mkey_t *mkey, uint32_t segno, int pe, int tr_id)
{
    spml_ucx_mkey_t   *ucx_mkey;
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;
    int rc;

    rc = mca_spml_ucx_ctx_mkey_add(ucx_ctx, pe, segno, mkey, &ucx_mkey);
    if (OSHMEM_SUCCESS != rc) {
        SPML_UCX_ERROR("mca_spml_ucx_ctx_mkey_cache failed");
        goto error_fatal;
    }
    if (ucx_ctx == &mca_spml_ucx_ctx_default) {
        mkey->spml_context = ucx_mkey;
    }
    return;

error_fatal:
    oshmem_shmem_abort(-1);
    return;
}

void mca_spml_ucx_memuse_hook(void *addr, size_t length)
{
    int my_pe;
    spml_ucx_mkey_t *ucx_mkey;
    ucp_mem_advise_params_t params;
    ucs_status_t status;
    int rc;

    if (!(mca_spml_ucx.heap_reg_nb && memheap_is_va_in_segment(addr, HEAP_SEG_INDEX))) {
        return;
    }

    my_pe = oshmem_my_proc_id();
    rc = mca_spml_ucx_ctx_mkey_by_seg(&mca_spml_ucx_ctx_default, my_pe, HEAP_SEG_INDEX, &ucx_mkey);
    if (OSHMEM_SUCCESS != rc) {
        SPML_UCX_ERROR("mca_spml_ucx_ctx_mkey_by_seg failed");
        return;
    }
    params.field_mask = UCP_MEM_ADVISE_PARAM_FIELD_ADDRESS |
                        UCP_MEM_ADVISE_PARAM_FIELD_LENGTH |
                        UCP_MEM_ADVISE_PARAM_FIELD_ADVICE;

    params.address = addr;
    params.length  = length;
    params.advice  = UCP_MADV_WILLNEED;

    status = ucp_mem_advise(mca_spml_ucx.ucp_context, ucx_mkey->mem_h, &params);
    if (UCS_OK != status) {
        SPML_UCX_ERROR("ucp_mem_advise failed addr %p len %llu : %s",
                       addr, (unsigned long long)length, ucs_status_string(status));
    }
}

sshmem_mkey_t *mca_spml_ucx_register(void* addr,
                                         size_t size,
                                         uint64_t shmid,
                                         int *count)
{
    sshmem_mkey_t *mkeys;
    ucs_status_t status;
    spml_ucx_mkey_t   *ucx_mkey = NULL;
    size_t len;
    ucp_mem_map_params_t mem_map_params;
    uint32_t segno;
    map_segment_t *mem_seg;
    unsigned flags;
    int my_pe = oshmem_my_proc_id();
    int rc;
    ucp_mem_h mem_h;

    *count = 0;
    mkeys = (sshmem_mkey_t *) calloc(1, sizeof(*mkeys));
    if (!mkeys) {
        return NULL;
    }

    segno   = memheap_find_segnum(addr, my_pe);
    if (MEMHEAP_SEG_INVALID == segno) {
        SPML_UCX_ERROR("mca_spml_ucx_register failed because of invalid "
            "segment number: %d\n", segno);
        goto error_out;
    }
    mem_seg = memheap_find_seg(segno);

    /* if possible use mem handle already created by ucx allocator */
    if (MAP_SEGMENT_ALLOC_UCX != mem_seg->type) {
        flags = 0;
        if (mca_spml_ucx.heap_reg_nb && memheap_is_va_in_segment(addr, HEAP_SEG_INDEX)) {
            flags = UCP_MEM_MAP_NONBLOCK;
        }

        mem_map_params.field_mask = UCP_MEM_MAP_PARAM_FIELD_ADDRESS |
                                    UCP_MEM_MAP_PARAM_FIELD_LENGTH |
                                    UCP_MEM_MAP_PARAM_FIELD_FLAGS;
        mem_map_params.address    = addr;
        mem_map_params.length     = size;
        mem_map_params.flags      = flags |
            mca_spml_ucx_mem_map_flags_symmetric_rkey(&mca_spml_ucx);

        status = ucp_mem_map(mca_spml_ucx.ucp_context, &mem_map_params, &mem_h);
        if (UCS_OK != status) {
            goto error_out;
        }

    } else {
        mca_sshmem_ucx_segment_context_t *ctx = mem_seg->context;
        mem_h  = ctx->ucp_memh;
    }

    status = ucp_rkey_pack(mca_spml_ucx.ucp_context, mem_h,
                           &mkeys[SPML_UCX_TRANSP_IDX].u.data, &len);
    if (UCS_OK != status) {
        goto error_out;
    }
    if (len >= 0xffff) {
        SPML_UCX_ERROR("packed rkey is too long: %llu >= %d",
                (unsigned long long)len,
                0xffff);
        oshmem_shmem_abort(-1);
    }
    mkeys[SPML_UCX_TRANSP_IDX].len     = len;
    mkeys[SPML_UCX_TRANSP_IDX].va_base = addr;
    *count = SPML_UCX_TRANSP_CNT;
    rc = mca_spml_ucx_ctx_mkey_add(&mca_spml_ucx_ctx_default, my_pe, segno, &mkeys[SPML_UCX_TRANSP_IDX], &ucx_mkey);
    if (OSHMEM_SUCCESS != rc) {
        SPML_UCX_ERROR("mca_spml_ucx_ctx_mkey_cache failed");
        goto error_unmap;
    }
    ucx_mkey->mem_h = mem_h;
    mkeys[SPML_UCX_TRANSP_IDX].spml_context = ucx_mkey;
    return mkeys;

error_unmap:
    if (NULL != ucx_mkey) {
        ucp_mem_unmap(mca_spml_ucx.ucp_context, ucx_mkey->mem_h);
    }
error_out:
    free(mkeys);

    return NULL ;
}

int mca_spml_ucx_deregister(sshmem_mkey_t *mkeys)
{
    spml_ucx_mkey_t   *ucx_mkey;
    map_segment_t *mem_seg;
    int my_pe = oshmem_my_proc_id();
    int rc;
    uint32_t segno;

    MCA_SPML_CALL(quiet(oshmem_ctx_default));
    if (!mkeys)
        return OSHMEM_SUCCESS;

    if (!mkeys[SPML_UCX_TRANSP_IDX].spml_context)
        return OSHMEM_SUCCESS;

    mem_seg  = memheap_find_va(mkeys[SPML_UCX_TRANSP_IDX].va_base);
    if (OPAL_UNLIKELY(NULL == mem_seg)) {
        return OSHMEM_ERROR;
    }

    segno = memheap_find_segnum(mkeys[SPML_UCX_TRANSP_IDX].va_base, my_pe);
    if (MEMHEAP_SEG_INVALID == segno) {
        SPML_UCX_ERROR("mca_spml_ucx_deregister failed because of invalid "
            "segment number: %d\n", segno);
        return OSHMEM_ERROR;
    }

    ucx_mkey = (spml_ucx_mkey_t*)mkeys[SPML_UCX_TRANSP_IDX].spml_context;

    if (MAP_SEGMENT_ALLOC_UCX != mem_seg->type) {
        ucp_mem_unmap(mca_spml_ucx.ucp_context, ucx_mkey->mem_h);
    }

    rc = mca_spml_ucx_ctx_mkey_del(&mca_spml_ucx_ctx_default, my_pe, segno, ucx_mkey);
    if (OSHMEM_SUCCESS != rc) {
        SPML_UCX_ERROR("mca_spml_ucx_ctx_mkey_del failed\n");
        return rc;
    }
    if (0 < mkeys[SPML_UCX_TRANSP_IDX].len) {
        ucp_rkey_buffer_release(mkeys[SPML_UCX_TRANSP_IDX].u.data);
    }

    free(mkeys);

    return OSHMEM_SUCCESS;
}

static inline void _ctx_add(mca_spml_ucx_ctx_array_t *array, mca_spml_ucx_ctx_t *ctx)
{
    int i;

    if (array->ctxs_count < array->ctxs_num) {
        array->ctxs[array->ctxs_count] = ctx;
    } else {
        array->ctxs = realloc(array->ctxs, (array->ctxs_num + MCA_SPML_UCX_CTXS_ARRAY_INC) * sizeof(mca_spml_ucx_ctx_t *));
        opal_atomic_wmb ();
        for (i = array->ctxs_num; i < array->ctxs_num + MCA_SPML_UCX_CTXS_ARRAY_INC; i++) {
            array->ctxs[i] = NULL;
        }
        array->ctxs[array->ctxs_num] = ctx;
        array->ctxs_num += MCA_SPML_UCX_CTXS_ARRAY_INC;
    }

    opal_atomic_wmb ();
    array->ctxs_count++;
}

static inline void _ctx_remove(mca_spml_ucx_ctx_array_t *array, mca_spml_ucx_ctx_t *ctx, int i)
{
    for (; i < array->ctxs_count; i++) {
        if (array->ctxs[i] == ctx) {
            array->ctxs[i] = array->ctxs[array->ctxs_count-1];
            array->ctxs[array->ctxs_count-1] = NULL;
            array->ctxs_count--;
            break;
        }
    }

    opal_atomic_wmb ();
}

static int mca_spml_ucx_ctx_create_common(long options, mca_spml_ucx_ctx_t **ucx_ctx_p)
{
    ucp_worker_params_t params;
    ucp_ep_params_t ep_params;
    size_t i, nprocs = oshmem_num_procs();
    int j;
    unsigned int cur_ucp_worker = mca_spml_ucx.ucp_worker_cnt++ % mca_spml_ucx.ucp_workers;
    ucs_status_t err;
    spml_ucx_mkey_t *ucx_mkey;
    sshmem_mkey_t *mkey;
    mca_spml_ucx_ctx_t *ucx_ctx;
    int rc = OSHMEM_ERROR;

    ucx_ctx = malloc(sizeof(mca_spml_ucx_ctx_t));
    ucx_ctx->options = options;
    ucx_ctx->ucp_worker = calloc(1, sizeof(ucp_worker_h));
    ucx_ctx->ucp_workers = 1;
    ucx_ctx->synchronized_quiet = mca_spml_ucx_ctx_default.synchronized_quiet;
    ucx_ctx->strong_sync = mca_spml_ucx_ctx_default.strong_sync;      

    params.field_mask  = UCP_WORKER_PARAM_FIELD_THREAD_MODE;
    if (oshmem_mpi_thread_provided == SHMEM_THREAD_SINGLE || options & SHMEM_CTX_PRIVATE || options & SHMEM_CTX_SERIALIZED) {
        params.thread_mode = UCS_THREAD_MODE_SINGLE;
    } else {
        params.thread_mode = UCS_THREAD_MODE_MULTI;
    }

    err = ucp_worker_create(mca_spml_ucx.ucp_context, &params,
                            &ucx_ctx->ucp_worker[0]);
    if (UCS_OK != err) {
        free(ucx_ctx);
        return OSHMEM_ERROR;
    }

    ucx_ctx->ucp_peers = (ucp_peer_t *) calloc(nprocs, sizeof(*(ucx_ctx->ucp_peers)));
    if (NULL == ucx_ctx->ucp_peers) {
        goto error;
    }

    rc = mca_spml_ucx_init_put_op_mask(ucx_ctx, nprocs);
    if (OSHMEM_SUCCESS != rc) {
        goto error2;
    }

    for (i = 0; i < nprocs; i++) {
        ep_params.field_mask = UCP_EP_PARAM_FIELD_REMOTE_ADDRESS;
        ep_params.address    = (ucp_address_t *)(mca_spml_ucx.remote_addrs_tbl[cur_ucp_worker][i]);

        err = ucp_ep_create(ucx_ctx->ucp_worker[0], &ep_params,
                            &ucx_ctx->ucp_peers[i].ucp_conn);
        if (UCS_OK != err) {
            SPML_ERROR("ucp_ep_create(proc=%d/%d) failed: %s", i, nprocs,
                       ucs_status_string(err));
            goto error2;
        }

        for (j = 0; j < memheap_map->n_segments; j++) {
            mkey = &memheap_map->mem_segs[j].mkeys_cache[i][0];
            rc = mca_spml_ucx_ctx_mkey_add(ucx_ctx, i, j, mkey, &ucx_mkey);
            if (OSHMEM_SUCCESS != rc) {
                SPML_UCX_ERROR("mca_spml_ucx_ctx_mkey_add failed");
                goto error2;
            }
        }
    }

    mca_spml_ucx_rkey_store_init(&ucx_ctx->rkey_store);

    *ucx_ctx_p = ucx_ctx;

    return OSHMEM_SUCCESS;

 error2:
    for (i = 0; i < nprocs; i++) {
        if (ucx_ctx->ucp_peers[i].ucp_conn) {
            ucp_ep_destroy(ucx_ctx->ucp_peers[i].ucp_conn);
        }
    }

    mca_spml_ucx_clear_put_op_mask(ucx_ctx);

    if (ucx_ctx->ucp_peers)
        free(ucx_ctx->ucp_peers);

 error:
    ucp_worker_destroy(ucx_ctx->ucp_worker[0]);
    free(ucx_ctx->ucp_worker);
    ucx_ctx->ucp_worker = NULL;
    free(ucx_ctx);
    rc = OSHMEM_ERR_OUT_OF_RESOURCE;
    SPML_ERROR("ctx create FAILED rc=%d", rc);
    return rc;
}

int mca_spml_ucx_ctx_create(long options, shmem_ctx_t *ctx)
{
    mca_spml_ucx_ctx_t *ucx_ctx            = NULL;
    mca_spml_ucx_ctx_array_t *active_array = &mca_spml_ucx.active_array;
    mca_spml_ucx_ctx_array_t *idle_array   = &mca_spml_ucx.idle_array;
    int i = 0, rc = OSHMEM_SUCCESS;

    /* Take a lock controlling context creation. AUX context may set specific
     * UCX parameters affecting worker creation, which are not needed for
     * regular contexts. */

    /* Check if we have an idle context to reuse */
    SHMEM_MUTEX_LOCK(mca_spml_ucx.internal_mutex);
    for (i = 0; i < idle_array->ctxs_count; i++) {
        if (idle_array->ctxs[i]->options & options) {
            ucx_ctx = idle_array->ctxs[i];
            _ctx_remove(idle_array, ucx_ctx, i);
            break;
        }
    }
    SHMEM_MUTEX_UNLOCK(mca_spml_ucx.internal_mutex);

    /* If we cannot reuse, create new ctx */
    if (ucx_ctx == NULL) {
        pthread_mutex_lock(&mca_spml_ucx.ctx_create_mutex);
        rc = mca_spml_ucx_ctx_create_common(options, &ucx_ctx);
        pthread_mutex_unlock(&mca_spml_ucx.ctx_create_mutex);
        if (rc != OSHMEM_SUCCESS) {
            return rc;
        }
    }
    
    if (!(options & SHMEM_CTX_PRIVATE)) {
        SHMEM_MUTEX_LOCK(mca_spml_ucx.internal_mutex);
        _ctx_add(active_array, ucx_ctx);
        if (active_array->ctxs_count == 0) {
            opal_progress_register(spml_ucx_ctx_progress);
        }
        SHMEM_MUTEX_UNLOCK(mca_spml_ucx.internal_mutex);
    }

    (*ctx) = (shmem_ctx_t)ucx_ctx;
    return OSHMEM_SUCCESS;
}

void mca_spml_ucx_ctx_destroy(shmem_ctx_t ctx)
{
    MCA_SPML_CALL(quiet(ctx));

    SHMEM_MUTEX_LOCK(mca_spml_ucx.internal_mutex);
    if (!(((mca_spml_ucx_ctx_t *)ctx)->options & SHMEM_CTX_PRIVATE)) {
        _ctx_remove(&mca_spml_ucx.active_array, (mca_spml_ucx_ctx_t *)ctx, 0);
    }
    _ctx_add(&mca_spml_ucx.idle_array, (mca_spml_ucx_ctx_t *)ctx);
    if (!mca_spml_ucx.active_array.ctxs_count) {
        opal_progress_unregister(spml_ucx_ctx_progress);
    }
    SHMEM_MUTEX_UNLOCK(mca_spml_ucx.internal_mutex);
}

int mca_spml_ucx_get(shmem_ctx_t ctx, void *src_addr, size_t size, void *dst_addr, int src)
{
    void *rva = NULL;
    spml_ucx_mkey_t *ucx_mkey = mca_spml_ucx_ctx_mkey_by_va(ctx, src, src_addr, &rva, &mca_spml_ucx);
    assert(NULL != ucx_mkey);
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;
#if (HAVE_DECL_UCP_GET_NBX || HAVE_DECL_UCP_GET_NB)
    ucs_status_ptr_t request;
#else
    ucs_status_t status;
#endif

#if HAVE_DECL_UCP_GET_NBX
    request = ucp_get_nbx(ucx_ctx->ucp_peers[src].ucp_conn, dst_addr, size,
                          (uint64_t)rva, ucx_mkey->rkey, &mca_spml_ucx_request_param_b);
    return opal_common_ucx_wait_request(request, ucx_ctx->ucp_worker[0], "ucp_get_nbx");
#elif HAVE_DECL_UCP_GET_NB
    request = ucp_get_nb(ucx_ctx->ucp_peers[src].ucp_conn, dst_addr, size,
                         (uint64_t)rva, ucx_mkey->rkey, opal_common_ucx_empty_complete_cb);
    return opal_common_ucx_wait_request(request, ucx_ctx->ucp_worker[0], "ucp_get_nb");
#else
    status = ucp_get(ucx_ctx->ucp_peers[src].ucp_conn, dst_addr, size,
                     (uint64_t)rva, ucx_mkey->rkey);
    return ucx_status_to_oshmem(status);
#endif
}

int mca_spml_ucx_get_nb(shmem_ctx_t ctx, void *src_addr, size_t size, void *dst_addr, int src, void **handle)
{
    void *rva = NULL;
    ucs_status_t status;
    spml_ucx_mkey_t *ucx_mkey = mca_spml_ucx_ctx_mkey_by_va(ctx, src, src_addr, &rva, &mca_spml_ucx);
    assert(NULL != ucx_mkey);
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;
#if HAVE_DECL_UCP_GET_NBX
    ucs_status_ptr_t status_ptr;
#endif

#if HAVE_DECL_UCP_GET_NBX
    status_ptr = ucp_get_nbx(ucx_ctx->ucp_peers[src].ucp_conn, dst_addr, size,
                             (uint64_t)rva, ucx_mkey->rkey, &mca_spml_ucx_request_param);
    if (UCS_PTR_IS_PTR(status_ptr)) {
        ucp_request_free(status_ptr);
        status = UCS_INPROGRESS;
    } else {
        status = UCS_PTR_STATUS(status_ptr);
    }
#else
    status = ucp_get_nbi(ucx_ctx->ucp_peers[src].ucp_conn, dst_addr, size,
                     (uint64_t)rva, ucx_mkey->rkey);
#endif
    return ucx_status_to_oshmem_nb(status);
}

int mca_spml_ucx_get_nb_wprogress(shmem_ctx_t ctx, void *src_addr, size_t size, void *dst_addr, int src, void **handle)
{
    unsigned int i;
    void *rva = NULL;
    ucs_status_t status;
    spml_ucx_mkey_t *ucx_mkey = mca_spml_ucx_ctx_mkey_by_va(ctx, src, src_addr, &rva, &mca_spml_ucx);
    assert(NULL != ucx_mkey);
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;
#if HAVE_DECL_UCP_GET_NBX
    ucs_status_ptr_t status_ptr;
#endif

#if HAVE_DECL_UCP_GET_NBX
    status_ptr = ucp_get_nbx(ucx_ctx->ucp_peers[src].ucp_conn,
                             dst_addr, size, (uint64_t)rva,
                             ucx_mkey->rkey, &mca_spml_ucx_request_param);
    if (UCS_PTR_IS_PTR(status_ptr)) {
        ucp_request_free(status_ptr);
        status = UCS_INPROGRESS;
    } else {
        status = UCS_PTR_STATUS(status_ptr);
    }
#else
    status = ucp_get_nbi(ucx_ctx->ucp_peers[src].ucp_conn, dst_addr, size,
                     (uint64_t)rva, ucx_mkey->rkey);
#endif

    if (++ucx_ctx->nb_progress_cnt > mca_spml_ucx.nb_get_progress_thresh) {
        for (i = 0; i < mca_spml_ucx.nb_ucp_worker_progress; i++) {
            if (!ucp_worker_progress(ucx_ctx->ucp_worker[0])) {
                ucx_ctx->nb_progress_cnt = 0;
                break;
            }
        }
    }

    return ucx_status_to_oshmem_nb(status);
}

int mca_spml_ucx_put(shmem_ctx_t ctx, void* dst_addr, size_t size, void* src_addr, int dst)
{
    void *rva = NULL;
    spml_ucx_mkey_t *ucx_mkey = mca_spml_ucx_ctx_mkey_by_va(ctx, dst, dst_addr, &rva, &mca_spml_ucx);
    assert(NULL != ucx_mkey);
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;
    int res;
#if (HAVE_DECL_UCP_PUT_NBX || HAVE_DECL_UCP_PUT_NB)
    ucs_status_ptr_t request;
#else
    ucs_status_t status;
#endif

#if HAVE_DECL_UCP_PUT_NBX
    request = ucp_put_nbx(ucx_ctx->ucp_peers[dst].ucp_conn, src_addr, size,
                          (uint64_t)rva, ucx_mkey->rkey, &mca_spml_ucx_request_param_b);
    res = opal_common_ucx_wait_request(request, ucx_ctx->ucp_worker[0], "ucp_put_nbx");
#elif HAVE_DECL_UCP_PUT_NB
    request = ucp_put_nb(ucx_ctx->ucp_peers[dst].ucp_conn, src_addr, size,
                         (uint64_t)rva, ucx_mkey->rkey, opal_common_ucx_empty_complete_cb);
    res = opal_common_ucx_wait_request(request, ucx_ctx->ucp_worker[0], "ucp_put_nb");
#else
    status = ucp_put(ucx_ctx->ucp_peers[dst].ucp_conn, src_addr, size,
                     (uint64_t)rva, ucx_mkey->rkey);
    res = ucx_status_to_oshmem(status);
#endif

    if (OPAL_LIKELY(OSHMEM_SUCCESS == res)) {
        mca_spml_ucx_remote_op_posted(ucx_ctx, dst);
    }

    return res;
}

int mca_spml_ucx_put_nb(shmem_ctx_t ctx, void* dst_addr, size_t size, void* src_addr, int dst, void **handle)
{
    void *rva = NULL;
    spml_ucx_mkey_t *ucx_mkey = mca_spml_ucx_ctx_mkey_by_va(ctx, dst, dst_addr, &rva, &mca_spml_ucx);
    assert(NULL != ucx_mkey);
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;
    ucs_status_t status;
#if HAVE_DECL_UCP_PUT_NBX
    ucs_status_ptr_t status_ptr;
#endif

#if HAVE_DECL_UCP_PUT_NBX
    status_ptr = ucp_put_nbx(ucx_ctx->ucp_peers[dst].ucp_conn,
                             src_addr, size, (uint64_t)rva,
                             ucx_mkey->rkey, &mca_spml_ucx_request_param);
    if (UCS_PTR_IS_PTR(status_ptr)) {
        ucp_request_free(status_ptr);
        status = UCS_INPROGRESS;
    } else {
        status = UCS_PTR_STATUS(status_ptr);
    }
#else
    status = ucp_put_nbi(ucx_ctx->ucp_peers[dst].ucp_conn, src_addr, size,
                     (uint64_t)rva, ucx_mkey->rkey);
#endif
    if (OPAL_LIKELY(status >= 0)) {
        mca_spml_ucx_remote_op_posted(ucx_ctx, dst);
    }

    return ucx_status_to_oshmem_nb(status);
}

int mca_spml_ucx_put_nb_wprogress(shmem_ctx_t ctx, void* dst_addr, size_t size, void* src_addr, int dst, void **handle)
{
    unsigned int i;
    void *rva = NULL;
    ucs_status_t status;
    spml_ucx_mkey_t *ucx_mkey = mca_spml_ucx_ctx_mkey_by_va(ctx, dst, dst_addr, &rva, &mca_spml_ucx);
    assert(NULL != ucx_mkey);
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;
#if HAVE_DECL_UCP_PUT_NBX
    ucs_status_ptr_t status_ptr;
#endif

#if HAVE_DECL_UCP_PUT_NBX
    status_ptr = ucp_put_nbx(ucx_ctx->ucp_peers[dst].ucp_conn, src_addr, size,
                             (uint64_t)rva, ucx_mkey->rkey,
                             &mca_spml_ucx_request_param);
    if (UCS_PTR_IS_PTR(status_ptr)) {
        ucp_request_free(status_ptr);
        status = UCS_INPROGRESS;
    } else {
        status = UCS_PTR_STATUS(status_ptr);
    }
#else
    status = ucp_put_nbi(ucx_ctx->ucp_peers[dst].ucp_conn, src_addr, size,
                     (uint64_t)rva, ucx_mkey->rkey);
#endif
    if (OPAL_LIKELY(status >= 0)) {
        mca_spml_ucx_remote_op_posted(ucx_ctx, dst);
    }

    if (++ucx_ctx->nb_progress_cnt > mca_spml_ucx.nb_put_progress_thresh) {
        for (i = 0; i < mca_spml_ucx.nb_ucp_worker_progress; i++) {
            if (!ucp_worker_progress(ucx_ctx->ucp_worker[0])) {
                ucx_ctx->nb_progress_cnt = 0;
                break;
            }
        }
    }

    return ucx_status_to_oshmem_nb(status);
}

static int mca_spml_ucx_strong_sync(shmem_ctx_t ctx)
{
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;
    ucs_status_ptr_t request;
    static int flush_get_data;
    unsigned i;
    int ret;
    int idx;
#if !(HAVE_DECL_UCP_EP_FLUSH_NBX || HAVE_DECL_UCP_EP_FLUSH_NB)
    ucs_status_t status;
#endif

    for (i = 0; i < ucx_ctx->put_proc_count; i++) {
        idx = ucx_ctx->put_proc_indexes[i];

        switch (ucx_ctx->strong_sync) {
        case SPML_UCX_STRONG_ORDERING_NONE:
        case SPML_UCX_STRONG_ORDERING_GETNB:
            ret = mca_spml_ucx_get_nb(ctx,
                                      ucx_ctx->ucp_peers[idx].mkeys[SPML_UCX_SERVICE_SEG]->super.super.va_base,
                                      sizeof(flush_get_data), &flush_get_data, idx, NULL);
            break;
        case SPML_UCX_STRONG_ORDERING_GET:
            ret = mca_spml_ucx_get(ctx,
                                   ucx_ctx->ucp_peers[idx].mkeys[SPML_UCX_SERVICE_SEG]->super.super.va_base,
                                   sizeof(flush_get_data), &flush_get_data, idx);
            break;
#if HAVE_DECL_UCP_EP_FLUSH_NBX
        case SPML_UCX_STRONG_ORDERING_FLUSH:
            request = ucp_ep_flush_nbx(ucx_ctx->ucp_peers[idx].ucp_conn,
                                       &mca_spml_ucx_request_param_b);
            ret = opal_common_ucx_wait_request(request, ucx_ctx->ucp_worker[0], "ucp_flush_nbx");
#elif HAVE_DECL_UCP_EP_FLUSH_NB
            request = ucp_ep_flush_nb(ucx_ctx->ucp_peers[idx].ucp_conn, 0, opal_common_ucx_empty_complete_cb);
            ret = opal_common_ucx_wait_request(request, ucx_ctx->ucp_worker[0], "ucp_flush_nb");
#else
            status = ucp_ep_flush(ucx_ctx->ucp_peers[idx].ucp_conn);
            ret = (status == UCS_OK) ? OPAL_SUCCESS : OPAL_ERROR;
#endif
            break;
        default:
            /* unknown mode */
            ret = OMPI_SUCCESS;
            break;
        }

        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            oshmem_shmem_abort(-1);
            return ret;
        }

        opal_bitmap_clear_bit(&ucx_ctx->put_op_bitmap, idx);
    }

    ucx_ctx->put_proc_count = 0;
    return OSHMEM_SUCCESS;
}

int mca_spml_ucx_fence(shmem_ctx_t ctx)
{
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;
    ucs_status_t err;
    int ret;
    unsigned int i = 0;

    opal_atomic_wmb();

    if (ucx_ctx->strong_sync != SPML_UCX_STRONG_ORDERING_NONE) {
        ret = mca_spml_ucx_strong_sync(ctx);
        if (ret != OSHMEM_SUCCESS) {
            oshmem_shmem_abort(-1);
            return ret;
        }
    }

    for (i=0; i < ucx_ctx->ucp_workers; i++) {
        if (ucx_ctx->ucp_worker[i] != NULL) {
            err = ucp_worker_fence(ucx_ctx->ucp_worker[i]);
            if (UCS_OK != err) {
                 SPML_UCX_ERROR("fence failed: %s", ucs_status_string(err));
                 oshmem_shmem_abort(-1);
                 return OSHMEM_ERROR;
            }
        }
    }
    return OSHMEM_SUCCESS;
}

int mca_spml_ucx_quiet(shmem_ctx_t ctx)
{
    int ret;
    unsigned i;
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;

    if (ucx_ctx->synchronized_quiet) {
        ret = mca_spml_ucx_strong_sync(ctx);
        if (ret != OSHMEM_SUCCESS) {
            oshmem_shmem_abort(-1);
            return ret;
        }
    }

    opal_atomic_wmb();

    for (i = 0; i < ucx_ctx->ucp_workers; i++) {
        if (ucx_ctx->ucp_worker[i] != NULL) {
            ret = opal_common_ucx_worker_flush(ucx_ctx->ucp_worker[i]);
            if (OMPI_SUCCESS != ret) {
                 oshmem_shmem_abort(-1);
                 return ret;
            }
        }
    }

    /* If put_all_nb op/s is/are being executed asynchronously, need to wait its
     * completion as well. */
    if (ctx == oshmem_ctx_default) {
        while (mca_spml_ucx.aux_refcnt) {
            opal_progress();
        }
    }

    ucx_ctx->nb_progress_cnt = 0;

    return OSHMEM_SUCCESS;
}

/* blocking receive */
int mca_spml_ucx_recv(void* buf, size_t size, int src)
{
    int rc = OSHMEM_SUCCESS;

    rc = MCA_PML_CALL(recv(buf,
                size,
                &(ompi_mpi_unsigned_char.dt),
                src,
                0,
                &(ompi_mpi_comm_world.comm),
                NULL));

    return rc;
}

/* for now only do blocking copy send */
int mca_spml_ucx_send(void* buf,
                        size_t size,
                        int dst,
                        mca_spml_base_put_mode_t mode)
{
    int rc = OSHMEM_SUCCESS;

    rc = MCA_PML_CALL(send(buf,
                size,
                &(ompi_mpi_unsigned_char.dt),
                dst,
                0,
                (mca_pml_base_send_mode_t)mode,
                &(ompi_mpi_comm_world.comm)));

    return rc;
}

/* this can be called with request==NULL in case of immediate completion */
static void mca_spml_ucx_put_all_complete_cb(void *request, ucs_status_t status)
{
    if (mca_spml_ucx.async_progress && (--mca_spml_ucx.aux_refcnt == 0)) {
        opal_event_evtimer_del(mca_spml_ucx.tick_event);
        opal_progress_unregister(spml_ucx_progress_aux_ctx);
    }

    if (request != NULL) {
        ucp_request_free(request);
    }
}

/* Should be called with AUX lock taken */
static int mca_spml_ucx_create_aux_ctx(void)
{
    unsigned major      = 0;
    unsigned minor      = 0;
    unsigned rel_number = 0;
    int rc;
    bool rand_dci_supp;

    ucp_get_version(&major, &minor, &rel_number);
    rand_dci_supp = UCX_VERSION(major, minor, rel_number) >= UCX_VERSION(1, 6, 0);

    if (rand_dci_supp) {
        pthread_mutex_lock(&mca_spml_ucx.ctx_create_mutex);
        opal_setenv("UCX_DC_MLX5_TX_POLICY", "rand", 0, &environ);
    }

    rc = mca_spml_ucx_ctx_create_common(SHMEM_CTX_PRIVATE, &mca_spml_ucx.aux_ctx);

    if (rand_dci_supp) {
        opal_unsetenv("UCX_DC_MLX5_TX_POLICY", &environ);
        pthread_mutex_unlock(&mca_spml_ucx.ctx_create_mutex);
    }

    return rc;
}

int mca_spml_ucx_put_all_nb(void *dest, const void *source, size_t size, long *counter)
{
    int my_pe = oshmem_my_proc_id();
    long val  = 1;
    int peer, dst_pe, rc;
    shmem_ctx_t ctx;
    struct timeval tv;
    void *request;

    mca_spml_ucx_aux_lock();
    if (mca_spml_ucx.async_progress) {
        if (mca_spml_ucx.aux_ctx == NULL) {
            rc = mca_spml_ucx_create_aux_ctx();
            if (rc != OMPI_SUCCESS) {
                mca_spml_ucx_aux_unlock();
                oshmem_shmem_abort(-1);
            }
        }

        if (mca_spml_ucx.aux_refcnt++ == 0) {
            tv.tv_sec  = 0;
            tv.tv_usec = mca_spml_ucx.async_tick;
            opal_event_evtimer_add(mca_spml_ucx.tick_event, &tv);
            opal_progress_register(spml_ucx_progress_aux_ctx);
        }
        ctx = (shmem_ctx_t)mca_spml_ucx.aux_ctx;
    } else {
        ctx = oshmem_ctx_default;
    }

    assert(ctx != NULL); /* make coverity happy */

    for (peer = 0; peer < oshmem_num_procs(); peer++) {
        dst_pe = (peer + my_pe) % oshmem_num_procs();
        rc = mca_spml_ucx_put_nb(ctx,
                                 (void*)((uintptr_t)dest + my_pe * size),
                                 size,
                                 (void*)((uintptr_t)source + dst_pe * size),
                                 dst_pe, NULL);
        RUNTIME_CHECK_RC(rc);

        mca_spml_ucx_fence(ctx);

        rc = MCA_ATOMIC_CALL(add(ctx, (void*)counter, val, sizeof(val), dst_pe));
        RUNTIME_CHECK_RC(rc);
    }

    request = ucp_worker_flush_nb(((mca_spml_ucx_ctx_t*)ctx)->ucp_worker[0], 0,
                                  mca_spml_ucx_put_all_complete_cb);
    if (!UCS_PTR_IS_PTR(request)) {
        mca_spml_ucx_put_all_complete_cb(NULL, UCS_PTR_STATUS(request));
    }

    mca_spml_ucx_aux_unlock();

    return OSHMEM_SUCCESS;
}

/* This routine is not implemented */
int mca_spml_ucx_put_signal(shmem_ctx_t ctx, void* dst_addr, size_t size, void*
        src_addr, uint64_t *sig_addr, uint64_t signal, int sig_op, int dst)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_put_signal_nb(shmem_ctx_t ctx, void* dst_addr, size_t size,
        void* src_addr, uint64_t *sig_addr, uint64_t signal, int sig_op, int
        dst)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
void mca_spml_ucx_wait_until_all(void *ivars, int cmp, void
        *cmp_value, size_t nelems, const int *status, int datatype)
{
    return ;
}

/* This routine is not implemented */
size_t mca_spml_ucx_wait_until_any(void *ivars, int cmp, void
        *cmp_value, size_t nelems, const int *status, int datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
size_t mca_spml_ucx_wait_until_some(void *ivars, int cmp, void
        *cmp_value, size_t nelems, size_t *indices, const int *status, int
        datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */ 
void mca_spml_ucx_wait_until_all_vector(void *ivars, int cmp, void
        *cmp_values, size_t nelems, const int *status, int datatype)
{
    return ;
}

/* This routine is not implemented */
size_t mca_spml_ucx_wait_until_any_vector(void *ivars, int cmp, void
        *cmp_value, size_t nelems, const int *status, int datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
size_t mca_spml_ucx_wait_until_some_vector(void *ivars, int cmp, void
        *cmp_value, size_t nelems, size_t *indices, const int *status, int
        datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_test_all(void *ivars, int cmp, void *cmp_value,
        size_t nelems, const int *status, int datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
size_t mca_spml_ucx_test_any(void *ivars, int cmp, void *cmp_value,
        size_t nelems, const int *status, int datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
size_t mca_spml_ucx_test_some(void *ivars, int cmp, void *cmp_value,
        size_t nelems, size_t *indices, const int *status, int datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_test_all_vector(void *ivars, int cmp, void
        *cmp_values, size_t nelems, const int *status, int datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_test_any_vector(void *ivars, int cmp, void
        *cmp_values, size_t nelems, const int *status, int datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_test_some_vector(void *ivars, int cmp, void
        *cmp_values, size_t nelems, size_t *indices, const int *status, int
        datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_sync(shmem_team_t team)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_my_pe(shmem_team_t team)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_n_pes(shmem_team_t team)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_get_config(shmem_team_t team, long config_mask,
        shmem_team_config_t *config)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_translate_pe(shmem_team_t src_team, int src_pe,
        shmem_team_t dest_team)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_split_strided(shmem_team_t parent_team, int start, int
        stride, int size, const shmem_team_config_t *config, long config_mask,
        shmem_team_t *new_team)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_split_2d(shmem_team_t parent_team, int xrange, const
        shmem_team_config_t *xaxis_config, long xaxis_mask, shmem_team_t
        *xaxis_team, const shmem_team_config_t *yaxis_config, long yaxis_mask,
        shmem_team_t *yaxis_team)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_destroy(shmem_team_t team)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_get(shmem_ctx_t ctx, shmem_team_t *team)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_create_ctx(shmem_team_t team, long options, shmem_ctx_t *ctx)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_alltoall(shmem_team_t team, void
        *dest, const void *source, size_t nelems, int datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_alltoalls(shmem_team_t team, void
        *dest, const void *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems,
        int datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_broadcast(shmem_team_t team, void
        *dest, const void *source, size_t nelems, int PE_root, int datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_collect(shmem_team_t team, void
        *dest, const void *source, size_t nelems, int datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_fcollect(shmem_team_t team, void
        *dest, const void *source, size_t nelems, int datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}

/* This routine is not implemented */
int mca_spml_ucx_team_reduce(shmem_team_t team, void
        *dest, const void *source, size_t nreduce, int operation, int datatype)
{
    return OSHMEM_ERR_NOT_IMPLEMENTED;
}


