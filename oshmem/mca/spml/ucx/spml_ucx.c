/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "orte/include/orte/types.h"
#include "orte/runtime/orte_globals.h"
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
#include "orte/util/show_help.h"

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
        .spml_get           = mca_spml_ucx_get,
        .spml_get_nb        = mca_spml_ucx_get_nb,
        .spml_recv          = mca_spml_ucx_recv,
        .spml_send          = mca_spml_ucx_send,
        .spml_wait          = mca_spml_base_wait,
        .spml_wait_nb       = mca_spml_base_wait_nb,
        .spml_test          = mca_spml_base_test,
        .spml_fence         = mca_spml_ucx_fence,
        .spml_quiet         = mca_spml_ucx_quiet,
        .spml_rmkey_unpack  = mca_spml_ucx_rmkey_unpack,
        .spml_rmkey_free    = mca_spml_ucx_rmkey_free,
        .spml_rmkey_ptr     = mca_spml_ucx_rmkey_ptr,
        .spml_memuse_hook   = mca_spml_ucx_memuse_hook,
        .spml_put_all_nb    = mca_spml_ucx_put_all_nb,
        .self               = (void*)&mca_spml_ucx
    },

    .ucp_context            = NULL,
    .num_disconnect         = 1,
    .heap_reg_nb            = 0,
    .enabled                = 0,
    .get_mkey_slow          = NULL,
    .synchronized_quiet     = false
};

mca_spml_ucx_ctx_t mca_spml_ucx_ctx_default = {
    .ucp_worker = NULL,
    .ucp_peers  = NULL,
    .options    = 0
};

int mca_spml_ucx_enable(bool enable)
{
    SPML_UCX_VERBOSE(50, "*** ucx ENABLED ****");
    if (false == enable) {
        return OSHMEM_SUCCESS;
    }

    mca_spml_ucx.enabled = true;

    return OSHMEM_SUCCESS;
}

int mca_spml_ucx_del_procs(ompi_proc_t** procs, size_t nprocs)
{
    opal_common_ucx_del_proc_t *del_procs;
    size_t i;
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
    }

    ret = opal_common_ucx_del_procs_nofence(del_procs, nprocs, oshmem_my_proc_id(),
                                            mca_spml_ucx.num_disconnect,
                                            mca_spml_ucx_ctx_default.ucp_worker);
    /* No need to barrier here - barrier is called in _shmem_finalize */
    free(del_procs);
    free(mca_spml_ucx.remote_addrs_tbl);
    free(mca_spml_ucx_ctx_default.ucp_peers);

    mca_spml_ucx_ctx_default.ucp_peers = NULL;

    return ret;
}

/* TODO: move func into common place, use it with rkey exchng too */
static int oshmem_shmem_xchng(
        void *local_data, int local_size, int nprocs,
        void **rdata_p, int **roffsets_p, int **rsizes_p)
{
    int *rcv_sizes   = NULL;
    int *rcv_offsets = NULL; 
    void *rcv_buf    = NULL;
    int rc;
    int i;

    /* do llgatherv */
    rcv_offsets = malloc(nprocs * sizeof(*rcv_offsets));
    if (NULL == rcv_offsets) {
        goto err;
    }

    /* todo: move into separate function. do allgatherv */
    rcv_sizes = malloc(nprocs * sizeof(*rcv_sizes));
    if (NULL == rcv_sizes) {
        goto err;
    }
    
    rc = oshmem_shmem_allgather(&local_size, rcv_sizes, sizeof(int));
    if (MPI_SUCCESS != rc) {
        goto err;
    }

    /* calculate displacements */
    rcv_offsets[0] = 0;
    for (i = 1; i < nprocs; i++) {
        rcv_offsets[i] = rcv_offsets[i - 1] + rcv_sizes[i - 1];
    }

    rcv_buf = malloc(rcv_offsets[nprocs - 1] + rcv_sizes[nprocs - 1]);
    if (NULL == rcv_buf) {
        goto err;
    }

    rc = oshmem_shmem_allgatherv(local_data, rcv_buf, local_size, rcv_sizes, rcv_offsets);
    if (MPI_SUCCESS != rc) {
        goto err;
    }

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

static void dump_address(int pe, char *addr, size_t len)
{
#ifdef SPML_UCX_DEBUG
    int my_rank = oshmem_my_proc_id();
    unsigned  i;

    printf("me=%d dest_pe=%d addr=%p len=%d\n", my_rank, pe, addr, len);
    for (i = 0; i < len; i++) {
        printf("%02X ", (unsigned)0xFF&addr[i]);
    }
    printf("\n");
#endif
}

static char spml_ucx_transport_ids[1] = { 0 };

int mca_spml_ucx_init_put_op_mask(mca_spml_ucx_ctx_t *ctx, size_t nprocs)
{
    int res;

    if (mca_spml_ucx.synchronized_quiet) {
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
    if (mca_spml_ucx.synchronized_quiet && ctx->put_proc_indexes) {
        OBJ_DESTRUCT(&ctx->put_op_bitmap);
        free(ctx->put_proc_indexes);
    }

    return OSHMEM_SUCCESS;
}

int mca_spml_ucx_add_procs(ompi_proc_t** procs, size_t nprocs)
{
    size_t i, j, n;
    int rc = OSHMEM_ERROR;
    int my_rank = oshmem_my_proc_id();
    ucs_status_t err;
    ucp_address_t *wk_local_addr;
    size_t wk_addr_len;
    int *wk_roffs = NULL;
    int *wk_rsizes = NULL;
    char *wk_raddrs = NULL;
    ucp_ep_params_t ep_params;


    mca_spml_ucx_ctx_default.ucp_peers = (ucp_peer_t *) calloc(nprocs, sizeof(*(mca_spml_ucx_ctx_default.ucp_peers)));
    if (NULL == mca_spml_ucx_ctx_default.ucp_peers) {
        goto error;
    }

    rc = mca_spml_ucx_init_put_op_mask(&mca_spml_ucx_ctx_default, nprocs);
    if (OSHMEM_SUCCESS != rc) {
        goto error;
    }

    err = ucp_worker_get_address(mca_spml_ucx_ctx_default.ucp_worker, &wk_local_addr, &wk_addr_len);
    if (err != UCS_OK) {
        goto error;
    }
    dump_address(my_rank, (char *)wk_local_addr, wk_addr_len);

    rc = oshmem_shmem_xchng(wk_local_addr, wk_addr_len, nprocs,
                            (void **)&wk_raddrs, &wk_roffs, &wk_rsizes);
    if (rc != OSHMEM_SUCCESS) {
        goto error;
    }

    opal_progress_register(spml_ucx_default_progress);

    mca_spml_ucx.remote_addrs_tbl = (char **)calloc(nprocs, sizeof(char *));
    memset(mca_spml_ucx.remote_addrs_tbl, 0, nprocs * sizeof(char *));

    /* Get the EP connection requests for all the processes from modex */
    for (n = 0; n < nprocs; ++n) {
        i = (my_rank + n) % nprocs;
        dump_address(i, (char *)(wk_raddrs + wk_roffs[i]), wk_rsizes[i]);

        ep_params.field_mask = UCP_EP_PARAM_FIELD_REMOTE_ADDRESS;
        ep_params.address    = (ucp_address_t *)(wk_raddrs + wk_roffs[i]);

        err = ucp_ep_create(mca_spml_ucx_ctx_default.ucp_worker, &ep_params,
                            &mca_spml_ucx_ctx_default.ucp_peers[i].ucp_conn);
        if (UCS_OK != err) {
            SPML_UCX_ERROR("ucp_ep_create(proc=%zu/%zu) failed: %s", n, nprocs,
                           ucs_status_string(err));
            goto error2;
        }

        OSHMEM_PROC_DATA(procs[i])->num_transports = 1;
        OSHMEM_PROC_DATA(procs[i])->transport_ids = spml_ucx_transport_ids;

        for (j = 0; j < MCA_MEMHEAP_MAX_SEGMENTS; j++) {
            mca_spml_ucx_ctx_default.ucp_peers[i].mkeys[j].key.rkey = NULL;
        }

        mca_spml_ucx.remote_addrs_tbl[i] = (char *)malloc(wk_rsizes[i]);
        memcpy(mca_spml_ucx.remote_addrs_tbl[i], (char *)(wk_raddrs + wk_roffs[i]),
               wk_rsizes[i]);
    }

    ucp_worker_release_address(mca_spml_ucx_ctx_default.ucp_worker, wk_local_addr);
    free(wk_raddrs);
    free(wk_rsizes);
    free(wk_roffs);

    SPML_UCX_VERBOSE(50, "*** ADDED PROCS ***");

    opal_common_ucx_mca_proc_added();
    return OSHMEM_SUCCESS;

error2:
    for (i = 0; i < nprocs; ++i) {
         if (mca_spml_ucx_ctx_default.ucp_peers[i].ucp_conn) {
             ucp_ep_destroy(mca_spml_ucx_ctx_default.ucp_peers[i].ucp_conn);
         }
         if (mca_spml_ucx.remote_addrs_tbl[i]) {
             free(mca_spml_ucx.remote_addrs_tbl[i]);
         }
    }

    mca_spml_ucx_clear_put_op_mask(&mca_spml_ucx_ctx_default);
    if (mca_spml_ucx_ctx_default.ucp_peers)
        free(mca_spml_ucx_ctx_default.ucp_peers);
    if (mca_spml_ucx.remote_addrs_tbl)
        free(mca_spml_ucx.remote_addrs_tbl);
    free(wk_raddrs);
    free(wk_rsizes);
    free(wk_roffs);
error:
    rc = OSHMEM_ERR_OUT_OF_RESOURCE;
    SPML_UCX_ERROR("add procs FAILED rc=%d", rc);
    return rc;

}

void mca_spml_ucx_rmkey_free(sshmem_mkey_t *mkey)
{
    spml_ucx_mkey_t   *ucx_mkey;

    if (!mkey->spml_context) {
        return;
    }
    ucx_mkey = (spml_ucx_mkey_t *)(mkey->spml_context);
    ucp_rkey_destroy(ucx_mkey->rkey);
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
    ucs_status_t err;
    
    ucx_mkey = &ucx_ctx->ucp_peers[pe].mkeys[segno].key;

    err = ucp_ep_rkey_unpack(ucx_ctx->ucp_peers[pe].ucp_conn,
            mkey->u.data,
            &ucx_mkey->rkey); 
    if (UCS_OK != err) {
        SPML_UCX_ERROR("failed to unpack rkey: %s", ucs_status_string(err));
        goto error_fatal;
    }

    if (ucx_ctx == &mca_spml_ucx_ctx_default) {
        mkey->spml_context = ucx_mkey;
    }
    mca_spml_ucx_cache_mkey(ucx_ctx, mkey, segno, pe);
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

    if (!(mca_spml_ucx.heap_reg_nb && memheap_is_va_in_segment(addr, HEAP_SEG_INDEX))) {
        return;
    }

    my_pe    = oshmem_my_proc_id();
    ucx_mkey = &mca_spml_ucx_ctx_default.ucp_peers[my_pe].mkeys[HEAP_SEG_INDEX].key;

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
    spml_ucx_mkey_t   *ucx_mkey;
    size_t len;
    ucp_mem_map_params_t mem_map_params;
    int segno;
    map_segment_t *mem_seg;
    unsigned flags;
    int my_pe = oshmem_my_proc_id();

    *count = 0;
    mkeys = (sshmem_mkey_t *) calloc(1, sizeof(*mkeys));
    if (!mkeys) {
        return NULL;
    }

    segno   = memheap_find_segnum(addr);
    mem_seg = memheap_find_seg(segno);

    ucx_mkey = &mca_spml_ucx_ctx_default.ucp_peers[my_pe].mkeys[segno].key;
    mkeys[0].spml_context = ucx_mkey;

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
        mem_map_params.flags      = flags;

        status = ucp_mem_map(mca_spml_ucx.ucp_context, &mem_map_params, &ucx_mkey->mem_h);
        if (UCS_OK != status) {
            goto error_out;
        }

    } else {
        mca_sshmem_ucx_segment_context_t *ctx = mem_seg->context;
        ucx_mkey->mem_h = ctx->ucp_memh;
    }

    status = ucp_rkey_pack(mca_spml_ucx.ucp_context, ucx_mkey->mem_h,
                           &mkeys[0].u.data, &len);
    if (UCS_OK != status) {
        goto error_unmap;
    }
    if (len >= 0xffff) {
        SPML_UCX_ERROR("packed rkey is too long: %llu >= %d",
                (unsigned long long)len,
                0xffff);
        oshmem_shmem_abort(-1);
    }

    status = ucp_ep_rkey_unpack(mca_spml_ucx_ctx_default.ucp_peers[oshmem_group_self->my_pe].ucp_conn,
                                mkeys[0].u.data,
                                &ucx_mkey->rkey);
    if (UCS_OK != status) {
        SPML_UCX_ERROR("failed to unpack rkey");
        goto error_unmap;
    }

    mkeys[0].len     = len;
    mkeys[0].va_base = addr;
    *count = 1;
    mca_spml_ucx_cache_mkey(&mca_spml_ucx_ctx_default, &mkeys[0], segno, my_pe);
    return mkeys;

error_unmap:
    ucp_mem_unmap(mca_spml_ucx.ucp_context, ucx_mkey->mem_h);
error_out:
    free(mkeys);

    return NULL ;
}

int mca_spml_ucx_deregister(sshmem_mkey_t *mkeys)
{
    spml_ucx_mkey_t   *ucx_mkey;
    map_segment_t *mem_seg;

    MCA_SPML_CALL(quiet(oshmem_ctx_default));
    if (!mkeys)
        return OSHMEM_SUCCESS;

    if (!mkeys[0].spml_context)
        return OSHMEM_SUCCESS;

    mem_seg  = memheap_find_va(mkeys[0].va_base);
    ucx_mkey = (spml_ucx_mkey_t*)mkeys[0].spml_context;

    if (OPAL_UNLIKELY(NULL == mem_seg)) {
        return OSHMEM_ERROR;
    }

    if (MAP_SEGMENT_ALLOC_UCX != mem_seg->type) {
        ucp_mem_unmap(mca_spml_ucx.ucp_context, ucx_mkey->mem_h);
    }
    ucp_rkey_destroy(ucx_mkey->rkey);
    ucx_mkey->rkey = NULL;

    if (0 < mkeys[0].len) {
        ucp_rkey_buffer_release(mkeys[0].u.data);
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

static inline void _ctx_remove(mca_spml_ucx_ctx_array_t *array, mca_spml_ucx_ctx_t *ctx)
{
    int i;

    for (i = 0; i < array->ctxs_count; i++) {
        if (array->ctxs[i] == ctx) {
            array->ctxs[i] = array->ctxs[array->ctxs_count-1];
            array->ctxs[array->ctxs_count-1] = NULL;
            break;
        }
    }

    array->ctxs_count--;
    opal_atomic_wmb ();
}

static int mca_spml_ucx_ctx_create_common(long options, mca_spml_ucx_ctx_t **ucx_ctx_p)
{
    ucp_worker_params_t params;
    ucp_ep_params_t ep_params;
    size_t i, nprocs = oshmem_num_procs();
    int j;
    ucs_status_t err;
    spml_ucx_mkey_t *ucx_mkey;
    sshmem_mkey_t *mkey;
    mca_spml_ucx_ctx_t *ucx_ctx;
    int rc = OSHMEM_ERROR;

    ucx_ctx = malloc(sizeof(mca_spml_ucx_ctx_t));
    ucx_ctx->options = options;

    params.field_mask  = UCP_WORKER_PARAM_FIELD_THREAD_MODE;
    if (oshmem_mpi_thread_provided == SHMEM_THREAD_SINGLE || options & SHMEM_CTX_PRIVATE || options & SHMEM_CTX_SERIALIZED) {
        params.thread_mode = UCS_THREAD_MODE_SINGLE;
    } else {
        params.thread_mode = UCS_THREAD_MODE_MULTI;
    }

    err = ucp_worker_create(mca_spml_ucx.ucp_context, &params,
                            &ucx_ctx->ucp_worker);
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
        ep_params.address    = (ucp_address_t *)(mca_spml_ucx.remote_addrs_tbl[i]);
        err = ucp_ep_create(ucx_ctx->ucp_worker, &ep_params,
                            &ucx_ctx->ucp_peers[i].ucp_conn);
        if (UCS_OK != err) {
            SPML_ERROR("ucp_ep_create(proc=%d/%d) failed: %s", i, nprocs,
                       ucs_status_string(err));
            goto error2;
        }

        for (j = 0; j < memheap_map->n_segments; j++) {
            mkey = &memheap_map->mem_segs[j].mkeys_cache[i][0];
            ucx_mkey = &ucx_ctx->ucp_peers[i].mkeys[j].key;
            if (mkey->u.data) {
                err = ucp_ep_rkey_unpack(ucx_ctx->ucp_peers[i].ucp_conn,
                                         mkey->u.data,
                                         &ucx_mkey->rkey);
                if (UCS_OK != err) {
                    SPML_UCX_ERROR("failed to unpack rkey");
                    goto error2;
                }
                mca_spml_ucx_cache_mkey(ucx_ctx, mkey, j, i);
            }
        }
    }

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
    ucp_worker_destroy(ucx_ctx->ucp_worker);
    free(ucx_ctx);
    rc = OSHMEM_ERR_OUT_OF_RESOURCE;
    SPML_ERROR("ctx create FAILED rc=%d", rc);
    return rc;
}

int mca_spml_ucx_ctx_create(long options, shmem_ctx_t *ctx)
{
    mca_spml_ucx_ctx_t *ucx_ctx;
    int rc;

    /* Take a lock controlling context creation. AUX context may set specific
     * UCX parameters affecting worker creation, which are not needed for
     * regular contexts. */
    pthread_mutex_lock(&mca_spml_ucx.ctx_create_mutex);
    rc = mca_spml_ucx_ctx_create_common(options, &ucx_ctx);
    pthread_mutex_unlock(&mca_spml_ucx.ctx_create_mutex);
    if (rc != OSHMEM_SUCCESS) {
        return rc;
    }

    if (mca_spml_ucx.active_array.ctxs_count == 0) {
        opal_progress_register(spml_ucx_ctx_progress);
    }

    SHMEM_MUTEX_LOCK(mca_spml_ucx.internal_mutex);
    _ctx_add(&mca_spml_ucx.active_array, ucx_ctx);
    SHMEM_MUTEX_UNLOCK(mca_spml_ucx.internal_mutex);

    (*ctx) = (shmem_ctx_t)ucx_ctx;
    return OSHMEM_SUCCESS;
}

void mca_spml_ucx_ctx_destroy(shmem_ctx_t ctx)
{
    MCA_SPML_CALL(quiet(ctx));

    SHMEM_MUTEX_LOCK(mca_spml_ucx.internal_mutex);
    _ctx_remove(&mca_spml_ucx.active_array, (mca_spml_ucx_ctx_t *)ctx);
    _ctx_add(&mca_spml_ucx.idle_array, (mca_spml_ucx_ctx_t *)ctx);
    SHMEM_MUTEX_UNLOCK(mca_spml_ucx.internal_mutex);

    if (!mca_spml_ucx.active_array.ctxs_count) {
        opal_progress_unregister(spml_ucx_ctx_progress);
    }
}

int mca_spml_ucx_get(shmem_ctx_t ctx, void *src_addr, size_t size, void *dst_addr, int src)
{
    void *rva;
    spml_ucx_mkey_t *ucx_mkey;
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;
#if HAVE_DECL_UCP_GET_NB
    ucs_status_ptr_t request;
#else
    ucs_status_t status;
#endif

    ucx_mkey = mca_spml_ucx_get_mkey(ctx, src, src_addr, &rva, &mca_spml_ucx);
#if HAVE_DECL_UCP_GET_NB
    request = ucp_get_nb(ucx_ctx->ucp_peers[src].ucp_conn, dst_addr, size,
                         (uint64_t)rva, ucx_mkey->rkey, opal_common_ucx_empty_complete_cb);
    return opal_common_ucx_wait_request(request, ucx_ctx->ucp_worker, "ucp_get_nb");
#else
    status = ucp_get(ucx_ctx->ucp_peers[src].ucp_conn, dst_addr, size,
                     (uint64_t)rva, ucx_mkey->rkey);
    return ucx_status_to_oshmem(status);
#endif
}

int mca_spml_ucx_get_nb(shmem_ctx_t ctx, void *src_addr, size_t size, void *dst_addr, int src, void **handle)
{
    void *rva;
    ucs_status_t status;
    spml_ucx_mkey_t *ucx_mkey;
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;

    ucx_mkey = mca_spml_ucx_get_mkey(ctx, src, src_addr, &rva, &mca_spml_ucx);
    status = ucp_get_nbi(ucx_ctx->ucp_peers[src].ucp_conn, dst_addr, size,
                     (uint64_t)rva, ucx_mkey->rkey);

    return ucx_status_to_oshmem_nb(status);
}

int mca_spml_ucx_put(shmem_ctx_t ctx, void* dst_addr, size_t size, void* src_addr, int dst)
{
    void *rva;
    spml_ucx_mkey_t *ucx_mkey;
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;
    int res;
#if HAVE_DECL_UCP_PUT_NB
    ucs_status_ptr_t request;
#else
    ucs_status_t status;
#endif

    ucx_mkey = mca_spml_ucx_get_mkey(ctx, dst, dst_addr, &rva, &mca_spml_ucx);
#if HAVE_DECL_UCP_PUT_NB
    request = ucp_put_nb(ucx_ctx->ucp_peers[dst].ucp_conn, src_addr, size,
                         (uint64_t)rva, ucx_mkey->rkey, opal_common_ucx_empty_complete_cb);
    res = opal_common_ucx_wait_request(request, ucx_ctx->ucp_worker, "ucp_put_nb");
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
    void *rva;
    ucs_status_t status;
    spml_ucx_mkey_t *ucx_mkey;
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;

    ucx_mkey = mca_spml_ucx_get_mkey(ctx, dst, dst_addr, &rva, &mca_spml_ucx);
    status = ucp_put_nbi(ucx_ctx->ucp_peers[dst].ucp_conn, src_addr, size,
                     (uint64_t)rva, ucx_mkey->rkey);

    if (OPAL_LIKELY(status >= 0)) {
        mca_spml_ucx_remote_op_posted(ucx_ctx, dst);
    }

    return ucx_status_to_oshmem_nb(status);
}



int mca_spml_ucx_fence(shmem_ctx_t ctx)
{
    ucs_status_t err;
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;

    opal_atomic_wmb();

    err = ucp_worker_fence(ucx_ctx->ucp_worker);
    if (UCS_OK != err) {
         SPML_UCX_ERROR("fence failed: %s", ucs_status_string(err));
         oshmem_shmem_abort(-1);
         return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

int mca_spml_ucx_quiet(shmem_ctx_t ctx)
{
    int flush_get_data;
    int ret;
    unsigned i;
    int idx;
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;

    if (mca_spml_ucx.synchronized_quiet) {
        for (i = 0; i < ucx_ctx->put_proc_count; i++) {
            idx = ucx_ctx->put_proc_indexes[i];
            ret = mca_spml_ucx_get_nb(ctx,
                                      ucx_ctx->ucp_peers[idx].mkeys->super.super.va_base,
                                      sizeof(flush_get_data), &flush_get_data, idx, NULL);
            if (OMPI_SUCCESS != ret) {
                oshmem_shmem_abort(-1);
                return ret;
            }

            opal_bitmap_clear_bit(&ucx_ctx->put_op_bitmap, idx);
        }
        ucx_ctx->put_proc_count = 0;
    }

    opal_atomic_wmb();

    ret = opal_common_ucx_worker_flush(ucx_ctx->ucp_worker);
    if (OMPI_SUCCESS != ret) {
         oshmem_shmem_abort(-1);
         return ret;
    }

    /* If put_all_nb op/s is/are being executed asynchronously, need to wait its
     * completion as well. */
    if (ctx == oshmem_ctx_default) {
        while (mca_spml_ucx.aux_refcnt) {
            opal_progress();
        }
    }

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

    request = ucp_worker_flush_nb(((mca_spml_ucx_ctx_t*)ctx)->ucp_worker, 0,
                                  mca_spml_ucx_put_all_complete_cb);
    if (!UCS_PTR_IS_PTR(request)) {
        mca_spml_ucx_put_all_complete_cb(NULL, UCS_PTR_STATUS(request));
    }

    mca_spml_ucx_aux_unlock();

    return OSHMEM_SUCCESS;
}
