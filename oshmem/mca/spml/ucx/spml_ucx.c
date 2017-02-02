/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
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
#include "oshmem/runtime/runtime.h"
#include "orte/util/show_help.h"

#include "oshmem/mca/spml/ucx/spml_ucx_component.h"

/* Turn ON/OFF debug output from build (default 0) */
#ifndef SPML_UCX_PUT_DEBUG
#define SPML_UCX_PUT_DEBUG    0
#endif

mca_spml_ucx_t mca_spml_ucx = {
    {
        /* Init mca_spml_base_module_t */
        mca_spml_ucx_add_procs,
        mca_spml_ucx_del_procs,
        mca_spml_ucx_enable,
        mca_spml_ucx_register,
        mca_spml_ucx_deregister,
        mca_spml_base_oob_get_mkeys,
        mca_spml_ucx_put,
        mca_spml_ucx_put_nb,
        mca_spml_ucx_get,
        mca_spml_ucx_get_nb,
        mca_spml_ucx_recv,
        mca_spml_ucx_send,
        mca_spml_base_wait,
        mca_spml_base_wait_nb,
        mca_spml_ucx_quiet, /* At the moment fence is the same as quite for 
                               every spml */
        mca_spml_ucx_rmkey_unpack,
        mca_spml_ucx_rmkey_free,
        mca_spml_ucx_memuse_hook,
        (void*)&mca_spml_ucx
    },

    NULL,   /* ucp_context */
    NULL,   /* ucp_worker */
    NULL,   /* ucp_peers */
    0,      /* using_mem_hooks */
    1,      /* num_disconnect */
    0       /* heap_reg_nb */
};

int mca_spml_ucx_enable(bool enable)
{
    SPML_VERBOSE(50, "*** ucx ENABLED ****");
    if (false == enable) {
        return OSHMEM_SUCCESS;
    }

    mca_spml_ucx.enabled = true;

    return OSHMEM_SUCCESS;
}


static void mca_spml_ucx_waitall(void **reqs, size_t *count_p)
{
    ucs_status_t status;
    size_t i;

    SPML_VERBOSE(10, "waiting for %d disconnect requests", *count_p);
    for (i = 0; i < *count_p; ++i) {
        do {
            opal_progress();
            status = ucp_request_test(reqs[i], NULL);
        } while (status == UCS_INPROGRESS);
        if (status != UCS_OK) {
            SPML_ERROR("disconnect request failed: %s",
                       ucs_status_string(status));
        }
        ucp_request_release(reqs[i]);
        reqs[i] = NULL;
    }

    *count_p = 0;
}

int mca_spml_ucx_del_procs(ompi_proc_t** procs, size_t nprocs)
{
    int my_rank = oshmem_my_proc_id();
    size_t num_reqs, max_reqs;
    void *dreq, **dreqs;
    ucp_ep_h ep;
    size_t i, n;

    oshmem_shmem_barrier();

    if (!mca_spml_ucx.ucp_peers) {
        return OSHMEM_SUCCESS;
    }

    max_reqs = mca_spml_ucx.num_disconnect;
    if (max_reqs > nprocs) {
        max_reqs = nprocs;
    }

    dreqs = malloc(sizeof(*dreqs) * max_reqs);
    if (dreqs == NULL) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    num_reqs = 0;

    for (i = 0; i < nprocs; ++i) {
        n  = (i + my_rank) % nprocs;
        ep = mca_spml_ucx.ucp_peers[n].ucp_conn;
        if (ep == NULL) {
            continue;
        }

        SPML_VERBOSE(10, "disconnecting from peer %d", n);
        dreq = ucp_disconnect_nb(ep);
        if (dreq != NULL) {
            if (UCS_PTR_IS_ERR(dreq)) {
                SPML_ERROR("ucp_disconnect_nb(%d) failed: %s", n,
                           ucs_status_string(UCS_PTR_STATUS(dreq)));
            } else {
                dreqs[num_reqs++] = dreq;
            }
        }

        mca_spml_ucx.ucp_peers[n].ucp_conn = NULL;

        if ((int)num_reqs >= mca_spml_ucx.num_disconnect) {
            mca_spml_ucx_waitall(dreqs, &num_reqs);
        }
    }

    mca_spml_ucx_waitall(dreqs, &num_reqs);
    free(dreqs);

    opal_pmix.fence(NULL, 0);
    free(mca_spml_ucx.ucp_peers);
    return OSHMEM_SUCCESS;
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

int mca_spml_ucx_add_procs(ompi_proc_t** procs, size_t nprocs)
{
    size_t i, n;
    int rc = OSHMEM_ERROR;
    int my_rank = oshmem_my_proc_id();
    ucs_status_t err;
    ucp_address_t *wk_local_addr;
    size_t wk_addr_len;
    int *wk_roffs, *wk_rsizes;
    char *wk_raddrs;
    ucp_ep_params_t ep_params;


    mca_spml_ucx.ucp_peers = (ucp_peer_t *) calloc(nprocs, sizeof(*(mca_spml_ucx.ucp_peers)));
    if (NULL == mca_spml_ucx.ucp_peers) {
        goto error;
    }

    err = ucp_worker_get_address(mca_spml_ucx.ucp_worker, &wk_local_addr, &wk_addr_len);
    if (err != UCS_OK) {
        goto error;
    }
    dump_address(my_rank, (char *)wk_local_addr, wk_addr_len);

    rc = oshmem_shmem_xchng(wk_local_addr, wk_addr_len, nprocs,
            (void **)&wk_raddrs, &wk_roffs, &wk_rsizes);
    if (rc != OSHMEM_SUCCESS) {
        goto error;
    }

    opal_progress_register(spml_ucx_progress);

    /* Get the EP connection requests for all the processes from modex */
    for (n = 0; n < nprocs; ++n) {
        i = (my_rank + n) % nprocs;
        dump_address(i, (char *)(wk_raddrs + wk_roffs[i]), wk_rsizes[i]);

        ep_params.field_mask = UCP_EP_PARAM_FIELD_REMOTE_ADDRESS;
        ep_params.address    = (ucp_address_t *)(wk_raddrs + wk_roffs[i]);

        err = ucp_ep_create(mca_spml_ucx.ucp_worker, 
                            &ep_params,
                            &mca_spml_ucx.ucp_peers[i].ucp_conn);
        if (UCS_OK != err) {
            SPML_ERROR("ucp_ep_create failed: %s", ucs_status_string(err));
            goto error2;
        }
        OSHMEM_PROC_DATA(procs[i])->num_transports = 1;
        OSHMEM_PROC_DATA(procs[i])->transport_ids = spml_ucx_transport_ids;
    }

    ucp_worker_release_address(mca_spml_ucx.ucp_worker, wk_local_addr);
    free(wk_raddrs);
    free(wk_rsizes);
    free(wk_roffs);

    SPML_VERBOSE(50, "*** ADDED PROCS ***");
    return OSHMEM_SUCCESS;

error2:
    for (i = 0; i < nprocs; ++i) {
         if (mca_spml_ucx.ucp_peers[i].ucp_conn) {
             ucp_ep_destroy(mca_spml_ucx.ucp_peers[i].ucp_conn);
         }
    }
    if (mca_spml_ucx.ucp_peers) 
        free(mca_spml_ucx.ucp_peers);
    if (wk_raddrs)
        free(wk_raddrs);
    if (wk_rsizes)
        free(wk_rsizes);
    if (wk_roffs)
        free(wk_roffs);
    if (mca_spml_ucx.ucp_peers)
        free(mca_spml_ucx.ucp_peers);
error:
    rc = OSHMEM_ERR_OUT_OF_RESOURCE;
    SPML_ERROR("add procs FAILED rc=%d", rc);
    return rc;

}


spml_ucx_mkey_t * mca_spml_ucx_get_mkey_slow(int pe, void *va, void **rva)
{
    sshmem_mkey_t *r_mkey;

    r_mkey = mca_memheap_base_get_cached_mkey(pe, va, 0, rva);
    if (OPAL_UNLIKELY(!r_mkey)) {
        SPML_ERROR("pe=%d: %p is not address of symmetric variable",
                   pe, va);
        oshmem_shmem_abort(-1);
        return NULL;
    }
    return (spml_ucx_mkey_t *)(r_mkey->spml_context);
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

static void mca_spml_ucx_cache_mkey(sshmem_mkey_t *mkey, uint32_t segno, int dst_pe)
{
    ucp_peer_t *peer;

    peer = &mca_spml_ucx.ucp_peers[dst_pe];
    mkey_segment_init(&peer->mkeys[segno].super, mkey, segno);
}

void mca_spml_ucx_rmkey_unpack(sshmem_mkey_t *mkey, uint32_t segno, int pe, int tr_id)
{
    spml_ucx_mkey_t   *ucx_mkey;
    ucs_status_t err;
    
    ucx_mkey = &mca_spml_ucx.ucp_peers[pe].mkeys[segno].key;

    err = ucp_ep_rkey_unpack(mca_spml_ucx.ucp_peers[pe].ucp_conn,
            mkey->u.data, 
            &ucx_mkey->rkey); 
    if (UCS_OK != err) {
        SPML_ERROR("failed to unpack rkey: %s", ucs_status_string(err));
        goto error_fatal;
    }

    mkey->spml_context = ucx_mkey;
    mca_spml_ucx_cache_mkey(mkey, segno, pe);
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
    ucx_mkey = &mca_spml_ucx.ucp_peers[my_pe].mkeys[HEAP_SEG_INDEX].key;

    params.field_mask = UCP_MEM_ADVISE_PARAM_FIELD_ADDRESS |
                        UCP_MEM_ADVISE_PARAM_FIELD_LENGTH |
                        UCP_MEM_ADVISE_PARAM_FIELD_ADVICE;

    params.address = addr;
    params.length  = length;
    params.advice  = UCP_MADV_WILLNEED;

    status = ucp_mem_advise(mca_spml_ucx.ucp_context, ucx_mkey->mem_h, &params);
    if (UCS_OK != status) {
        SPML_ERROR("ucp_mem_advise failed addr %p len %llu : %s",
                   addr, (unsigned long long)length, ucs_status_string(status));
    }
}

sshmem_mkey_t *mca_spml_ucx_register(void* addr,
                                         size_t size,
                                         uint64_t shmid,
                                         int *count)
{
    sshmem_mkey_t *mkeys;
    ucs_status_t err;
    spml_ucx_mkey_t   *ucx_mkey;
    size_t len;
    int my_pe = oshmem_my_proc_id();
    ucp_mem_map_params_t mem_map_params;
    int seg;
    unsigned flags;

    *count = 0;
    mkeys = (sshmem_mkey_t *) calloc(1, sizeof(*mkeys));
    if (!mkeys) {
        return NULL;
    }

    seg = memheap_find_segnum(addr);

    ucx_mkey = &mca_spml_ucx.ucp_peers[my_pe].mkeys[seg].key;
    mkeys[0].spml_context = ucx_mkey;

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

    err = ucp_mem_map(mca_spml_ucx.ucp_context, &mem_map_params, &ucx_mkey->mem_h);
    if (UCS_OK != err) {
        goto error_out;
    }

    err = ucp_rkey_pack(mca_spml_ucx.ucp_context, ucx_mkey->mem_h, 
            &mkeys[0].u.data, &len); 
    if (UCS_OK != err) {
        goto error_unmap;
    }
    if (len >= 0xffff) {
        SPML_ERROR("packed rkey is too long: %llu >= %d",
                (unsigned long long)len,
                0xffff);
        oshmem_shmem_abort(-1);
    }

    err = ucp_ep_rkey_unpack(mca_spml_ucx.ucp_peers[oshmem_group_self->my_pe].ucp_conn,
                             mkeys[0].u.data,
                             &ucx_mkey->rkey);
    if (UCS_OK != err) {
        SPML_ERROR("failed to unpack rkey");
        goto error_unmap;
    }

    mkeys[0].len     = len;
    mkeys[0].va_base = mem_map_params.address;
    *count = 1;
    mca_spml_ucx_cache_mkey(&mkeys[0], seg, my_pe);
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

    MCA_SPML_CALL(fence());
    if (!mkeys)
        return OSHMEM_SUCCESS;

    if (!mkeys[0].spml_context) 
        return OSHMEM_SUCCESS;

    ucx_mkey = (spml_ucx_mkey_t *)mkeys[0].spml_context;
    ucp_mem_unmap(mca_spml_ucx.ucp_context, ucx_mkey->mem_h);

    if (0 < mkeys[0].len) {
        ucp_rkey_buffer_release(mkeys[0].u.data);
    }

    return OSHMEM_SUCCESS;
}

int mca_spml_ucx_get(void *src_addr, size_t size, void *dst_addr, int src)
{
    void *rva;
    ucs_status_t status;
    spml_ucx_mkey_t *ucx_mkey;

    ucx_mkey = mca_spml_ucx_get_mkey(src, src_addr, &rva);
    status = ucp_get(mca_spml_ucx.ucp_peers[src].ucp_conn, dst_addr, size,
                     (uint64_t)rva, ucx_mkey->rkey);

    return ucx_status_to_oshmem(status);
}

int mca_spml_ucx_get_nb(void *src_addr, size_t size, void *dst_addr, int src, void **handle)
{
    void *rva;
    ucs_status_t status;
    spml_ucx_mkey_t *ucx_mkey;

    ucx_mkey = mca_spml_ucx_get_mkey(src, src_addr, &rva);
    status = ucp_get_nbi(mca_spml_ucx.ucp_peers[src].ucp_conn, dst_addr, size,
                     (uint64_t)rva, ucx_mkey->rkey);

    return ucx_status_to_oshmem_nb(status);
}

int mca_spml_ucx_put(void* dst_addr, size_t size, void* src_addr, int dst)
{
    void *rva;
    ucs_status_t status;
    spml_ucx_mkey_t *ucx_mkey;

    ucx_mkey = mca_spml_ucx_get_mkey(dst, dst_addr, &rva);
    status = ucp_put(mca_spml_ucx.ucp_peers[dst].ucp_conn, src_addr, size,
                     (uint64_t)rva, ucx_mkey->rkey);

    return ucx_status_to_oshmem(status);
}

int mca_spml_ucx_put_nb(void* dst_addr, size_t size, void* src_addr, int dst, void **handle)
{
    void *rva;
    ucs_status_t status;
    spml_ucx_mkey_t *ucx_mkey;

    ucx_mkey = mca_spml_ucx_get_mkey(dst, dst_addr, &rva);
    status = ucp_put_nbi(mca_spml_ucx.ucp_peers[dst].ucp_conn, src_addr, size,
                     (uint64_t)rva, ucx_mkey->rkey);

    return ucx_status_to_oshmem_nb(status);
}

int mca_spml_ucx_fence(void)
{
    ucs_status_t err;

    err = ucp_worker_flush(mca_spml_ucx.ucp_worker);
    if (UCS_OK != err) {
         SPML_ERROR("fence failed: %s", ucs_status_string(err));
         oshmem_shmem_abort(-1);
         return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

int mca_spml_ucx_quiet(void)
{
    ucs_status_t err;

    err = ucp_worker_flush(mca_spml_ucx.ucp_worker);
    if (UCS_OK != err) {
         SPML_ERROR("fence failed: %s", ucs_status_string(err));
         oshmem_shmem_abort(-1);
         return OSHMEM_ERROR;
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
