/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
        NULL, /* todo: mca_spml_ucx_put_nb, */
        mca_spml_ucx_get,
        mca_spml_ucx_recv,
        mca_spml_ucx_send,
        mca_spml_base_wait,
        mca_spml_base_wait_nb,
        mca_spml_ucx_quiet, /* At the moment fence is the same as quite for 
                               every spml */
        mca_spml_ucx_rmkey_unpack,
        mca_spml_ucx_rmkey_free,
        (void*)&mca_spml_ucx
    }
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

int mca_spml_ucx_del_procs(oshmem_proc_t** procs, size_t nprocs)
{
    size_t i, n;
    int my_rank = oshmem_my_proc_id();

    oshmem_shmem_barrier();

    if (!mca_spml_ucx.ucp_peers) {
        return OSHMEM_SUCCESS;
    }

     for (n = 0; n < nprocs; n++) {
         i = (my_rank + n) % nprocs;
         if (mca_spml_ucx.ucp_peers[i].ucp_conn) {
             ucp_ep_destroy(mca_spml_ucx.ucp_peers[i].ucp_conn);
         }
     }

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

int mca_spml_ucx_add_procs(oshmem_proc_t** procs, size_t nprocs)
{
    size_t i, n;
    int rc = OSHMEM_ERROR;
    int my_rank = oshmem_my_proc_id();
    ucs_status_t err;
    ucp_address_t *wk_local_addr;
    size_t wk_addr_len;
    int *wk_roffs, *wk_rsizes;
    char *wk_raddrs;


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
        err = ucp_ep_create(mca_spml_ucx.ucp_worker, 
                (ucp_address_t *)(wk_raddrs + wk_roffs[i]),
                &mca_spml_ucx.ucp_peers[i].ucp_conn);
        if (UCS_OK != err) {
            SPML_ERROR("ucp_ep_create failed!!!\n");
            goto error2;
        }
        procs[i]->num_transports = 1;
        procs[i]->transport_ids = spml_ucx_transport_ids;
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

void mca_spml_ucx_rmkey_free(sshmem_mkey_t *mkey)
{
    spml_ucx_mkey_t   *ucx_mkey;

    if (!mkey->spml_context) {
        return;
    }
    ucx_mkey = (spml_ucx_mkey_t *)(mkey->spml_context);
    ucp_rkey_destroy(ucx_mkey->rkey);
    free(ucx_mkey);
}

void mca_spml_ucx_rmkey_unpack(sshmem_mkey_t *mkey, int pe)
{
    spml_ucx_mkey_t   *ucx_mkey;
    ucs_status_t err;

    ucx_mkey = (spml_ucx_mkey_t *)malloc(sizeof(*ucx_mkey));
    if (!ucx_mkey) {
        SPML_ERROR("not enough memory to allocate mkey");
        goto error_fatal;
    }
    
    err = ucp_ep_rkey_unpack(mca_spml_ucx.ucp_peers[pe].ucp_conn,
            mkey->u.data, 
            &ucx_mkey->rkey); 
    if (UCS_OK != err) {
        SPML_ERROR("failed to unpack rkey");
        goto error_fatal;
    }

    mkey->spml_context = ucx_mkey;
    return;

error_fatal:
    oshmem_shmem_abort(-1);
    return;
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

    *count = 0;
    mkeys = (sshmem_mkey_t *) calloc(1, sizeof(*mkeys));
    if (!mkeys) {
        return NULL ;
    }

    ucx_mkey = (spml_ucx_mkey_t *)malloc(sizeof(*ucx_mkey));
    if (!ucx_mkey) {
        goto error_out;
    }

    mkeys[0].spml_context = ucx_mkey;
    err = ucp_mem_map(mca_spml_ucx.ucp_context, 
            &addr, size, 0, &ucx_mkey->mem_h);
    if (UCS_OK != err) {
        goto error_out1;
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
    mkeys[0].va_base = addr;
    *count = 1;
    return mkeys;

error_unmap:
    ucp_mem_unmap(mca_spml_ucx.ucp_context, ucx_mkey->mem_h);
error_out1:
    free(ucx_mkey);
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

    free(ucx_mkey);
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

int mca_spml_ucx_fence(void)
{
    ucs_status_t err;

    err = ucp_worker_flush(mca_spml_ucx.ucp_worker);
    if (UCS_OK != err) {
        SPML_ERROR("fence failed");
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
        SPML_ERROR("fence failed");
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
