/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
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
#include "oshmem/mca/spml/ikrit/spml_ikrit.h"
#include "oshmem/include/shmem.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/proc/proc.h"
#include "oshmem/mca/spml/base/base.h"
#include "oshmem/mca/spml/base/spml_base_putreq.h"
#include "oshmem/runtime/runtime.h"
#include "orte/util/show_help.h"
#include "ompi/runtime/ompi_module_exchange.h"

#include "oshmem/mca/spml/ikrit/spml_ikrit_component.h"
#include "ompi/communicator/communicator.h" /*TODO:  ompi_communicator_t */
#include "ompi/patterns/comm/coll_ops.h" /*TODO:  comm_bcast_pml */

/* use zcopy for put/get via sysv shared memory */
#define SPML_IKRIT_USE_SHM_ZCOPY
//#define SPML_IKRIT_DEBUG_PUT

typedef struct spml_ikrit_am_hdr {
    uint64_t va;
} spml_ikrit_am_hdr_t;

struct mca_spml_ikrit_put_request { 
    mca_spml_base_put_request_t req_put;
    mxm_send_req_t mxm_req;
    int pe;
    mxm_req_buffer_t iov[2];
    spml_ikrit_am_hdr_t am_pkt;
};

typedef struct mca_spml_ikrit_put_request mca_spml_ikrit_put_request_t;
OBJ_CLASS_DECLARATION(mca_spml_ikrit_put_request_t);

static int spml_ikrit_get_ep_address(spml_ikrit_mxm_ep_conn_info_t *ep_info, mxm_ptl_id_t ptlid)
{
    size_t addrlen;
    mxm_error_t err;

    addrlen = sizeof(ep_info->ptl_addr[ptlid]);
    err = mxm_ep_address(mca_spml_ikrit.mxm_ep, ptlid,
            (struct sockaddr *) &ep_info->ptl_addr[ptlid], &addrlen);
    if (MXM_OK != err) {
        orte_show_help("help-spml-ikrit.txt", "unable to extract endpoint address",
                true, mxm_error_string(err));
        return OSHMEM_ERROR;
    }

    return OSHMEM_SUCCESS;
}

static inline void mca_spml_irkit_req_wait(mxm_req_base_t *req)
{
    while (!mxm_req_test(req)) 
        opal_progress();
}

static int mca_spml_ikrit_put_request_free(struct oshmem_request_t** request)
{
    mca_spml_ikrit_put_request_t *put_req = *(mca_spml_ikrit_put_request_t **)request;

    assert(false == put_req->req_put.req_base.req_free_called);
    OPAL_THREAD_LOCK(&oshmem_request_lock);
    put_req->req_put.req_base.req_free_called = true;
    OMPI_FREE_LIST_RETURN( &mca_spml_base_put_requests,                 
                (ompi_free_list_item_t*)put_req);
    OPAL_THREAD_UNLOCK(&oshmem_request_lock);

    *request = SHMEM_REQUEST_NULL; /*MPI_REQUEST_NULL;*/

    return OSHMEM_SUCCESS;
}

static int mca_spml_ikrit_put_request_cancel(struct oshmem_request_t * request, int complete)
{
    return OSHMEM_SUCCESS;
}

static void mca_spml_ikrit_put_request_construct(mca_spml_ikrit_put_request_t* req)
{
    req->req_put.req_base.req_type = MCA_SPML_REQUEST_PUT;
    req->req_put.req_base.req_oshmem.req_free = mca_spml_ikrit_put_request_free;
    req->req_put.req_base.req_oshmem.req_cancel = mca_spml_ikrit_put_request_cancel;
}

static void mca_spml_ikrit_put_request_destruct(mca_spml_ikrit_put_request_t* req)
{
}

OBJ_CLASS_INSTANCE( mca_spml_ikrit_put_request_t,
        mca_spml_base_put_request_t,
        mca_spml_ikrit_put_request_construct,
        mca_spml_ikrit_put_request_destruct );


struct mca_spml_ikrit_get_request { 
    mca_spml_base_get_request_t req_get;
    mxm_send_req_t mxm_req;
};

typedef struct mca_spml_ikrit_get_request mca_spml_ikrit_get_request_t;
OBJ_CLASS_DECLARATION(mca_spml_ikrit_get_request_t);

static int mca_spml_ikrit_get_request_free(struct oshmem_request_t** request)
{
    mca_spml_ikrit_get_request_t *get_req = *(mca_spml_ikrit_get_request_t **)request;

    assert(false == get_req->req_get.req_base.req_free_called);
    OPAL_THREAD_LOCK(&oshmem_request_lock);
    get_req->req_get.req_base.req_free_called = true;
    OMPI_FREE_LIST_RETURN( &mca_spml_base_get_requests,                 
                (ompi_free_list_item_t*)get_req);
    OPAL_THREAD_UNLOCK(&oshmem_request_lock);

    *request = SHMEM_REQUEST_NULL; /*MPI_REQUEST_NULL;*/

    return OSHMEM_SUCCESS;
}

static int mca_spml_ikrit_get_request_cancel(struct oshmem_request_t * request, int complete)
{
    return OSHMEM_SUCCESS;
}

static void mca_spml_ikrit_get_request_construct(mca_spml_ikrit_get_request_t* req)
{
    req->req_get.req_base.req_type = MCA_SPML_REQUEST_PUT;
    req->req_get.req_base.req_oshmem.req_free = mca_spml_ikrit_get_request_free;
    req->req_get.req_base.req_oshmem.req_cancel = mca_spml_ikrit_get_request_cancel;
}

static void mca_spml_ikrit_get_request_destruct(mca_spml_ikrit_get_request_t* req)
{
}

OBJ_CLASS_INSTANCE( mca_spml_ikrit_get_request_t,
        mca_spml_base_get_request_t,
        mca_spml_ikrit_get_request_construct,
        mca_spml_ikrit_get_request_destruct );

int mca_spml_ikrit_put_simple(void* dst_addr, size_t size, void* src_addr, int dst);

static void mxm_setup_relays(oshmem_proc_t **procs, size_t nprocs);

mca_spml_ikrit_t mca_spml_ikrit = {
    {
        /* Init mca_spml_base_module_t */
        mca_spml_ikrit_add_procs,
        mca_spml_ikrit_del_procs,
        mca_spml_ikrit_enable,
        mca_spml_ikrit_register,
        mca_spml_ikrit_deregister,
        mca_spml_ikrit_oob_get_mkeys,
        mca_spml_ikrit_put,
        mca_spml_ikrit_put_nb,
        mca_spml_ikrit_get,
        mca_spml_ikrit_recv ,
        mca_spml_ikrit_send, 
        mca_spml_base_wait,
        mca_spml_base_wait_nb,
        mca_spml_ikrit_fence
    }
};

void mca_spml_ikrit_dump_stats(void);
void mca_spml_ikrit_dump_stats() 
{
    int num_procs;
    int i;
    char sbuf[1024];
    FILE *fp;

    fp = fmemopen(sbuf, sizeof(sbuf), "rw");
    num_procs = oshmem_num_procs();
    for (i = 0; i < num_procs; i++) {
        mxm_print_conn_state(mca_spml_ikrit.mxm_peers[i]->mxm_conn, MXM_STATE_DETAIL_LEVEL_DATA, "", fp);
        printf("=========== pe:%d conn:%p stats:\n %s==================\n", i, mca_spml_ikrit.mxm_peers[i]->mxm_conn, sbuf);
        rewind(fp);
    }
    fclose(fp);
}

static inline mca_spml_ikrit_put_request_t *alloc_put_req(void)
{
    mca_spml_ikrit_put_request_t *req;
    ompi_free_list_item_t* item;
    int rc;

    rc = OSHMEM_SUCCESS;
    OMPI_FREE_LIST_WAIT(&mca_spml_base_put_requests, item, rc);
    if (OMPI_SUCCESS != rc)
        return NULL;

    req = (mca_spml_ikrit_put_request_t *)item;
    req->req_put.req_base.req_free_called = false;
    req->req_put.req_base.req_oshmem.req_complete = false;

    return req;
}

static inline mca_spml_ikrit_get_request_t *alloc_get_req(void)
{
    mca_spml_ikrit_get_request_t *req;
    ompi_free_list_item_t* item;
    int rc;

    rc = OSHMEM_SUCCESS;
    OMPI_FREE_LIST_WAIT(&mca_spml_base_get_requests, item, rc);
    if (OMPI_SUCCESS != rc)
        return NULL;

    req = (mca_spml_ikrit_get_request_t *)item;
    req->req_get.req_base.req_free_called = false;
    req->req_get.req_base.req_oshmem.req_complete = false;

    return req;
}

int mca_spml_ikrit_enable(bool enable)
{
    SPML_VERBOSE(50, "*** ikrit ENABLED ****");
    ompi_free_list_init_new( &mca_spml_base_put_requests,
            sizeof(mca_spml_ikrit_put_request_t),
            opal_cache_line_size,
            OBJ_CLASS(mca_spml_ikrit_put_request_t),
            0,opal_cache_line_size,
            mca_spml_ikrit.free_list_num,
            mca_spml_ikrit.free_list_max,
            mca_spml_ikrit.free_list_inc,
            NULL );

    ompi_free_list_init_new( &mca_spml_base_get_requests,
            sizeof(mca_spml_ikrit_get_request_t),
            opal_cache_line_size,
            OBJ_CLASS(mca_spml_ikrit_get_request_t),
            0,opal_cache_line_size,
            mca_spml_ikrit.free_list_num,
            mca_spml_ikrit.free_list_max,
            mca_spml_ikrit.free_list_inc,
            NULL );

    return OSHMEM_SUCCESS;
}

static int create_ptl_idx(int dst_pe)
{
    oshmem_proc_t *proc;

    proc = oshmem_proc_group_find(oshmem_group_all, dst_pe);

    proc->transport_ids = (char *)malloc(MXM_PTL_LAST * sizeof(char));
    if (!proc->transport_ids)
        return OSHMEM_ERROR;

    proc->num_transports = 1;
    if (oshmem_my_proc_id() == dst_pe) 
        proc->transport_ids[0] = MXM_PTL_SELF;
    else
        proc->transport_ids[0] = MXM_PTL_RDMA;
    return OSHMEM_SUCCESS;
}

static void destroy_ptl_idx(int dst_pe)
{
    oshmem_proc_t *proc;

    proc = oshmem_proc_group_find(oshmem_group_all, dst_pe);
    if (proc->transport_ids)
        free(proc->transport_ids);
}

static void mxm_peer_construct(mxm_peer_t *p)
{
    p->pe = -1;
    p->n_active_puts = 0;
    p->need_fence = 0;
    p->pe_relay = -1;
    p->n_slaves = 0;
}

static void mxm_peer_destruct(mxm_peer_t *p)
{
    /* may be we need to remov item from list */
}

OBJ_CLASS_INSTANCE(
        mxm_peer_t,
        opal_list_item_t,
        mxm_peer_construct,
        mxm_peer_destruct
        );


int mca_spml_ikrit_del_procs(oshmem_proc_t** procs, size_t nprocs)
{
    size_t i;
    opal_list_item_t *item;

    if (mca_spml_ikrit.mxm_ep) {
        mxm_ep_destroy(mca_spml_ikrit.mxm_ep);
        mca_spml_ikrit.mxm_ep = 0;
    }

    while (NULL != (item = opal_list_remove_first(&mca_spml_ikrit.active_peers))) {};
    OBJ_DESTRUCT(&mca_spml_ikrit.active_peers);

    for (i = 0; i < nprocs; i++) {
        destroy_ptl_idx(i);
        if (mca_spml_ikrit.mxm_peers[i]) {
            //mxm_ep_disconnect(mca_spml_ikrit.mxm_peers[i]->mxm_conn);
            OBJ_RELEASE(mca_spml_ikrit.mxm_peers[i]);
        }
    }
    if (mca_spml_ikrit.mxm_peers)
        free(mca_spml_ikrit.mxm_peers);

    return OSHMEM_SUCCESS;
}

int mca_spml_ikrit_add_procs(oshmem_proc_t** procs, size_t nprocs)
{
    spml_ikrit_mxm_ep_conn_info_t* ep_info;
    mxm_conn_req_t *conn_reqs;
    mxm_error_t err;
    size_t i;
    int rc = OSHMEM_ERROR;
    oshmem_proc_t *proc_self;
    int my_rank = oshmem_my_proc_id();
    int timeout;

    OBJ_CONSTRUCT(&mca_spml_ikrit.active_peers, opal_list_t);
    /* Allocate connection requests */
    conn_reqs = malloc(nprocs * sizeof(mxm_conn_req_t));
    ep_info = malloc(nprocs * sizeof(spml_ikrit_mxm_ep_conn_info_t));
    if (NULL == conn_reqs || NULL == ep_info) {
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto bail;
    }
    memset(conn_reqs, 0x0, sizeof(mxm_conn_req_t));
    memset(ep_info, 0x0, sizeof(spml_ikrit_mxm_ep_conn_info_t));

    mca_spml_ikrit.mxm_peers = (mxm_peer_t **)malloc(nprocs*sizeof(*(mca_spml_ikrit.mxm_peers)));
    if (NULL == mca_spml_ikrit.mxm_peers) {
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto bail;
    }

    int* ranks_in_comm;
    ranks_in_comm = (int *) malloc(nprocs * sizeof(int));
    for (i = 0; i < nprocs; ++i) {
        ranks_in_comm[i] = i;
    }
    
    if (OSHMEM_SUCCESS != spml_ikrit_get_ep_address(ep_info + my_rank, MXM_PTL_SELF)) {
        return OSHMEM_ERROR;
    }
    if (OSHMEM_SUCCESS != spml_ikrit_get_ep_address(ep_info + my_rank, MXM_PTL_RDMA)) {
        return OSHMEM_ERROR;
    }

    opal_progress_register(spml_ikrit_progress);
    for (i = 0; i < nprocs; ++i)
    {
        comm_bcast_pml(ep_info + i, i, sizeof(spml_ikrit_mxm_ep_conn_info_t),
                MPI_BYTE, my_rank, nprocs,
                ranks_in_comm, (ompi_communicator_t *)&ompi_mpi_comm_world);
    }

    if (ranks_in_comm)
        free(ranks_in_comm);

    /* Get the EP connection requests for all the processes from modex */
    for (i = 0; i < nprocs; ++i) {

        mca_spml_ikrit.mxm_peers[i] = OBJ_NEW(mxm_peer_t);
        if (NULL == mca_spml_ikrit.mxm_peers[i]) {
            rc = OSHMEM_ERR_OUT_OF_RESOURCE;
            goto bail;
        }
        mca_spml_ikrit.mxm_peers[i]->pe = i;

        conn_reqs[i].ptl_addr[MXM_PTL_SELF] = (struct sockaddr *)&ep_info[i].ptl_addr[MXM_PTL_SELF];
        conn_reqs[i].ptl_addr[MXM_PTL_SHM] = NULL;
        conn_reqs[i].ptl_addr[MXM_PTL_RDMA] = (struct sockaddr *)&ep_info[i].ptl_addr[MXM_PTL_RDMA];
    }

    /* Connect to remote peers */
    if (mxm_get_version() < MXM_VERSION(1,5)) {
            timeout = 1000;
        } else {
            timeout = -1;
    }
    err = mxm_ep_connect(mca_spml_ikrit.mxm_ep, conn_reqs, nprocs, timeout);
    if (MXM_OK != err) {
        SPML_ERROR("MXM returned connect error: %s\n", mxm_error_string(err));
        for (i = 0; i < nprocs; ++i) {
            if (MXM_OK != conn_reqs[i].error) {
                SPML_ERROR("MXM EP connect to %s error: %s\n", procs[i]->proc_hostname,
                        mxm_error_string(conn_reqs[i].error));
            }
        }
        rc = OSHMEM_ERR_CONNECTION_FAILED;
        goto bail;
    }

    /* Save returned connections */
    for (i = 0; i < nprocs; ++i) {
        mca_spml_ikrit.mxm_peers[i]->mxm_conn = conn_reqs[i].conn;
        if (OSHMEM_SUCCESS != create_ptl_idx(i))
                goto bail;

        //printf("proc=%d name=%s jobid = %u vpid = %u flags = %x\n", (int)i, procs[i]->proc_hostname, procs[i]->proc_name.jobid, procs[i]->proc_name.vpid, procs[i]->proc_flags);
        mxm_conn_ctx_set(conn_reqs[i].conn, mca_spml_ikrit.mxm_peers[i]);
    }

    if (ep_info)
        free(ep_info);
    if (conn_reqs)
        free(conn_reqs);

    proc_self = oshmem_proc_group_find(oshmem_group_all, my_rank);
    /* identify local processes and change transport to SHM */
    for (i = 0; i < nprocs; i++) {
        if (procs[i]->proc_name.jobid != proc_self->proc_name.jobid ||
                !OPAL_PROC_ON_LOCAL_NODE(procs[i]->proc_flags)) {
            continue;
        }
        if (procs[i] == proc_self)
            continue;
#ifdef SPML_IKRIT_USE_SHM_ZCOPY
        procs[i]->transport_ids[0] = MXM_PTL_SHM;
        procs[i]->transport_ids[1] = MXM_PTL_RDMA;
        procs[i]->num_transports = 2;
#endif
    }

    mxm_setup_relays(procs, nprocs);

    SPML_VERBOSE(50, "*** ADDED PROCS ***");
    return OSHMEM_SUCCESS;

bail:
    if (ep_info)
        free(ep_info);
    if (conn_reqs)
        free(conn_reqs);
    SPML_ERROR("add procs FAILED rc=%d", rc);
    return rc;

}

mca_spml_mkey_t *mca_spml_ikrit_register(void* addr, size_t size, uint64_t shmid, int *count)
{
    //mxm_error_t err;
    int i;
    mca_spml_mkey_t *mkeys;

    *count = 0;
    mkeys = (mca_spml_mkey_t *)calloc(1, MXM_PTL_LAST * sizeof(*mkeys));
    if(!mkeys){
        return NULL;
    }
    
    for (i = 0; i < MXM_PTL_LAST; i++) {
        switch(i) {
            case MXM_PTL_SHM:
                if ((int)MEMHEAP_SHM_GET_ID(shmid) != MEMHEAP_SHM_INVALID) {
                    mkeys[i].key = shmid;
                    mkeys[i].va_base = 0; 
                }
                else {
                    mkeys[i].key = 0;
                    mkeys[i].va_base = (unsigned long)addr;
                }
                mkeys[i].spml_context = 0;
                break;
            case MXM_PTL_SELF:
                mkeys[i].key = 0;
                mkeys[i].spml_context = 0;
                mkeys[i].va_base = (unsigned long)addr;
                break;
            case MXM_PTL_RDMA:
#if MXM_API < MXM_VERSION(1,5)
                mkeys[i].ib.lkey = mkeys[i].ib.rkey = MXM_MKEY_NONE;
#else
                mkeys[i].ib.lkey = mkeys[i].ib.rkey = 0;
#endif
                mkeys[i].spml_context = 0;
#if 0
                /* don't register memheap if zcopy support is not enabled */
                err = mxm_reg_mr(mca_spml_ikrit.mxm_ep, MXM_PTL_RDMA, addr, size, &mkeys[i].ib.lkey, &mkeys[i].ib.rkey);
                if (MXM_OK != err) {
                    SPML_VERBOSE(1, "failed to register memory: %s", mxm_error_string(err));
                    goto err;
                }
#endif
                mkeys[i].va_base = (unsigned long)addr;
                //mkeys[i].spml_context = (void *)(unsigned long)size;
                break;

            default:
                SPML_ERROR("unsupported PTL: %d", i);
                goto err;
        }
        SPML_VERBOSE(5,"rank %d ptl %d rkey %x lkey %x key %llx address 0x%llX len %llu shmid 0x%X|0x%X",
                oshmem_proc_local_proc->proc_name.vpid,
                i,
                mkeys[i].ib.rkey,
                mkeys[i].ib.lkey,
                (unsigned long long)mkeys[i].key,
                (unsigned long long)mkeys[i].va_base,
                (unsigned long long)size,
                MEMHEAP_SHM_GET_TYPE(shmid), MEMHEAP_SHM_GET_ID(shmid)
                );

    }
    *count = MXM_PTL_LAST;

    return mkeys;

err:
    mca_spml_ikrit_deregister(mkeys);
    return NULL;
}

int mca_spml_ikrit_deregister(mca_spml_mkey_t *mkeys)
{
    int i;

    if (!mkeys)
        return OSHMEM_SUCCESS;

    for (i = 0; i < MXM_PTL_LAST; i++) {
        switch(i) {
            case MXM_PTL_SELF:
            case MXM_PTL_SHM:
                break;
            case MXM_PTL_RDMA:
                /* dereg memory */
                if (!mkeys[i].spml_context)
                    break;
#if MXM_API < MXM_VERSION(1,5)
                mxm_dereg_mr(mca_spml_ikrit.mxm_ep, MXM_PTL_RDMA, 
                        (void *)mkeys[i].va_base,
                        (unsigned long)mkeys[i].spml_context
                        );
#endif
                break;
        }
    }
    return OSHMEM_SUCCESS;

}

static inline int get_ptl_id(int dst)
{
    oshmem_proc_t *proc;

    /* get endpoint and btl */
    proc = oshmem_proc_group_all(dst);
    if (!proc) {
        SPML_ERROR("Can not find destination proc for pe=%d", dst);
        oshmem_shmem_abort(-1);
        return -1;
    }
    return proc->transport_ids[0];
}

int  mca_spml_ikrit_oob_get_mkeys(int pe, uint32_t seg, mca_spml_mkey_t *mkeys)
{
    int ptl;

    ptl = get_ptl_id(pe);
    if (ptl < 0)
        return OSHMEM_ERROR;

    if (ptl != MXM_PTL_RDMA)
        return OSHMEM_ERROR;

    if (seg > 1)
        return OSHMEM_ERROR;

#if MXM_API < MXM_VERSION(1,5)
    mkeys[ptl].ib.rkey = MXM_MKEY_NONE;
#endif

    return OSHMEM_SUCCESS;
}


static int mca_spml_ikrit_get_helper(mxm_send_req_t *sreq, void *src_addr, size_t size, void *dst_addr, int src)
{
    /* shmem spec states that get() operations are blocking. So it is enough
       to have single mxm request. Also we count on mxm doing copy */
    uint64_t rva;
    mca_spml_mkey_t *r_mkey;
    int ptl_id;

    ptl_id = get_ptl_id(src); 
    /* already tried to send via shm and failed. go via rdma */
    if (ptl_id == MXM_PTL_SHM)
        ptl_id = MXM_PTL_RDMA;

    /**
     * Get the address to the remote rkey.
     **/
    r_mkey = mca_memheap.memheap_get_cached_mkey(src, (unsigned long)src_addr, ptl_id, &rva);
    if (!r_mkey) {
        SPML_ERROR("pe=%d: %p is not address of shared variable", src, src_addr);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

    SPML_VERBOSE(100, "get: pe:%d ptl=%d src=%p -> dst: %p sz=%d. src_rva=%p, src_rkey=0x%lx",
            src, ptl_id, src_addr, dst_addr, (int)size, (void *)rva, r_mkey->key);


    /* mxm does not really cares for get lkey */
    sreq->base.mq = mca_spml_ikrit.mxm_mq; 
    sreq->base.conn = mca_spml_ikrit.mxm_peers[src]->mxm_conn;
    sreq->base.data_type = MXM_REQ_DATA_BUFFER;
    sreq->base.data.buffer.ptr = dst_addr;
    sreq->base.data.buffer.length = size;
#if MXM_API < MXM_VERSION(1,5)
    sreq->base.data.buffer.mkey = MXM_MKEY_NONE;
    sreq->op.mem.remote_mkey = r_mkey->ib.rkey;
#else
    sreq->base.data.buffer.memh = NULL;
    sreq->op.mem.remote_memh = NULL;
#endif
    sreq->opcode = MXM_REQ_OP_GET;
    sreq->op.mem.remote_vaddr = (intptr_t)rva;
    sreq->base.state = MXM_REQ_NEW;

    return OSHMEM_SUCCESS;
}

static inline int mca_spml_ikrit_get_shm(void *src_addr, size_t size, void *dst_addr, int src)
{
    int ptl_id;
    uint64_t rva;
    mca_spml_mkey_t *r_mkey;

    ptl_id = get_ptl_id(src); 
    /**
     * Get the address to the remote rkey.
     **/
    if (ptl_id != MXM_PTL_SHM)
        return OSHMEM_ERROR;

    r_mkey = mca_memheap.memheap_get_cached_mkey(src, (unsigned long)src_addr, ptl_id, &rva);
    if (!r_mkey) {
        SPML_ERROR("pe=%d: %p is not address of shared variable", src, src_addr);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    
    if (OPAL_UNLIKELY(!mca_memheap.memheap_is_symmetric_addr((unsigned long)src_addr) || (unsigned long)src_addr == rva)) 
        return OSHMEM_ERROR;

    SPML_VERBOSE(100, "shm get: pe:%d src=%p -> dst: %p sz=%d. src_rva=%p, src_rkey=0x%lx",
            src, src_addr, dst_addr, (int)size, (void *)rva, r_mkey->key);
    memcpy(dst_addr, (void *)(unsigned long)rva, size);
    opal_progress();
    return OSHMEM_SUCCESS;
}

int mca_spml_ikrit_get(void *src_addr, size_t size, void *dst_addr, int src)
{
    mxm_send_req_t sreq;

    if (OSHMEM_SUCCESS == mca_spml_ikrit_get_shm(src_addr, size, dst_addr, src)) 
        return OSHMEM_SUCCESS;

    if (OSHMEM_SUCCESS != mca_spml_ikrit_get_helper(&sreq,  src_addr, size, dst_addr, src)) {
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

    sreq.base.flags = MXM_REQ_FLAG_BLOCKING;
    sreq.base.completed_cb = NULL;

    mxm_req_send(&sreq);
    opal_progress();
    mca_spml_irkit_req_wait(&sreq.base);

    if (MXM_OK != sreq.base.error) {
        SPML_ERROR("get request failed: %s - aborting", mxm_error_string(sreq.base.error));
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

static inline void get_completion_cb(void *ctx)
{
    mca_spml_ikrit_get_request_t *get_req = (mca_spml_ikrit_get_request_t *)ctx;

    OPAL_THREAD_ADD32(&mca_spml_ikrit.n_active_gets, -1);
    get_req->req_get.req_base.req_spml_complete = true;
    get_req->req_get.req_base.req_oshmem.req_status.SHMEM_ERROR = OSHMEM_SUCCESS;
    oshmem_request_complete( &get_req->req_get.req_base.req_oshmem, 1);
    oshmem_request_free( (oshmem_request_t**)&get_req );
}

/* extension. used 4 fence implementation b4 fence was added to mxm */
int mca_spml_ikrit_get_async(void *src_addr, size_t size, void *dst_addr, int src)
{
    mca_spml_ikrit_get_request_t *get_req;

    if (OSHMEM_SUCCESS == mca_spml_ikrit_get_shm(src_addr, size, dst_addr, src)) 
        return OSHMEM_SUCCESS;

    get_req = alloc_get_req();
    if (NULL == get_req) {
        SPML_ERROR("out of get requests - aborting");
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

    if (OSHMEM_SUCCESS != mca_spml_ikrit_get_helper(&get_req->mxm_req, src_addr, size, dst_addr, src)) {
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

    get_req->mxm_req.base.flags = 0;
    get_req->mxm_req.base.completed_cb = get_completion_cb;
    get_req->mxm_req.base.context = get_req;
    OPAL_THREAD_ADD32(&mca_spml_ikrit.n_active_gets, 1);

    mxm_req_send(&get_req->mxm_req);

    if (MXM_OK != get_req->mxm_req.base.error) {
        SPML_ERROR("get request failed: %s - aborting", mxm_error_string(get_req->mxm_req.base.error));
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

static inline void fence_completion_cb(void *ctx)
{
    mca_spml_ikrit_get_request_t *fence_req = (mca_spml_ikrit_get_request_t *)ctx;

    OPAL_THREAD_ADD32(&mca_spml_ikrit.n_mxm_fences, -1);
    fence_req->req_get.req_base.req_spml_complete = true;
    fence_req->req_get.req_base.req_oshmem.req_status.SHMEM_ERROR = OSHMEM_SUCCESS;
    oshmem_request_complete( &fence_req->req_get.req_base.req_oshmem, 1);
    oshmem_request_free( (oshmem_request_t**)&fence_req );
}

static int mca_spml_ikrit_mxm_fence(int dst)
{
    mca_spml_ikrit_get_request_t *fence_req;


    fence_req = alloc_get_req();
    if (NULL == fence_req) {
        SPML_ERROR("out of get requests - aborting");
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

    fence_req->mxm_req.base.mq = mca_spml_ikrit.mxm_mq; 
    fence_req->mxm_req.base.conn = mca_spml_ikrit.mxm_peers[dst]->mxm_conn;
    fence_req->mxm_req.opcode = MXM_REQ_OP_FENCE;
    fence_req->mxm_req.base.state = MXM_REQ_NEW;
    fence_req->mxm_req.base.flags = MXM_REQ_FLAG_SEND_SYNC;
    fence_req->mxm_req.base.completed_cb = fence_completion_cb;
    fence_req->mxm_req.base.context = fence_req;
    OPAL_THREAD_ADD32(&mca_spml_ikrit.n_mxm_fences, 1);

    mxm_req_send(&fence_req->mxm_req);
    return OSHMEM_SUCCESS;
}

static inline void put_completion_cb(void *ctx)
{
    mca_spml_ikrit_put_request_t *put_req = (mca_spml_ikrit_put_request_t *)ctx;
    mxm_peer_t *peer;

    OPAL_THREAD_ADD32(&mca_spml_ikrit.n_active_puts, -1);
    peer = mca_spml_ikrit.mxm_peers[put_req->pe];

    /* this was last put in progress. Remove peer from the list so that we do not need explicit fence */
#ifdef SPML_IKRIT_DEBUG_PUT
    if (peer) {
       if (peer->n_active_puts <= 0) {
           /* actually this can happen because fence forces ref count to 0 while puts still may be in flight */
           SPML_VERBOSE(1, "pe %d n_active_puts %d", put_req->pe, peer->n_active_puts);
       }
    }
    
    if (put_req->mxm_req.base.state != MXM_REQ_COMPLETED)
        SPML_ERROR("oops: pe %d uncompleted request state %d", put_req->pe, put_req->mxm_req.base.state);
#endif

    if (0 < peer->n_active_puts) {
        peer->n_active_puts--;
        if (0 == peer->n_active_puts && (put_req->mxm_req.base.flags & MXM_REQ_FLAG_SEND_SYNC)) {
            //SPML_VERBOSE(20, "removed pe %d from active list", put_req->pe); 
            opal_list_remove_item(&mca_spml_ikrit.active_peers, &peer->super);
            peer->need_fence = 0;
        }
    }

    put_req->req_put.req_base.req_spml_complete = true;
    put_req->req_put.req_base.req_oshmem.req_status.SHMEM_ERROR = OSHMEM_SUCCESS;
    oshmem_request_complete( &put_req->req_put.req_base.req_oshmem, 1);
    oshmem_request_free( (oshmem_request_t**)&put_req );
}

/**
 * TODO: using put request as handle is not good.
 */
static inline int mca_spml_ikrit_put_internal(void* dst_addr, size_t size, void* src_addr, int dst, void **handle, int zcopy)
{
    uint64_t rva;
    mca_spml_ikrit_put_request_t *put_req;
    int ptl_id;
    mca_spml_mkey_t *l_mkey, *r_mkey;
    uint32_t lkey;
    static int count;
    int need_progress = 0;

    ptl_id = get_ptl_id(dst); 
    /* Get rkey of remote PE (dst proc) which must be on memheap  */
    r_mkey = mca_memheap.memheap_get_cached_mkey(dst, (unsigned long)dst_addr, ptl_id, &rva);
    if(!r_mkey) {
        SPML_ERROR("pe=%d: %p is not address of shared variable", dst, dst_addr);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

#ifdef SPML_IKRIT_DEBUG_PUT 

    SPML_VERBOSE(100, "put: pe:%d ptl=%d dst=%p <- src: %p sz=%d. dst_rva=%p, dst_rkey=0x%lx",
                        dst, ptl_id, dst_addr, src_addr, (int)size, (void *)rva, r_mkey->key);
#endif
    if (ptl_id == MXM_PTL_SHM) {

        if (OPAL_LIKELY(mca_memheap.memheap_is_symmetric_addr((unsigned long)dst_addr) && (unsigned long)dst_addr != rva)) {
            memcpy((void *)(unsigned long)rva, src_addr, size);
            // call progress as often as we would have with regular put
            if (++count % SPML_IKRIT_PACKETS_PER_SYNC == 0)
                mxm_progress(mca_spml_ikrit.mxm_context);
            return OSHMEM_SUCCESS;
        }
        // segment not mapped - fallback to rmda
        ptl_id = MXM_PTL_RDMA;
        r_mkey = mca_memheap.memheap_get_cached_mkey(dst, (unsigned long)dst_addr, ptl_id, &rva);
        if(!r_mkey) {
            SPML_ERROR("pe=%d: %p is not address of shared variable", dst, dst_addr);
            oshmem_shmem_abort(-1);
            return OSHMEM_ERROR;
        }
    }

#ifdef SPML_IKRIT_DEBUG_PUT 
    SPML_VERBOSE(100, "put: pe:%d ptl=%d dst=%p <- src: %p sz=%d. dst_rva=%p, dst_rkey=0x%lx",
                        dst, ptl_id, dst_addr, src_addr, (int)size, (void *)rva, r_mkey->key);
#endif

    l_mkey =  mca_memheap.memheap_get_local_mkey((unsigned long)src_addr, ptl_id);
    if (zcopy == 0 || !l_mkey) {
        /* local memory is not registered - pass proper flag to mxm */
#if MXM_API < MXM_VERSION(1,5)
        lkey = MXM_MKEY_NONE;
#else
        lkey = 0;
#endif
    }
    else {
        lkey = l_mkey->ib.lkey;
    }

    put_req = alloc_put_req();
    if (NULL == put_req) {
        SPML_ERROR("out of put requests - aborting");
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    if (handle) 
        *handle = put_req;

    /* fill out request */
    put_req->mxm_req.base.mq = mca_spml_ikrit.mxm_mq; 
    /* request immediate responce if we are getting low on send buffers. We only get responce from remote on ack timeout.
     * Also request explicit ack once in a while  */
    if (mca_spml_ikrit.free_list_max - mca_spml_ikrit.n_active_puts <= SPML_IKRIT_PUT_LOW_WATER ||
            (mca_spml_ikrit.mxm_peers[dst]->n_active_puts + 1) % SPML_IKRIT_PACKETS_PER_SYNC == 0) {
        put_req->mxm_req.base.flags = MXM_REQ_FLAG_SEND_SYNC;
        need_progress = 1;
    } else  {
        put_req->mxm_req.base.flags = MXM_REQ_FLAG_SEND_LAZY|MXM_REQ_FLAG_SEND_SYNC; 
    }

    put_req->mxm_req.base.conn = mca_spml_ikrit.mxm_peers[dst]->mxm_conn;
    put_req->mxm_req.base.data_type = MXM_REQ_DATA_BUFFER;
    put_req->mxm_req.base.data.buffer.ptr = src_addr;
    put_req->mxm_req.base.data.buffer.length = size;
    put_req->mxm_req.base.completed_cb = put_completion_cb; 
    put_req->mxm_req.base.context = put_req;
    put_req->mxm_req.opcode = MXM_REQ_OP_PUT;
    put_req->mxm_req.op.mem.remote_vaddr = (intptr_t)rva;
    put_req->mxm_req.base.state = MXM_REQ_NEW;
    put_req->pe = dst;

#if MXM_API < MXM_VERSION(1,5)
    put_req->mxm_req.base.data.buffer.mkey = lkey;
    put_req->mxm_req.op.mem.remote_mkey = r_mkey->ib.rkey;
#else
    put_req->mxm_req.base.data.buffer.memh = NULL;
    put_req->mxm_req.op.mem.remote_memh = NULL;
#endif

    if (mca_spml_ikrit.mxm_peers[dst]->pe_relay >= 0 &&
        mca_memheap_base_detect_addr_type((unsigned long)dst_addr) == ADDR_USER ) {
        put_req->mxm_req.op.am.hid = 0;
        put_req->mxm_req.op.am.imm_data = dst;
        put_req->pe = mca_spml_ikrit.mxm_peers[dst]->pe_relay;
        put_req->mxm_req.base.conn = mca_spml_ikrit.mxm_peers[put_req->pe]->mxm_conn;
        put_req->mxm_req.opcode = MXM_REQ_OP_AM;
        
        /* set up iov */
        put_req->mxm_req.base.data_type = MXM_REQ_DATA_IOV;
        put_req->mxm_req.base.data.iov.count = 2;
        put_req->mxm_req.base.data.iov.vector = put_req->iov;

        put_req->iov[0].ptr    = &put_req->am_pkt.va;
        put_req->iov[0].length = sizeof(uint64_t);
        put_req->am_pkt.va     = (uint64_t)rva;

        put_req->iov[1].ptr    = src_addr;
        put_req->iov[1].length = size;

#if MXM_API < MXM_VERSION(1,5)
        put_req->iov[0].mkey   = MXM_MKEY_NONE;
        put_req->iov[1].mkey   = lkey;
#else
        put_req->iov[0].memh   = NULL;
        put_req->iov[1].memh   = NULL;
#endif
    }

    OPAL_THREAD_ADD32(&mca_spml_ikrit.n_active_puts, 1);
    if (mca_spml_ikrit.mxm_peers[dst]->need_fence == 0) {
        //SPML_VERBOSE(20, "added pe %d to active list", dst); 
        opal_list_append(&mca_spml_ikrit.active_peers, &mca_spml_ikrit.mxm_peers[dst]->super);
        mca_spml_ikrit.mxm_peers[dst]->need_fence = 1; 
    }

    mca_spml_ikrit.mxm_peers[dst]->n_active_puts++;

    mxm_req_send(&put_req->mxm_req);

    if (MXM_OK != put_req->mxm_req.base.error) {
        OPAL_THREAD_ADD32(&mca_spml_ikrit.n_active_puts, -1);
        SPML_ERROR("put request %p failed: %s - aborting", put_req, mxm_error_string(put_req->mxm_req.base.error));
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    //put_completion_cb(put_req);
    if (need_progress) 
        mxm_progress(mca_spml_ikrit.mxm_context);

    return OSHMEM_SUCCESS;
}

/* simple buffered put implementation. NOT IN USE
 * Problems:
 * - slighly worse performance than impl based on non buffered put
 * - fence complexity is O(n_active_connections) instead of O(n_connections_with_outstanding_puts).
 *   Later is bounded by the network RTT & mxm ack timer. 
 */
int mca_spml_ikrit_put_simple(void* dst_addr, size_t size, void* src_addr, int dst)
{
    uint64_t rva;
    mxm_send_req_t  mxm_req;
    mxm_wait_t wait;
    int ptl_id;
    mca_spml_mkey_t *r_mkey;
    static int count;

    ptl_id = get_ptl_id(dst); 
    /* Get rkey of remote PE (dst proc) which must be on memheap  */
    r_mkey = mca_memheap.memheap_get_cached_mkey(dst, (unsigned long)dst_addr, ptl_id, &rva);
    if(!r_mkey) {
        SPML_ERROR("pe=%d: %p is not address of shared variable", dst, dst_addr);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

#ifdef SPML_IKRIT_DEBUG_PUT 

    SPML_VERBOSE(100, "put: pe:%d ptl=%d dst=%p <- src: %p sz=%d. dst_rva=%p, dst_rkey=0x%lx",
                        dst, ptl_id, dst_addr, src_addr, (int)size, (void *)rva, r_mkey->key);
#endif
    if (ptl_id == MXM_PTL_SHM) {

        if (OPAL_LIKELY(mca_memheap.memheap_is_symmetric_addr((unsigned long)dst_addr) && (unsigned long)dst_addr != rva)) {
            memcpy((void *)(unsigned long)rva, src_addr, size);
            // call progress as often as we would have with regular put
            if (++count % SPML_IKRIT_PACKETS_PER_SYNC == 0)
                mxm_progress(mca_spml_ikrit.mxm_context);
            return OSHMEM_SUCCESS;
        }
        // segment not mapped - fallback to rmda
        ptl_id = MXM_PTL_RDMA;
        r_mkey = mca_memheap.memheap_get_cached_mkey(dst, (unsigned long)dst_addr, ptl_id, &rva);
        if(!r_mkey) {
            SPML_ERROR("pe=%d: %p is not address of shared variable", dst, dst_addr);
            oshmem_shmem_abort(-1);
            return OSHMEM_ERROR;
        }
    }

#ifdef SPML_IKRIT_DEBUG_PUT 
    SPML_VERBOSE(100, "put: pe:%d ptl=%d dst=%p <- src: %p sz=%d. dst_rva=%p, dst_rkey=0x%lx",
                        dst, ptl_id, dst_addr, src_addr, (int)size, (void *)rva, r_mkey->key);
#endif


    /* fill out request */
    mxm_req.base.mq = mca_spml_ikrit.mxm_mq; 
    mxm_req.base.flags = MXM_REQ_FLAG_BLOCKING;
    mxm_req.base.conn = mca_spml_ikrit.mxm_peers[dst]->mxm_conn;
    mxm_req.base.data_type = MXM_REQ_DATA_BUFFER;
    mxm_req.base.data.buffer.ptr = src_addr;
    mxm_req.base.data.buffer.length = size;
    mxm_req.base.completed_cb = 0; 
    mxm_req.base.context = 0;
    mxm_req.opcode = MXM_REQ_OP_PUT;
    mxm_req.op.mem.remote_vaddr = (intptr_t)rva;
    mxm_req.base.state = MXM_REQ_NEW;
    mxm_req.base.error = MXM_OK;

#if MXM_API < MXM_VERSION(1,5)
    mxm_req.base.data.buffer.mkey = MXM_MKEY_NONE;
    mxm_req.op.mem.remote_mkey = MXM_MKEY_NONE;
#else
    mxm_req.base.data.buffer.memh = NULL;
    mxm_req.op.mem.remote_memh = NULL;
#endif


    if (mca_spml_ikrit.mxm_peers[dst]->need_fence == 0) {
        //SPML_VERBOSE(20, "added pe %d to active list", dst); 
        opal_list_append(&mca_spml_ikrit.active_peers, &mca_spml_ikrit.mxm_peers[dst]->super);
        mca_spml_ikrit.mxm_peers[dst]->need_fence = 1; 
    }

    mxm_req_send(&mxm_req);
    if (MXM_OK != mxm_req.base.error) {
        SPML_ERROR("put request failed: %s(%d) - aborting", mxm_error_string(mxm_req.base.error), mxm_req.base.error);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }


    wait.req            = &mxm_req.base;
    wait.state          = (mxm_req_state_t)(MXM_REQ_SENT|MXM_REQ_COMPLETED);
    wait.progress_cb    = NULL;
    wait.progress_arg   = NULL;
    mxm_wait(&wait);

    return OSHMEM_SUCCESS;
}

int mca_spml_ikrit_put_nb(void* dst_addr, size_t size, void* src_addr, int dst, void **handle)
{
    int err;
    err = mca_spml_ikrit_put_internal(dst_addr, size, src_addr, dst, handle, 1);
    if (OSHMEM_SUCCESS != err) {
        SPML_ERROR("put failed - aborting");
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

int mca_spml_ikrit_put(void* dst_addr, size_t size, void* src_addr, int dst)
{
    int err;
    mca_spml_ikrit_put_request_t *put_req;
    mxm_wait_t wait;

    put_req = 0;
    err = mca_spml_ikrit_put_internal(dst_addr, size, src_addr, dst, (void **)&put_req, 0);
    if (OSHMEM_SUCCESS != err) {
        SPML_ERROR("put failed - aborting");
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    if (!put_req)
        return OSHMEM_SUCCESS;

    wait.req            = &put_req->mxm_req.base;
    wait.state          = (mxm_req_state_t)(MXM_REQ_SENT|MXM_REQ_COMPLETED);
    wait.progress_cb    = NULL;
    wait.progress_arg   = NULL;
    mxm_wait(&wait);

    return OSHMEM_SUCCESS;
}

static void mxm_relay_handler(mxm_conn_h conn, mxm_imm_t imm, void *data, size_t len, size_t offset, int is_lf)
{
    uint64_t va, rva;
    char *pkt_data;
    mca_spml_mkey_t *r_mkey;
    int ptl_id;
    mxm_peer_t *peer;

    //printf("relay req recvd conn=%p data=%p dst=%d len=%d offset=%d is_lf=%d - aborting\n", 
    //       conn, data, (int)imm, (int)len, (int)offset, is_lf);

    if (offset == 0) {
        va = *(uint64_t *)data;
        pkt_data = (char *)data + sizeof(va);     
        len -= sizeof(va);
        if (!is_lf) {
            // we expect more fragments: save destination virtual address 
            peer = mxm_conn_ctx_get(conn);
            peer->dst_va = va;
        }
    }
    else {
        // next fragment: use saved va and offset to compute va
        pkt_data = data;
        peer = mxm_conn_ctx_get(conn);
        va = peer->dst_va + offset - sizeof(va);
    }

    ptl_id = get_ptl_id(imm); 
    if (ptl_id != MXM_PTL_SHM) {
        SPML_ERROR("relay req to non local PE recvd dst=%d va=%llx len=%d - aborting", (int)imm, (unsigned long long)va, (int)len);
        oshmem_shmem_abort(-1);
        return;
    }

    /* Get rkey of remote PE (dst proc) which must be on memheap  */
    r_mkey = mca_memheap.memheap_get_cached_mkey(imm, va, ptl_id, &rva);
    if(!r_mkey) {
        SPML_ERROR("relay to PE=%d: %p is not address of shared variable", imm, (void *)va);
        oshmem_shmem_abort(-1);
        return;
    }

    memcpy((void *)(unsigned long)rva, pkt_data, len);
}

static void mxm_setup_relays(oshmem_proc_t **procs, size_t nprocs)
{
    size_t i;
    //long i;
    opal_hash_table_t h;
    int pe_relay;
    int ret;
    int r_i, r;

    if (mca_spml_ikrit.n_relays <= 0)
        return;

    OBJ_CONSTRUCT(&h, opal_hash_table_t);
    opal_hash_table_init(&h, 128);

    /* lowest rank on node will be used to relay to everyone on that node */
    for (i = 0; i < nprocs; i++) {
    //for (i = nprocs-1; i >= 0; i--) {
        if (OPAL_PROC_ON_LOCAL_NODE(procs[i]->proc_flags)) 
            continue;

        ret = opal_hash_table_get_value_ptr(&h, procs[i]->proc_hostname, strlen(procs[i]->proc_hostname), (void **)&pe_relay);
        if (ret != OPAL_SUCCESS) {
            opal_hash_table_set_value_ptr(&h, procs[i]->proc_hostname, strlen(procs[i]->proc_hostname), (void *)i);
            mca_spml_ikrit.mxm_peers[i]->n_relays = 1;
            mca_spml_ikrit.mxm_peers[i]->pe_relays[0] = i;
            continue;
        }
#if 0
        if (mca_spml_ikrit.mxm_peers[pe_relay]->n_slaves >= 15) {
            opal_hash_table_set_value_ptr(&h, procs[i]->proc_hostname, strlen(procs[i]->proc_hostname), (void *)i);
        }
        else {
            mca_spml_ikrit.mxm_peers[pe_relay]->n_slaves++;
            mca_spml_ikrit.mxm_peers[i]->pe_relay = pe_relay;
//            printf("dst %d relay %d\n", (int)i, pe_relay);
        }
#endif
        /* first allocate relays */
        if (mca_spml_ikrit.mxm_peers[pe_relay]->n_relays < mca_spml_ikrit.n_relays) {
            //printf("r_i=%d assigned relay %d\n", mca_spml_ikrit.mxm_peers[pe_relay]->n_relays, (int)i);
            mca_spml_ikrit.mxm_peers[pe_relay]->pe_relays[mca_spml_ikrit.mxm_peers[pe_relay]->n_relays] = i;
            mca_spml_ikrit.mxm_peers[pe_relay]->n_relays++;
            continue;
        }

        /* now assign slave to relay */
        r_i = mca_spml_ikrit.mxm_peers[pe_relay]->n_relays - 1;
        while (r_i >= 0) { 
            r = mca_spml_ikrit.mxm_peers[pe_relay]->pe_relays[r_i];
            if (mca_spml_ikrit.mxm_peers[r]->n_slaves >= 1) {
                r_i--;
                continue;
            }
            mca_spml_ikrit.mxm_peers[r]->n_slaves++;
            mca_spml_ikrit.mxm_peers[i]->pe_relay = r;
            //printf("dst %d relay %d r_idx %d master %d\n", (int)i, r, r_i, pe_relay);
            break;

        }
        //mca_spml_ikrit.mxm_peers[pe_relay]->n_relays  = r_i;
    }

    OBJ_DESTRUCT(&h);
    mxm_set_am_handler(mca_spml_ikrit.mxm_context, 0, mxm_relay_handler, MXM_AM_FLAG_THREAD_SAFE);
}

int mca_spml_ikrit_fence(void)
{
    mxm_peer_t *peer;
    opal_list_item_t *item;


    SPML_VERBOSE(20, "Into fence with %d active puts on %d pes", 
            mca_spml_ikrit.n_active_puts,
            (int)opal_list_get_size(&mca_spml_ikrit.active_peers)
            );

    /* puts(unless are send sync) are completed by remote side lazily. That is either when remote decides to 
     * ack window which can take hundreds of ms. So speed things up by doing fence */
    while (NULL != (item = opal_list_remove_first(&mca_spml_ikrit.active_peers))) {
        peer = (mxm_peer_t *)item;
        peer->n_active_puts = 0;
        peer->need_fence = 0;
        mca_spml_ikrit_mxm_fence(peer->pe);
    }

    while (0 < mca_spml_ikrit.n_mxm_fences) {
        oshmem_request_wait_any_completion();
    }

    SPML_VERBOSE(20, "fence completed");
    return OSHMEM_SUCCESS;
}



/* blocking receive */
int mca_spml_ikrit_recv(void* buf, size_t size, int src)
{
    mxm_recv_req_t req;
    char dummy_buf[1];

    /* tag mask 0 matches any tag */
    SPML_VERBOSE(100, "want to recv from src %d, size %d buf %p", src, (int)size, buf);
    req.tag                  = src == SHMEM_ANY_SOURCE ? 0 : src;
    req.tag_mask             = src == SHMEM_ANY_SOURCE ? 0 : 0xFFFFFFFF;

    req.base.state           = MXM_REQ_NEW;
    req.base.mq              = mca_spml_ikrit.mxm_mq;
    req.base.conn            = NULL;
    req.base.flags           = MXM_REQ_FLAG_BLOCKING;
    req.base.completed_cb    = NULL;

    req.base.data_type           = MXM_REQ_DATA_BUFFER;
    req.base.data.buffer.ptr     = buf == NULL ? dummy_buf : buf;
    req.base.data.buffer.length  = size == 0 ? sizeof(dummy_buf) : size;
#if MXM_API < MXM_VERSION(1,5)
    req.base.data.buffer.mkey    = MXM_MKEY_NONE;
#else
    req.base.data.buffer.memh    = NULL;
#endif

    mxm_req_recv(&req);
    mca_spml_irkit_req_wait(&req.base);
    if (req.base.error != MXM_OK) {
        return OSHMEM_ERROR;
    }
    SPML_VERBOSE(100, "recvd from tag %d len %d", req.completion.sender_tag, 
            (int)req.completion.actual_len);

    return OSHMEM_SUCCESS;
}


/* for now only do blocking copy send */
int mca_spml_ikrit_send(void* buf, size_t size, int dst, mca_spml_base_put_mode_t mode)
{
    mxm_send_req_t req;
    char dummy_buf[1];
    
    SPML_VERBOSE(100, "sending %p size %d to %d, mode %d", buf, (int)size, dst, (int)mode);
    req.opcode               = MXM_REQ_OP_SEND;
    // FIXME: doing tagging like this can cause a conflict with MPI mtl
    req.op.send.tag          = oshmem_my_proc_id();

    req.base.state           = MXM_REQ_NEW;
    req.base.mq              = mca_spml_ikrit.mxm_mq;
    req.base.conn            = mca_spml_ikrit.mxm_peers[dst]->mxm_conn;
    req.base.flags           = MXM_REQ_FLAG_BLOCKING;
    req.base.completed_cb    = NULL;

    req.base.data_type           = MXM_REQ_DATA_BUFFER;
    req.base.data.buffer.ptr     = buf == NULL ? dummy_buf : buf;
    req.base.data.buffer.length  = size == 0 ? sizeof(dummy_buf) : size;
#if MXM_API < MXM_VERSION(1,5)
    req.base.data.buffer.mkey    = MXM_MKEY_NONE;
#else
    req.base.data.buffer.memh    = NULL;
#endif

    mxm_req_send(&req);
    mca_spml_irkit_req_wait(&req.base);
    if (req.base.error != MXM_OK) {
        return OSHMEM_ERROR;
    }

    return OSHMEM_SUCCESS;
}
