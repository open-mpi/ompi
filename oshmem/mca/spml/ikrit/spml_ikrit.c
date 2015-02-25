/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
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

#include "oshmem/mca/spml/ikrit/spml_ikrit_component.h"

/* Turn ON/OFF debug output from build (default 0) */
#ifndef SPML_IKRIT_PUT_DEBUG
#define SPML_IKRIT_PUT_DEBUG    0
#endif

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

#if MXM_API < MXM_VERSION(2,0)
static int spml_ikrit_get_ep_address(spml_ikrit_mxm_ep_conn_info_t *ep_info,
                                     mxm_ptl_id_t ptlid)
{
    size_t addrlen;
    mxm_error_t err;

    addrlen = sizeof(ep_info->addr.ptl_addr[ptlid]);
    err = mxm_ep_address(mca_spml_ikrit.mxm_ep,
                         ptlid,
                         (struct sockaddr *) &ep_info->addr.ptl_addr[ptlid],
                         &addrlen);
    if (MXM_OK != err) {
        orte_show_help("help-oshmem-spml-ikrit.txt",
                       "unable to get endpoint address",
                       true,
                       mxm_error_string(err));
        return OSHMEM_ERROR;
    }

    return OSHMEM_SUCCESS;
}
#else
static inline mxm_mem_key_t *to_mxm_mkey(sshmem_mkey_t *mkey) {

    if (0 == mkey->len) {
        return &mxm_empty_mem_key;
    }
    return (mxm_mem_key_t *)mkey->u.data;
}
#endif

static inline void mca_spml_irkit_req_wait(mxm_req_base_t *req)
{
    while (!mxm_req_test(req))
        opal_progress();
}

static int mca_spml_ikrit_put_request_free(struct oshmem_request_t** request)
{
    mca_spml_ikrit_put_request_t *put_req =
            *(mca_spml_ikrit_put_request_t **) request;

    assert(false == put_req->req_put.req_base.req_free_called);
    OPAL_THREAD_LOCK(&oshmem_request_lock);
    put_req->req_put.req_base.req_free_called = true;
    opal_free_list_return (&mca_spml_base_put_requests,
                           (opal_free_list_item_t*)put_req);
    OPAL_THREAD_UNLOCK(&oshmem_request_lock);

    *request = SHMEM_REQUEST_NULL; /*MPI_REQUEST_NULL;*/

    return OSHMEM_SUCCESS;
}

static int mca_spml_ikrit_put_request_cancel(struct oshmem_request_t * request,
                                             int complete)
{
    return OSHMEM_SUCCESS;
}

static void mca_spml_ikrit_put_request_construct(mca_spml_ikrit_put_request_t* req)
{
    req->req_put.req_base.req_type = MCA_SPML_REQUEST_PUT;
    req->req_put.req_base.req_oshmem.req_free = mca_spml_ikrit_put_request_free;
    req->req_put.req_base.req_oshmem.req_cancel =
            mca_spml_ikrit_put_request_cancel;
}

static void mca_spml_ikrit_put_request_destruct(mca_spml_ikrit_put_request_t* req)
{
}

OBJ_CLASS_INSTANCE( mca_spml_ikrit_put_request_t,
                   mca_spml_base_put_request_t,
                   mca_spml_ikrit_put_request_construct,
                   mca_spml_ikrit_put_request_destruct);

struct mca_spml_ikrit_get_request {
    mca_spml_base_get_request_t req_get;
    mxm_send_req_t mxm_req;
};

typedef struct mca_spml_ikrit_get_request mca_spml_ikrit_get_request_t;
OBJ_CLASS_DECLARATION(mca_spml_ikrit_get_request_t);

static int mca_spml_ikrit_get_request_free(struct oshmem_request_t** request)
{
    mca_spml_ikrit_get_request_t *get_req =
            *(mca_spml_ikrit_get_request_t **) request;

    assert(false == get_req->req_get.req_base.req_free_called);
    OPAL_THREAD_LOCK(&oshmem_request_lock);
    get_req->req_get.req_base.req_free_called = true;
    opal_free_list_return (&mca_spml_base_get_requests,
                           (opal_free_list_item_t*)get_req);
    OPAL_THREAD_UNLOCK(&oshmem_request_lock);

    *request = SHMEM_REQUEST_NULL; /*MPI_REQUEST_NULL;*/

    return OSHMEM_SUCCESS;
}

static int mca_spml_ikrit_get_request_cancel(struct oshmem_request_t * request,
                                             int complete)
{
    return OSHMEM_SUCCESS;
}

static void mca_spml_ikrit_get_request_construct(mca_spml_ikrit_get_request_t* req)
{
    req->req_get.req_base.req_type = MCA_SPML_REQUEST_PUT;
    req->req_get.req_base.req_oshmem.req_free = mca_spml_ikrit_get_request_free;
    req->req_get.req_base.req_oshmem.req_cancel =
            mca_spml_ikrit_get_request_cancel;
}

static void mca_spml_ikrit_get_request_destruct(mca_spml_ikrit_get_request_t* req)
{
}

OBJ_CLASS_INSTANCE( mca_spml_ikrit_get_request_t,
                   mca_spml_base_get_request_t,
                   mca_spml_ikrit_get_request_construct,
                   mca_spml_ikrit_get_request_destruct);

int mca_spml_ikrit_put_simple(void* dst_addr,
                              size_t size,
                              void* src_addr,
                              int dst);

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
        mca_spml_ikrit_recv,
        mca_spml_ikrit_send,
        mca_spml_base_wait,
        mca_spml_base_wait_nb,
        mca_spml_ikrit_fence,
        (void*)&mca_spml_ikrit
    }
};

#if MXM_API < MXM_VERSION(2,0)
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
        mxm_print_conn_state(mca_spml_ikrit.mxm_peers[i]->mxm_conn,
                             MXM_STATE_DETAIL_LEVEL_DATA,
                             "",
                             fp);
        printf("=========== pe:%d conn:%p stats:\n %s==================\n",
               i,
               mca_spml_ikrit.mxm_peers[i]->mxm_conn,
               sbuf);
        rewind(fp);
    }
    fclose(fp);
}
#endif

static inline mca_spml_ikrit_put_request_t *alloc_put_req(void)
{
    mca_spml_ikrit_put_request_t *req;
    opal_free_list_item_t* item;

    item = opal_free_list_wait (&mca_spml_base_put_requests);

    req = (mca_spml_ikrit_put_request_t *) item;
    req->req_put.req_base.req_free_called = false;
    req->req_put.req_base.req_oshmem.req_complete = false;

    return req;
}

static inline mca_spml_ikrit_get_request_t *alloc_get_req(void)
{
    mca_spml_ikrit_get_request_t *req;
    opal_free_list_item_t* item;

    item = opal_free_list_wait (&mca_spml_base_get_requests);

    req = (mca_spml_ikrit_get_request_t *) item;
    req->req_get.req_base.req_free_called = false;
    req->req_get.req_base.req_oshmem.req_complete = false;

    return req;
}

int mca_spml_ikrit_enable(bool enable)
{
    SPML_VERBOSE(50, "*** ikrit ENABLED ****");
    if (false == enable) {
        return OSHMEM_SUCCESS;
    }

    opal_free_list_init (&mca_spml_base_put_requests,
                         sizeof(mca_spml_ikrit_put_request_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_spml_ikrit_put_request_t),
                         0,
                         opal_cache_line_size,
                         mca_spml_ikrit.free_list_num,
                         mca_spml_ikrit.free_list_max,
                         mca_spml_ikrit.free_list_inc,
                         NULL, 0, NULL, NULL, NULL);

    opal_free_list_init (&mca_spml_base_get_requests,
                         sizeof(mca_spml_ikrit_get_request_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_spml_ikrit_get_request_t),
                         0,
                         opal_cache_line_size,
                         mca_spml_ikrit.free_list_num,
                         mca_spml_ikrit.free_list_max,
                         mca_spml_ikrit.free_list_inc,
                         NULL, 0, NULL, NULL, NULL);

    mca_spml_ikrit.enabled = true;

    return OSHMEM_SUCCESS;
}

static int create_ptl_idx(int dst_pe)
{
    oshmem_proc_t *proc;

    proc = oshmem_proc_group_find(oshmem_group_all, dst_pe);

    proc->transport_ids = (char *) malloc(MXM_PTL_LAST * sizeof(char));
    if (!proc->transport_ids)
        return OSHMEM_ERROR;

    proc->num_transports = 1;
#if MXM_API < MXM_VERSION(2,0)
    if (oshmem_my_proc_id() == dst_pe)
        proc->transport_ids[0] = MXM_PTL_SELF;
    else
#endif
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
}

static void mxm_peer_destruct(mxm_peer_t *p)
{
    /* may be we need to remov item from list */
}

OBJ_CLASS_INSTANCE( mxm_peer_t,
                   opal_list_item_t,
                   mxm_peer_construct,
                   mxm_peer_destruct);

int mca_spml_ikrit_del_procs(oshmem_proc_t** procs, size_t nprocs)
{
    size_t i, n;
    int my_rank = oshmem_my_proc_id();

    oshmem_shmem_barrier();
#if MXM_API >= MXM_VERSION(2,0)
    if (mca_spml_ikrit.bulk_disconnect) {
        mxm_ep_powerdown(mca_spml_ikrit.mxm_ep);
    }
#endif

    while (NULL != opal_list_remove_first(&mca_spml_ikrit.active_peers)) {
    };
    OBJ_DESTRUCT(&mca_spml_ikrit.active_peers);

    for (n = 0; n < nprocs; n++) {
        i = (my_rank + n) % nprocs;
        if (mca_spml_ikrit.mxm_peers[i]->mxm_conn) {
            mxm_ep_disconnect(mca_spml_ikrit.mxm_peers[i]->mxm_conn);
        }
        if (mca_spml_ikrit.hw_rdma_channel && mca_spml_ikrit.mxm_peers[i]->mxm_hw_rdma_conn) {
            mxm_ep_disconnect(mca_spml_ikrit.mxm_peers[i]->mxm_hw_rdma_conn);
        }
        destroy_ptl_idx(i);
        if (mca_spml_ikrit.mxm_peers[i]) {
            OBJ_RELEASE(mca_spml_ikrit.mxm_peers[i]);
        }
    }
    if (mca_spml_ikrit.mxm_peers)
        free(mca_spml_ikrit.mxm_peers);

    return OSHMEM_SUCCESS;
}

int mca_spml_ikrit_add_procs(oshmem_proc_t** procs, size_t nprocs)
{
    spml_ikrit_mxm_ep_conn_info_t *ep_info = NULL;
    spml_ikrit_mxm_ep_conn_info_t *ep_hw_rdma_info = NULL;
    spml_ikrit_mxm_ep_conn_info_t my_ep_info;
#if MXM_API < MXM_VERSION(2,0)
    mxm_conn_req_t *conn_reqs;
    int timeout;
#else
    size_t mxm_addr_len = MXM_MAX_ADDR_LEN;
#endif
    mxm_error_t err;
    size_t i, n;
    int rc = OSHMEM_ERROR;
    oshmem_proc_t *proc_self;
    int my_rank = oshmem_my_proc_id();

    OBJ_CONSTRUCT(&mca_spml_ikrit.active_peers, opal_list_t);
    /* Allocate connection requests */
#if MXM_API < MXM_VERSION(2,0)
    conn_reqs = malloc(nprocs * sizeof(mxm_conn_req_t));
    if (NULL == conn_reqs) {
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto bail;
    }
    memset(conn_reqs, 0x0, sizeof(mxm_conn_req_t));
#endif
    ep_info = malloc(nprocs * sizeof(spml_ikrit_mxm_ep_conn_info_t));
    if (NULL == ep_info) {
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto bail;
    }
    memset(ep_info, 0x0, sizeof(spml_ikrit_mxm_ep_conn_info_t));

    if (mca_spml_ikrit.hw_rdma_channel) {
        ep_hw_rdma_info = malloc(nprocs * sizeof(spml_ikrit_mxm_ep_conn_info_t));
        if (NULL == ep_hw_rdma_info) {
            rc = OSHMEM_ERR_OUT_OF_RESOURCE;
            goto bail;
        }
        memset(ep_hw_rdma_info, 0x0, sizeof(spml_ikrit_mxm_ep_conn_info_t));
    }

    mca_spml_ikrit.mxm_peers = (mxm_peer_t **) malloc(nprocs
            * sizeof(*(mca_spml_ikrit.mxm_peers)));
    if (NULL == mca_spml_ikrit.mxm_peers) {
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto bail;
    }

#if MXM_API < MXM_VERSION(2,0)
    if (OSHMEM_SUCCESS
            != spml_ikrit_get_ep_address(&my_ep_info, MXM_PTL_SELF)) {
        rc = OSHMEM_ERROR;
        goto bail;
    }
    if (OSHMEM_SUCCESS
            != spml_ikrit_get_ep_address(&my_ep_info, MXM_PTL_RDMA)) {
        rc = OSHMEM_ERROR;
        goto bail;
    }
#else
    if (mca_spml_ikrit.hw_rdma_channel) {
        err = mxm_ep_get_address(mca_spml_ikrit.mxm_hw_rdma_ep, &my_ep_info.addr.ep_addr, &mxm_addr_len);
        if (MXM_OK != err) {
            orte_show_help("help-oshmem-spml-ikrit.txt", "unable to get endpoint address", true,
                    mxm_error_string(err));
            rc = OSHMEM_ERROR;
            goto bail;
        }
        oshmem_shmem_allgather(&my_ep_info, ep_hw_rdma_info,
                sizeof(spml_ikrit_mxm_ep_conn_info_t));
    }
    err = mxm_ep_get_address(mca_spml_ikrit.mxm_ep, &my_ep_info.addr.ep_addr, &mxm_addr_len);
    if (MXM_OK != err) {
        orte_show_help("help-oshmem-spml-ikrit.txt", "unable to get endpoint address", true,
                mxm_error_string(err));
        rc = OSHMEM_ERROR;
        goto bail;
    }
#endif
    oshmem_shmem_allgather(&my_ep_info, ep_info,
                           sizeof(spml_ikrit_mxm_ep_conn_info_t));

    opal_progress_register(spml_ikrit_progress);

    /* Get the EP connection requests for all the processes from modex */
    for (n = 0; n < nprocs; ++n) {

        /* mxm 2.0 keeps its connections on a list. Make sure
         * that list have different order on every rank */
        i = (my_rank + n) % nprocs;
        mca_spml_ikrit.mxm_peers[i] = OBJ_NEW(mxm_peer_t);
        if (NULL == mca_spml_ikrit.mxm_peers[i]) {
            rc = OSHMEM_ERR_OUT_OF_RESOURCE;
            goto bail;
        }
        mca_spml_ikrit.mxm_peers[i]->pe = i;

#if MXM_API < MXM_VERSION(2,0)
        conn_reqs[i].ptl_addr[MXM_PTL_SELF] =
                (struct sockaddr *) &ep_info[i].addr.ptl_addr[MXM_PTL_SELF];
        conn_reqs[i].ptl_addr[MXM_PTL_SHM] = NULL;
        conn_reqs[i].ptl_addr[MXM_PTL_RDMA] =
                (struct sockaddr *) &ep_info[i].addr.ptl_addr[MXM_PTL_RDMA];
#else
        err = mxm_ep_connect(mca_spml_ikrit.mxm_ep, ep_info[i].addr.ep_addr, &mca_spml_ikrit.mxm_peers[i]->mxm_conn);
        if (MXM_OK != err) {
            SPML_ERROR("MXM returned connect error: %s\n", mxm_error_string(err));
            goto bail;
        }
        if (OSHMEM_SUCCESS != create_ptl_idx(i))
                goto bail;
        mxm_conn_ctx_set(mca_spml_ikrit.mxm_peers[i]->mxm_conn, mca_spml_ikrit.mxm_peers[i]);
        if (mca_spml_ikrit.hw_rdma_channel) {
            err = mxm_ep_connect(mca_spml_ikrit.mxm_hw_rdma_ep, ep_hw_rdma_info[i].addr.ep_addr, &mca_spml_ikrit.mxm_peers[i]->mxm_hw_rdma_conn);
            if (MXM_OK != err) {
                SPML_ERROR("MXM returned connect error: %s\n", mxm_error_string(err));
                goto bail;
            }
        } else {
            mca_spml_ikrit.mxm_peers[i]->mxm_hw_rdma_conn = mca_spml_ikrit.mxm_peers[i]->mxm_conn;
        }
#endif
    }

#if MXM_API < MXM_VERSION(2,0)
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
                SPML_ERROR("MXM EP connect to %s error: %s\n",
                           procs[i]->proc_hostname, mxm_error_string(conn_reqs[i].error));
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

        mxm_conn_ctx_set(conn_reqs[i].conn, mca_spml_ikrit.mxm_peers[i]);
    }

    if (conn_reqs)
        free(conn_reqs);
#endif
    if (ep_info)
        free(ep_info);
    if (ep_hw_rdma_info)
        free(ep_hw_rdma_info);

#if MXM_API >= MXM_VERSION(2,0)
    if (mca_spml_ikrit.bulk_connect) {
        /* Need a barrier to ensure remote peers already created connection */
        oshmem_shmem_barrier();
        mxm_ep_wireup(mca_spml_ikrit.mxm_ep);
    }
#endif

    proc_self = oshmem_proc_group_find(oshmem_group_all, my_rank);
    /* identify local processes and change transport to SHM */
    for (i = 0; i < nprocs; i++) {
        if (procs[i]->super.proc_name.jobid != proc_self->super.proc_name.jobid ||
            !OPAL_PROC_ON_LOCAL_NODE(procs[i]->super.proc_flags)) {
            continue;
        }
        if (procs[i] == proc_self)
        continue;

        /* use zcopy for put/get via sysv shared memory */
        procs[i]->transport_ids[0] = MXM_PTL_SHM;
        procs[i]->transport_ids[1] = MXM_PTL_RDMA;
        procs[i]->num_transports = 2;
    }

    SPML_VERBOSE(50, "*** ADDED PROCS ***");
    return OSHMEM_SUCCESS;

bail:
#if MXM_API < MXM_VERSION(2,0)
	if (conn_reqs)
		free(conn_reqs);
#endif
	if (ep_info)
		free(ep_info);
	if (ep_hw_rdma_info)
		free(ep_hw_rdma_info);
    SPML_ERROR("add procs FAILED rc=%d", rc);

    return rc;

}

sshmem_mkey_t *mca_spml_ikrit_register(void* addr,
                                         size_t size,
                                         uint64_t shmid,
                                         int *count)
{
    int i;
    sshmem_mkey_t *mkeys;
#if MXM_API >= MXM_VERSION(2,0)
    mxm_error_t err;
    mxm_mem_key_t *m_key;
#endif

    *count = 0;
    mkeys = (sshmem_mkey_t *) calloc(1, MXM_PTL_LAST * sizeof(*mkeys));
    if (!mkeys) {
        return NULL ;
    }

    for (i = 0; i < MXM_PTL_LAST; i++) {
        mkeys[i].u.key = MAP_SEGMENT_SHM_INVALID;
        switch (i) {
        case MXM_PTL_SHM:
            if ((int)shmid != MAP_SEGMENT_SHM_INVALID) {
                mkeys[i].u.key = shmid;
                mkeys[i].va_base = 0;
            } else {
                mkeys[i].len = 0;
                mkeys[i].va_base = addr;
            }
            mkeys[i].spml_context = 0;
            break;
#if MXM_API < MXM_VERSION(2,0)
        case MXM_PTL_SELF:
            mkeys[i].len = 0;
            mkeys[i].spml_context = 0;
            mkeys[i].va_base = addr;
            break;
#endif
        case MXM_PTL_RDMA:
            mkeys[i].va_base = addr;
            mkeys[i].spml_context = 0;
#if MXM_API < MXM_VERSION(2,0)
            mkeys[i].len = 0;
#else
            if (mca_spml_ikrit.ud_only && !mca_spml_ikrit.hw_rdma_channel) {
                mkeys[i].len = 0;
                break;
            }

            err = mxm_mem_map(mca_spml_ikrit.mxm_context, &addr, &size, 0, 0, 0);
            if (MXM_OK != err) {
                SPML_ERROR("Failed to register memory: %s", mxm_error_string(err));
                goto error_out;
            }
            mkeys[i].spml_context = (void *)(unsigned long)size;

            m_key = malloc(sizeof(*m_key));
            if (NULL == m_key) {
                SPML_ERROR("Failed to allocate m_key memory");
                goto error_out;
            }
            mkeys[i].len = sizeof(*m_key);
            mkeys[i].u.data = m_key;

            err = mxm_mem_get_key(mca_spml_ikrit.mxm_context, addr, m_key);
            if (MXM_OK != err) {
                SPML_ERROR("Failed to get memory key: %s", mxm_error_string(err));
                goto error_out;
            }
#endif
            break;

        default:
            SPML_ERROR("unsupported PTL: %d", i);
            goto error_out;
        }
        SPML_VERBOSE(5,
                     "rank %d ptl %d addr %p size %llu %s",
                     oshmem_proc_local_proc->super.proc_name.vpid, i, addr, (unsigned long long)size,
                     mca_spml_base_mkey2str(&mkeys[i]));

    }
    *count = MXM_PTL_LAST;

    return mkeys;

error_out:
    mca_spml_ikrit_deregister(mkeys);

    return NULL;
}

int mca_spml_ikrit_deregister(sshmem_mkey_t *mkeys)
{
    int i;

    MCA_SPML_CALL(fence());
    if (!mkeys)
        return OSHMEM_SUCCESS;

    for (i = 0; i < MXM_PTL_LAST; i++) {
        switch (i) {
#if MXM_API < MXM_VERSION(2,0)
        case MXM_PTL_SELF:
#endif
        case MXM_PTL_SHM:
            break;
        case MXM_PTL_RDMA:
            /* dereg memory */
            if (!mkeys[i].spml_context)
                break;
#if MXM_API >= MXM_VERSION(2,0)
            mxm_mem_unmap(mca_spml_ikrit.mxm_context,
                    (void *)mkeys[i].va_base,
                    (unsigned long)mkeys[i].spml_context,
                    0);
            if (0 < mkeys[i].len) {
                free(mkeys[i].u.data);
            }
#endif
            break;
        }
    }
    free(mkeys);

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

int mca_spml_ikrit_oob_get_mkeys(int pe, uint32_t seg, sshmem_mkey_t *mkeys)
{
    int ptl;
    ptl = get_ptl_id(pe);
    if (ptl < 0)
        return OSHMEM_ERROR;

    if (ptl != MXM_PTL_RDMA)
        return OSHMEM_ERROR;

#if MXM_API < MXM_VERSION(2,0)
    if (seg > 1)
        return OSHMEM_ERROR;

    mkeys[ptl].len = 0;
    mkeys[ptl].u.key = MAP_SEGMENT_SHM_INVALID;
    return OSHMEM_SUCCESS;
#else
    /* we are actually registering memory in 2.0 and later.
     * So can only skip mkey exchange when ud is the only transport
     */
    if (mca_spml_ikrit.ud_only) {
        mkeys[ptl].len = 0;
        mkeys[ptl].u.key = MAP_SEGMENT_SHM_INVALID;
        return OSHMEM_SUCCESS;
    } 

    return OSHMEM_ERROR;
#endif
}

static int mca_spml_ikrit_get_helper(mxm_send_req_t *sreq,
                                     void *src_addr,
                                     size_t size,
                                     void *dst_addr,
                                     int src)
{
    /* shmem spec states that get() operations are blocking. So it is enough
     to have single mxm request. Also we count on mxm doing copy */
    void *rva;
    sshmem_mkey_t *r_mkey;
    int ptl_id;

    ptl_id = get_ptl_id(src);
    /* already tried to send via shm and failed. go via rdma */
    if (ptl_id == MXM_PTL_SHM)
        ptl_id = MXM_PTL_RDMA;

    /**
     * Get the address to the remote rkey.
     **/
    r_mkey = mca_memheap.memheap_get_cached_mkey(src,
                                                 src_addr,
                                                 ptl_id,
                                                 &rva);
    if (!r_mkey) {
        SPML_ERROR("pe=%d: %p is not address of shared variable",
                   src, src_addr);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

    SPML_VERBOSE(100,
                 "get: pe:%d ptl=%d src=%p -> dst: %p sz=%d. src_rva=%p, %s",
                 src, ptl_id, src_addr, dst_addr, (int)size, (void *)rva, mca_spml_base_mkey2str(r_mkey));

    /* mxm does not really cares for get lkey */
    sreq->base.mq = mca_spml_ikrit.mxm_mq;
    sreq->base.conn = mca_spml_ikrit.mxm_peers[src]->mxm_conn;
    sreq->base.data_type = MXM_REQ_DATA_BUFFER;
    sreq->base.data.buffer.ptr = dst_addr;
    sreq->base.data.buffer.length = size;
#if MXM_API < MXM_VERSION(2,0)
    sreq->base.data.buffer.memh = NULL;
    sreq->op.mem.remote_memh = NULL;
#else
    sreq->op.mem.remote_mkey = to_mxm_mkey(r_mkey);
#endif
    sreq->opcode = MXM_REQ_OP_GET;
    sreq->op.mem.remote_vaddr = (intptr_t) rva;
    sreq->base.state = MXM_REQ_NEW;

    return OSHMEM_SUCCESS;
}

static inline int mca_spml_ikrit_get_shm(void *src_addr,
                                         size_t size,
                                         void *dst_addr,
                                         int src)
{
    int ptl_id;
    void *rva;
    sshmem_mkey_t *r_mkey;

    ptl_id = get_ptl_id(src);
    /**
     * Get the address to the remote rkey.
     **/
    if (ptl_id != MXM_PTL_SHM)
        return OSHMEM_ERROR;

    r_mkey = mca_memheap.memheap_get_cached_mkey(src,
                                                 src_addr,
                                                 ptl_id,
                                                 &rva);
    if (!r_mkey) {
        SPML_ERROR("pe=%d: %p is not address of shared variable",
                   src, src_addr);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

    if (!mca_memheap_base_can_local_copy(r_mkey, src_addr))
        return OSHMEM_ERROR;

    SPML_VERBOSE(100,
                 "shm get: pe:%d src=%p -> dst: %p sz=%d. src_rva=%p, %s",
                 src, src_addr, dst_addr, (int)size, (void *)rva, mca_spml_base_mkey2str(r_mkey));
    memcpy(dst_addr, (void *) (unsigned long) rva, size);
    opal_progress();
    return OSHMEM_SUCCESS;
}

int mca_spml_ikrit_get(void *src_addr, size_t size, void *dst_addr, int src)
{
    mxm_send_req_t sreq;

    if (0 >= size) {
        return OSHMEM_SUCCESS;
    }

    if (OSHMEM_SUCCESS == mca_spml_ikrit_get_shm(src_addr, size, dst_addr, src))
        return OSHMEM_SUCCESS;

    if (OSHMEM_SUCCESS
            != mca_spml_ikrit_get_helper(&sreq,
                                         src_addr,
                                         size,
                                         dst_addr,
                                         src)) {
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

#if MXM_API < MXM_VERSION(2,0)
    sreq.base.flags = MXM_REQ_FLAG_BLOCKING;
#else
    sreq.flags = MXM_REQ_SEND_FLAG_BLOCKING;
#endif
    sreq.base.completed_cb = NULL;

    mxm_req_send(&sreq);
    opal_progress();
    mca_spml_irkit_req_wait(&sreq.base);

    if (MXM_OK != sreq.base.error) {
        SPML_ERROR("get request failed: %s - aborting",
                   mxm_error_string(sreq.base.error));
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

static inline void get_completion_cb(void *ctx)
{
    mca_spml_ikrit_get_request_t *get_req = (mca_spml_ikrit_get_request_t *) ctx;

    OPAL_THREAD_ADD32(&mca_spml_ikrit.n_active_gets, -1);
    get_req->req_get.req_base.req_spml_complete = true;
    get_req->req_get.req_base.req_oshmem.req_status.SHMEM_ERROR =
            OSHMEM_SUCCESS;
    oshmem_request_complete(&get_req->req_get.req_base.req_oshmem, 1);
    oshmem_request_free((oshmem_request_t**) &get_req);
}

/* extension. used 4 fence implementation b4 fence was added to mxm */
int mca_spml_ikrit_get_async(void *src_addr,
                             size_t size,
                             void *dst_addr,
                             int src)
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

    if (OSHMEM_SUCCESS
            != mca_spml_ikrit_get_helper(&get_req->mxm_req,
                                         src_addr,
                                         size,
                                         dst_addr,
                                         src)) {
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

#if MXM_API < MXM_VERSION(2,0)
    get_req->mxm_req.base.flags = 0;
#else
    get_req->mxm_req.flags = 0;
#endif
    get_req->mxm_req.base.completed_cb = get_completion_cb;
    get_req->mxm_req.base.context = get_req;
    OPAL_THREAD_ADD32(&mca_spml_ikrit.n_active_gets, 1);

    mxm_req_send(&get_req->mxm_req);

    if (MXM_OK != get_req->mxm_req.base.error) {
        SPML_ERROR("get request failed: %s - aborting",
                   mxm_error_string(get_req->mxm_req.base.error));
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

static inline void fence_completion_cb(void *ctx)
{
    mca_spml_ikrit_get_request_t *fence_req =
            (mca_spml_ikrit_get_request_t *) ctx;

    OPAL_THREAD_ADD32(&mca_spml_ikrit.n_mxm_fences, -1);
    fence_req->req_get.req_base.req_spml_complete = true;
    fence_req->req_get.req_base.req_oshmem.req_status.SHMEM_ERROR =
            OSHMEM_SUCCESS;
    oshmem_request_complete(&fence_req->req_get.req_base.req_oshmem, 1);
    oshmem_request_free((oshmem_request_t**) &fence_req);
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
#if MXM_API < MXM_VERSION(2,0)
    fence_req->mxm_req.opcode = MXM_REQ_OP_FENCE;
    fence_req->mxm_req.base.flags = MXM_REQ_FLAG_SEND_SYNC;
#else
    fence_req->mxm_req.opcode = MXM_REQ_OP_PUT_SYNC;
    fence_req->mxm_req.flags  = MXM_REQ_SEND_FLAG_FENCE;
    fence_req->mxm_req.op.mem.remote_vaddr = 0;
    fence_req->mxm_req.op.mem.remote_mkey  = &mxm_empty_mem_key;
    fence_req->mxm_req.base.data_type      = MXM_REQ_DATA_BUFFER;
    fence_req->mxm_req.base.data.buffer.ptr    = 0;
    fence_req->mxm_req.base.data.buffer.length = 0;
#endif
    fence_req->mxm_req.base.state = MXM_REQ_NEW;
    fence_req->mxm_req.base.completed_cb = fence_completion_cb;
    fence_req->mxm_req.base.context = fence_req;
    OPAL_THREAD_ADD32(&mca_spml_ikrit.n_mxm_fences, 1);

    mxm_req_send(&fence_req->mxm_req);
    return OSHMEM_SUCCESS;
}

static inline void put_completion_cb(void *ctx)
{
    mca_spml_ikrit_put_request_t *put_req = (mca_spml_ikrit_put_request_t *) ctx;
    mxm_peer_t *peer;

    OPAL_THREAD_ADD32(&mca_spml_ikrit.n_active_puts, -1);
    peer = mca_spml_ikrit.mxm_peers[put_req->pe];

    /* this was last put in progress. Remove peer from the list so that we do not need explicit fence */
#if SPML_IKRIT_PUT_DEBUG == 1
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
#if MXM_API < MXM_VERSION(2,0)
        if (0 == peer->n_active_puts &&
                (put_req->mxm_req.base.flags & MXM_REQ_FLAG_SEND_SYNC)) {
            opal_list_remove_item(&mca_spml_ikrit.active_peers, &peer->super);
            peer->need_fence = 0;
        }
#else
        if (0 == peer->n_active_puts &&
                (put_req->mxm_req.opcode == MXM_REQ_OP_PUT_SYNC)) {
            opal_list_remove_item(&mca_spml_ikrit.active_peers, &peer->super);
            peer->need_fence = 0;
        }
#endif
    }

    put_req->req_put.req_base.req_spml_complete = true;
    put_req->req_put.req_base.req_oshmem.req_status.SHMEM_ERROR =
            OSHMEM_SUCCESS;
    oshmem_request_complete(&put_req->req_put.req_base.req_oshmem, 1);
    oshmem_request_free((oshmem_request_t**) &put_req);
}

/**
 * TODO: using put request as handle is not good.
 */
static inline int mca_spml_ikrit_put_internal(void* dst_addr,
                                              size_t size,
                                              void* src_addr,
                                              int dst,
                                              void **handle,
                                              int zcopy)
{
    void *rva;
    mca_spml_ikrit_put_request_t *put_req;
    int ptl_id;
    sshmem_mkey_t *r_mkey;
    static int count;
    int need_progress = 0;

    if (0 >= size) {
        return OSHMEM_SUCCESS;
    }

    ptl_id = get_ptl_id(dst);
    /* Get rkey of remote PE (dst proc) which must be on memheap  */
    r_mkey = mca_memheap.memheap_get_cached_mkey(dst,
                                                 dst_addr,
                                                 ptl_id,
                                                 &rva);
    if (!r_mkey) {
        SPML_ERROR("pe=%d: %p is not address of shared variable",
                   dst, dst_addr);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

#if SPML_IKRIT_PUT_DEBUG == 1

    SPML_VERBOSE(100, "put: pe:%d ptl=%d dst=%p <- src: %p sz=%d. dst_rva=%p, %s",
            dst, ptl_id, dst_addr, src_addr, (int)size, (void *)rva, mca_spml_base_mkey2str(r_mkey));
#endif
    if (ptl_id == MXM_PTL_SHM) {

        if (mca_memheap_base_can_local_copy(r_mkey, dst_addr)) {
            memcpy((void *) (unsigned long) rva, src_addr, size);
            /* call progress as often as we would have with regular put */
            if (++count % SPML_IKRIT_PACKETS_PER_SYNC == 0)
                mxm_progress(mca_spml_ikrit.mxm_context);
            return OSHMEM_SUCCESS;
        }
        /* segment not mapped - fallback to rmda */
        ptl_id = MXM_PTL_RDMA;
        r_mkey = mca_memheap.memheap_get_cached_mkey(dst,
                                                     dst_addr,
                                                     ptl_id,
                                                     &rva);
        if (!r_mkey) {
            SPML_ERROR("pe=%d: %p is not address of shared variable",
                       dst, dst_addr);
            oshmem_shmem_abort(-1);
            return OSHMEM_ERROR;
        }
    }

#if SPML_IKRIT_PUT_DEBUG == 1
    SPML_VERBOSE(100, "put: pe:%d ptl=%d dst=%p <- src: %p sz=%d. dst_rva=%p, %s",
            dst, ptl_id, dst_addr, src_addr, (int)size, (void *)rva, mca_spml_base_mkey2str(r_mkey));
#endif

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
#if MXM_API < MXM_VERSION(2,0)
    put_req->mxm_req.opcode = MXM_REQ_OP_PUT;
    if (mca_spml_ikrit.free_list_max - mca_spml_ikrit.n_active_puts <= SPML_IKRIT_PUT_LOW_WATER ||
            (mca_spml_ikrit.mxm_peers[dst]->n_active_puts + 1) % SPML_IKRIT_PACKETS_PER_SYNC == 0) {
        put_req->mxm_req.base.flags = MXM_REQ_FLAG_SEND_SYNC;
        need_progress = 1;
    } else  {
        put_req->mxm_req.base.flags = MXM_REQ_FLAG_SEND_LAZY|MXM_REQ_FLAG_SEND_SYNC;
    }
#else
    put_req->mxm_req.flags = 0;
    if (mca_spml_ikrit.free_list_max - mca_spml_ikrit.n_active_puts <= SPML_IKRIT_PUT_LOW_WATER ||
            (int)opal_list_get_size(&mca_spml_ikrit.active_peers) > mca_spml_ikrit.unsync_conn_max ||
            (mca_spml_ikrit.mxm_peers[dst]->n_active_puts + 1) % SPML_IKRIT_PACKETS_PER_SYNC == 0) {
        need_progress = 1;
        put_req->mxm_req.opcode = MXM_REQ_OP_PUT_SYNC;
    } else  {
        put_req->mxm_req.opcode = MXM_REQ_OP_PUT;
    }
    if (!zcopy) {
        put_req->mxm_req.flags |= MXM_REQ_SEND_FLAG_BLOCKING;
    }
#endif

    put_req->mxm_req.base.conn = mca_spml_ikrit.mxm_peers[dst]->mxm_conn;
    put_req->mxm_req.base.data_type = MXM_REQ_DATA_BUFFER;
    put_req->mxm_req.base.data.buffer.ptr = src_addr;
    put_req->mxm_req.base.data.buffer.length = size;
    put_req->mxm_req.base.completed_cb = put_completion_cb;
    put_req->mxm_req.base.context = put_req;
    put_req->mxm_req.op.mem.remote_vaddr = (intptr_t) rva;
    put_req->mxm_req.base.state = MXM_REQ_NEW;
    put_req->pe = dst;

#if MXM_API < MXM_VERSION(2,0)
    put_req->mxm_req.base.data.buffer.memh = NULL;
    put_req->mxm_req.op.mem.remote_memh = NULL;
#else
    put_req->mxm_req.op.mem.remote_mkey = to_mxm_mkey(r_mkey);
#endif

    OPAL_THREAD_ADD32(&mca_spml_ikrit.n_active_puts, 1);
    if (mca_spml_ikrit.mxm_peers[dst]->need_fence == 0) {
        opal_list_append(&mca_spml_ikrit.active_peers,
                         &mca_spml_ikrit.mxm_peers[dst]->super);
        mca_spml_ikrit.mxm_peers[dst]->need_fence = 1;
    }

    mca_spml_ikrit.mxm_peers[dst]->n_active_puts++;

    mxm_req_send(&put_req->mxm_req);

    if (MXM_OK != put_req->mxm_req.base.error) {
        OPAL_THREAD_ADD32(&mca_spml_ikrit.n_active_puts, -1);
        SPML_ERROR("put request %p failed: %s - aborting",
                   (void*)put_req, mxm_error_string(put_req->mxm_req.base.error));
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
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
int mca_spml_ikrit_put_simple(void* dst_addr,
                              size_t size,
                              void* src_addr,
                              int dst)
{
    void *rva;
    mxm_send_req_t mxm_req;
    mxm_wait_t wait;
    int ptl_id;
    sshmem_mkey_t *r_mkey;
    static int count;

    ptl_id = get_ptl_id(dst);
    /* Get rkey of remote PE (dst proc) which must be on memheap  */
    r_mkey = mca_memheap.memheap_get_cached_mkey(dst,
                                                 //(unsigned long) dst_addr,
                                                 dst_addr,
                                                 ptl_id,
                                                 &rva);
    if (!r_mkey) {
        SPML_ERROR("pe=%d: %p is not address of shared variable",
                   dst, dst_addr);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

#if SPML_IKRIT_PUT_DEBUG == 1
    SPML_VERBOSE(100, "put: pe:%d ptl=%d dst=%p <- src: %p sz=%d. dst_rva=%p, %s",
            dst, ptl_id, dst_addr, src_addr, (int)size, (void *)rva, mca_spml_base_mkey2str(r_mkey));
#endif
    if (ptl_id == MXM_PTL_SHM) {

        if (mca_memheap_base_can_local_copy(r_mkey, dst_addr)) {
            memcpy((void *) (unsigned long) rva, src_addr, size);
            /* call progress as often as we would have with regular put */
            if (++count % SPML_IKRIT_PACKETS_PER_SYNC == 0)
                mxm_progress(mca_spml_ikrit.mxm_context);
            return OSHMEM_SUCCESS;
        }
        /* segment not mapped - fallback to rmda */
        ptl_id = MXM_PTL_RDMA;
        r_mkey = mca_memheap.memheap_get_cached_mkey(dst,
                                                     //(unsigned long) dst_addr,
                                                     dst_addr,
                                                     ptl_id,
                                                     &rva);
        if (!r_mkey) {
            SPML_ERROR("pe=%d: %p is not address of shared variable",
                       dst, dst_addr);
            oshmem_shmem_abort(-1);
            return OSHMEM_ERROR;
        }
    }

#if SPML_IKRIT_PUT_DEBUG == 1
    SPML_VERBOSE(100, "put: pe:%d ptl=%d dst=%p <- src: %p sz=%d. dst_rva=%p, %s",
            dst, ptl_id, dst_addr, src_addr, (int)size, (void *)rva, mca_spml_base_mkey2str(r_mkey));
#endif

    /* fill out request */
    mxm_req.base.mq = mca_spml_ikrit.mxm_mq;
#if MXM_API <  MXM_VERSION(2,0)
    mxm_req.base.flags = MXM_REQ_FLAG_BLOCKING;
#else
    mxm_req.flags = MXM_REQ_SEND_FLAG_BLOCKING;
#endif
    mxm_req.base.conn = mca_spml_ikrit.mxm_peers[dst]->mxm_conn;
    mxm_req.base.data_type = MXM_REQ_DATA_BUFFER;
    mxm_req.base.data.buffer.ptr = src_addr;
    mxm_req.base.data.buffer.length = size;
    mxm_req.base.completed_cb = 0;
    mxm_req.base.context = 0;
    mxm_req.opcode = MXM_REQ_OP_PUT;
    mxm_req.op.mem.remote_vaddr = (intptr_t) rva;
    mxm_req.base.state = MXM_REQ_NEW;
    mxm_req.base.error = MXM_OK;

#if MXM_API < MXM_VERSION(2, 0)
    mxm_req.base.data.buffer.memh = NULL;
    mxm_req.op.mem.remote_memh = NULL;
#else
    mxm_req.op.mem.remote_mkey = to_mxm_mkey(r_mkey);
#endif

    if (mca_spml_ikrit.mxm_peers[dst]->need_fence == 0) {
        opal_list_append(&mca_spml_ikrit.active_peers,
                         &mca_spml_ikrit.mxm_peers[dst]->super);
        mca_spml_ikrit.mxm_peers[dst]->need_fence = 1;
    }

    mxm_req_send(&mxm_req);
    if (MXM_OK != mxm_req.base.error) {
        SPML_ERROR("put request failed: %s(%d) - aborting",
                   mxm_error_string(mxm_req.base.error), mxm_req.base.error);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

    wait.req = &mxm_req.base;
    wait.state = (mxm_req_state_t)(MXM_REQ_SENT | MXM_REQ_COMPLETED);
    wait.progress_cb = NULL;
    wait.progress_arg = NULL;
    mxm_wait(&wait);

    return OSHMEM_SUCCESS;
}

int mca_spml_ikrit_put_nb(void* dst_addr,
                          size_t size,
                          void* src_addr,
                          int dst,
                          void **handle)
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
    err = mca_spml_ikrit_put_internal(dst_addr,
                                      size,
                                      src_addr,
                                      dst,
                                      (void **) &put_req,
                                      0);
    if (OSHMEM_SUCCESS != err) {
        SPML_ERROR("put failed - aborting");
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }
    if (!put_req)
        return OSHMEM_SUCCESS;

    wait.req = &put_req->mxm_req.base;
    wait.state = (mxm_req_state_t)(MXM_REQ_SENT | MXM_REQ_COMPLETED);
    wait.progress_cb = NULL;
    wait.progress_arg = NULL;
    mxm_wait(&wait);

    return OSHMEM_SUCCESS;
}


int mca_spml_ikrit_fence(void)
{
    mxm_peer_t *peer;
    opal_list_item_t *item;

    SPML_VERBOSE(20,
                 "Into fence with %d active puts on %d pes",
                 mca_spml_ikrit.n_active_puts, (int)opal_list_get_size(&mca_spml_ikrit.active_peers));

    /* puts(unless are send sync) are completed by remote side lazily. That is either when remote decides to 
     * ack window which can take hundreds of ms. So speed things up by doing fence */
    while (NULL != (item = opal_list_remove_first(&mca_spml_ikrit.active_peers))) {
        peer = (mxm_peer_t *) item;
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
    mxm_error_t ret = MXM_OK;
    mxm_recv_req_t req;
    char dummy_buf[1];

    /* tag mask 0 matches any tag */
    SPML_VERBOSE(100,
                 "want to recv from src %d, size %d buf %p",
                 src, (int)size, buf);
    req.tag = src == SHMEM_ANY_SOURCE ? 0 : src;
    req.tag_mask = src == SHMEM_ANY_SOURCE ? 0 : 0xFFFFFFFF;

    req.base.state = MXM_REQ_NEW;
    req.base.mq = mca_spml_ikrit.mxm_mq;
    req.base.conn = NULL;
#if MXM_API < MXM_VERSION(2,0)
    req.base.flags           = MXM_REQ_FLAG_BLOCKING;
#endif
    req.base.completed_cb = NULL;

    req.base.data_type = MXM_REQ_DATA_BUFFER;
    req.base.data.buffer.ptr = buf == NULL ? dummy_buf : buf;
    req.base.data.buffer.length = size == 0 ? sizeof(dummy_buf) : size;
    req.base.data.buffer.memh = NULL;

    ret = mxm_req_recv(&req);
    if (MXM_OK != ret) {
        return OSHMEM_ERROR;
    }
    mca_spml_irkit_req_wait(&req.base);
    if (MXM_OK != req.base.error) {
        return OSHMEM_ERROR;
    }
    SPML_VERBOSE(100,
                 "recvd from tag %d len %d",
                 req.completion.sender_tag, (int)req.completion.actual_len);

    return OSHMEM_SUCCESS;
}

/* for now only do blocking copy send */
int mca_spml_ikrit_send(void* buf,
                        size_t size,
                        int dst,
                        mca_spml_base_put_mode_t mode)
{
    mxm_send_req_t req;
    char dummy_buf[1];

    SPML_VERBOSE(100,
                 "sending %p size %d to %d, mode %d",
                 buf, (int)size, dst, (int)mode);
    req.opcode = MXM_REQ_OP_SEND;

    req.op.send.tag = oshmem_my_proc_id();

    req.base.state = MXM_REQ_NEW;
    req.base.mq = mca_spml_ikrit.mxm_mq;
    req.base.conn = mca_spml_ikrit.mxm_peers[dst]->mxm_conn;
#if MXM_API < MXM_VERSION(2,0)
    req.base.flags           = MXM_REQ_FLAG_BLOCKING;
#else
    req.flags                = MXM_REQ_SEND_FLAG_BLOCKING;
#endif
    req.base.completed_cb = NULL;

    req.base.data_type = MXM_REQ_DATA_BUFFER;
    req.base.data.buffer.ptr = buf == NULL ? dummy_buf : buf;
    req.base.data.buffer.length = size == 0 ? sizeof(dummy_buf) : size;
    req.base.data.buffer.memh = NULL;

    mxm_req_send(&req);
    mca_spml_irkit_req_wait(&req.base);
    if (req.base.error != MXM_OK) {
        return OSHMEM_ERROR;
    }

    return OSHMEM_SUCCESS;
}
