/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
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
#include "opal/mca/memchecker/base/base.h"
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
#include "oshmem/mca/sshmem/sshmem.h"

#include "oshmem/mca/spml/ikrit/spml_ikrit_component.h"

/* Turn ON/OFF debug output from build (default 0) */
#ifndef SPML_IKRIT_PUT_DEBUG
#define SPML_IKRIT_PUT_DEBUG    0
#endif

#define SPML_IKRIT_MXM_POST_SEND(sreq) \
do { \
    mxm_error_t err; \
    err = mxm_req_send(&sreq); \
    if (MXM_OK != err) { \
        SPML_ERROR("mxm_req_send (op=%d) failed: %s - aborting", \
                   sreq.opcode, \
                   mxm_error_string(err)); \
        oshmem_shmem_abort(-1); \
        return OSHMEM_ERROR; \
    } \
} while(0)

static int mca_spml_ikrit_get_async(void *src_addr,
                                    size_t size,
                                    void *dst_addr,
                                    int src);

mca_spml_ikrit_ctx_t mca_spml_ikrit_ctx_default = { 0 };

struct mca_spml_ikrit_put_request {
    opal_free_list_item_t   link;   /* must be a first member */
    mxm_send_req_t          mxm_req;
    int                     pe;
};

typedef struct mca_spml_ikrit_put_request mca_spml_ikrit_put_request_t;


static inline int get_ptl_id(int dst)
{
    return mca_spml_ikrit.mxm_peers[dst].ptl_id;
}

static inline mxm_mem_key_t *to_mxm_mkey(sshmem_mkey_t *mkey) {

    if (0 == mkey->len) {
        return &mxm_empty_mem_key;
    }
    return (mxm_mem_key_t *)mkey->u.data;
}

static inline void mca_spml_irkit_req_wait(mxm_req_base_t *req)
{
    do {
        /* do at least one progress since
         * with some TLs (self, shm) request
         * can be completed immediately
         */
        opal_progress();
    } while (!mxm_req_test(req));
}

static inline void free_put_req(mca_spml_ikrit_put_request_t *put_req)
{
    opal_free_list_return (&mca_spml_base_put_requests,
                           (opal_free_list_item_t*)put_req);
    opal_memchecker_base_mem_noaccess(put_req, sizeof(*put_req));
}

static inline mca_spml_ikrit_put_request_t *alloc_put_req(void)
{
    mca_spml_ikrit_put_request_t *req;
    opal_free_list_item_t* item;

    item = opal_free_list_wait (&mca_spml_base_put_requests);
    assert(item != NULL);

    req = (mca_spml_ikrit_put_request_t *) item;
    opal_memchecker_base_mem_undefined(req, sizeof(*req));

    return req;
}


struct mca_spml_ikrit_get_request {
    opal_free_list_item_t   link;   /* must be a first member */
    mxm_send_req_t          mxm_req;
};

typedef struct mca_spml_ikrit_get_request mca_spml_ikrit_get_request_t;

static inline void free_get_req(mca_spml_ikrit_get_request_t *get_req)
{
    opal_free_list_return (&mca_spml_base_get_requests,
                           (opal_free_list_item_t*)get_req);
    opal_memchecker_base_mem_noaccess(get_req, sizeof(*get_req));
}

static inline mca_spml_ikrit_get_request_t *alloc_get_req(void)
{
    mca_spml_ikrit_get_request_t *req;
    opal_free_list_item_t* item;

    item = opal_free_list_wait (&mca_spml_base_get_requests);
    assert(item != NULL);

    req = (mca_spml_ikrit_get_request_t *) item;
    opal_memchecker_base_mem_undefined(req, sizeof(*req));
    return req;
}


int mca_spml_ikrit_put_simple(void* dst_addr,
                              size_t size,
                              void* src_addr,
                              int dst);

static void mca_spml_ikrit_cache_mkeys(shmem_ctx_t ctx, sshmem_mkey_t *,
                                       uint32_t seg, int remote_pe, int tr_id);

static mxm_mem_key_t *mca_spml_ikrit_get_mkey_slow(int pe, void *va, int ptl_id, void **rva);

mca_spml_ikrit_t mca_spml_ikrit = {
    .super = {
        /* Init mca_spml_base_module_t */
        .spml_add_procs     = mca_spml_ikrit_add_procs,
        .spml_del_procs     = mca_spml_ikrit_del_procs,
        .spml_enable        = mca_spml_ikrit_enable,
        .spml_register      = mca_spml_ikrit_register,
        .spml_deregister    = mca_spml_ikrit_deregister,
        .spml_oob_get_mkeys = mca_spml_ikrit_oob_get_mkeys,
        .spml_ctx_create    = mca_spml_ikrit_ctx_create,
        .spml_ctx_destroy   = mca_spml_ikrit_ctx_destroy,
        .spml_put           = mca_spml_ikrit_put,
        .spml_put_nb        = mca_spml_ikrit_put_nb,
        .spml_get           = mca_spml_ikrit_get,
        .spml_get_nb        = mca_spml_ikrit_get_nb,
        .spml_recv          = mca_spml_ikrit_recv,
        .spml_send          = mca_spml_ikrit_send,
        .spml_wait          = mca_spml_base_wait,
        .spml_wait_nb       = mca_spml_base_wait_nb,
        .spml_test          = mca_spml_base_test,
        .spml_fence         = mca_spml_ikrit_fence, /* fence is implemented as quiet */
        .spml_quiet         = mca_spml_ikrit_fence,
        .spml_rmkey_unpack  = mca_spml_ikrit_cache_mkeys,
        .spml_rmkey_free    = mca_spml_base_rmkey_free,
        .spml_rmkey_ptr     = mca_spml_base_rmkey_ptr,
        .spml_memuse_hook   = mca_spml_base_memuse_hook,
        .spml_put_all_nb    = mca_spml_base_put_all_nb,

        .self               = (void*)&mca_spml_ikrit
    },
    .get_mkey_slow          = mca_spml_ikrit_get_mkey_slow
};

static void mca_spml_ikrit_cache_mkeys(shmem_ctx_t ctx, sshmem_mkey_t *mkey,
                                       uint32_t seg, int dst_pe, int tr_id)
{
    mxm_peer_t *peer;

    if (MXM_PTL_RDMA != tr_id) {
        return;
    }

    peer = &mca_spml_ikrit.mxm_peers[dst_pe];
    mkey_segment_init(&peer->mkeys[seg].super, mkey, seg);

    if (0 != mkey->len) {
        memcpy(&peer->mkeys[seg].key, mkey->u.data, mkey->len);
    } else {
        memcpy(&peer->mkeys[seg].key, &mxm_empty_mem_key, sizeof(mxm_empty_mem_key));
    }
}

static
mxm_mem_key_t *mca_spml_ikrit_get_mkey_slow(int pe, void *va, int ptl_id, void **rva)
{
    sshmem_mkey_t *mkey;

retry:
    mkey = mca_memheap_base_get_cached_mkey(oshmem_ctx_default, pe, va, ptl_id, rva);
    if (NULL == mkey) {
        SPML_ERROR("pe=%d: %p is not address of shared variable", pe, va);
        oshmem_shmem_abort(-1);
        return NULL;
    }

    if (MXM_PTL_SHM == ptl_id) {
        if (mca_memheap_base_can_local_copy(mkey, va)) {
            return NULL;
        }

        /* if dst addr is on memheap and local copy is not allowed 
         * disable direct shm transport
         */ 
        if (memheap_is_va_in_segment(va, HEAP_SEG_INDEX)) {
            mca_spml_ikrit.mxm_peers[pe].ptl_id = MXM_PTL_RDMA;
        }
        /* going via mxm must always work */
        ptl_id = MXM_PTL_RDMA;
        goto retry;
    }

    return to_mxm_mkey(mkey);
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
                         OBJ_CLASS(opal_free_list_item_t),
                         0,
                         opal_cache_line_size,
                         mca_spml_ikrit.free_list_num,
                         mca_spml_ikrit.free_list_max,
                         mca_spml_ikrit.free_list_inc,
                         NULL, 0, NULL, NULL, NULL);

    opal_free_list_init (&mca_spml_base_get_requests,
                         sizeof(mca_spml_ikrit_get_request_t),
                         opal_cache_line_size,
                         OBJ_CLASS(opal_free_list_item_t),
                         0,
                         opal_cache_line_size,
                         mca_spml_ikrit.free_list_num,
                         mca_spml_ikrit.free_list_max,
                         mca_spml_ikrit.free_list_inc,
                         NULL, 0, NULL, NULL, NULL);

    mca_spml_ikrit.enabled = true;

    return OSHMEM_SUCCESS;
}

static void mxm_peer_construct(mxm_peer_t *p)
{
    p->n_active_puts = 0;
    p->need_fence    = 0;
    p->ptl_id        = MXM_PTL_RDMA;
    OBJ_CONSTRUCT(&p->link, opal_list_item_t);
}

static void mxm_peer_destruct(mxm_peer_t *p)
{
    OBJ_DESTRUCT(&p->link);
}

int mca_spml_ikrit_del_procs(ompi_proc_t** procs, size_t nprocs)
{
    size_t i, n;
    int my_rank = oshmem_my_proc_id();

    oshmem_shmem_barrier();
    if (mca_spml_ikrit.bulk_disconnect) {
        mxm_ep_powerdown(mca_spml_ikrit.mxm_ep);
    }

    while (NULL != opal_list_remove_first(&mca_spml_ikrit.active_peers)) {
    };
    OBJ_DESTRUCT(&mca_spml_ikrit.active_peers);

    for (n = 0; n < nprocs; n++) {
        i = (my_rank + n) % nprocs;
        mxm_ep_disconnect(mca_spml_ikrit.mxm_peers[i].mxm_conn);
        if (mca_spml_ikrit.hw_rdma_channel) {
            assert(mca_spml_ikrit.mxm_peers[i].mxm_hw_rdma_conn != mca_spml_ikrit.mxm_peers[i].mxm_conn);
            mxm_ep_disconnect(mca_spml_ikrit.mxm_peers[i].mxm_hw_rdma_conn);
        }
        mxm_peer_destruct(&mca_spml_ikrit.mxm_peers[i]);
    }
    free(mca_spml_ikrit.mxm_peers);

    return OSHMEM_SUCCESS;
}

int mca_spml_ikrit_add_procs(ompi_proc_t** procs, size_t nprocs)
{
    spml_ikrit_mxm_ep_conn_info_t *ep_info = NULL;
    spml_ikrit_mxm_ep_conn_info_t *ep_hw_rdma_info = NULL;
    spml_ikrit_mxm_ep_conn_info_t my_ep_info;
    size_t mxm_addr_len = MXM_MAX_ADDR_LEN;
    mxm_error_t err;
    size_t i, n;
    int rc = OSHMEM_ERROR;
    ompi_proc_t *proc_self;
    int my_rank = oshmem_my_proc_id();

    OBJ_CONSTRUCT(&mca_spml_ikrit.active_peers, opal_list_t);
    /* Allocate connection requests */
    ep_info = calloc(sizeof(spml_ikrit_mxm_ep_conn_info_t), nprocs);
    if (NULL == ep_info) {
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto bail;
    }

    if (mca_spml_ikrit.hw_rdma_channel) {
        ep_hw_rdma_info = calloc(sizeof(spml_ikrit_mxm_ep_conn_info_t), nprocs);
        if (NULL == ep_hw_rdma_info) {
            rc = OSHMEM_ERR_OUT_OF_RESOURCE;
            goto bail;
        }
    }

    mca_spml_ikrit.mxm_peers = (mxm_peer_t *) calloc(nprocs , sizeof(mxm_peer_t));
    if (NULL == mca_spml_ikrit.mxm_peers) {
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto bail;
    }

    memset(&my_ep_info, 0, sizeof(my_ep_info));

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

    oshmem_shmem_allgather(&my_ep_info, ep_info,
                           sizeof(spml_ikrit_mxm_ep_conn_info_t));

    opal_progress_register(spml_ikrit_progress);

    /* Get the EP connection requests for all the processes from modex */
    for (n = 0; n < nprocs; ++n) {

        /* mxm 2.0 keeps its connections on a list. Make sure
         * that list have different order on every rank */
        i = (my_rank + n) % nprocs;
        mxm_peer_construct(&mca_spml_ikrit.mxm_peers[i]);

        err = mxm_ep_connect(mca_spml_ikrit.mxm_ep, ep_info[i].addr.ep_addr, &mca_spml_ikrit.mxm_peers[i].mxm_conn);
        if (MXM_OK != err) {
            SPML_ERROR("MXM returned connect error: %s\n", mxm_error_string(err));
            goto bail;
        }
        mxm_conn_ctx_set(mca_spml_ikrit.mxm_peers[i].mxm_conn, &mca_spml_ikrit.mxm_peers[i]);
        if (mca_spml_ikrit.hw_rdma_channel) {
            err = mxm_ep_connect(mca_spml_ikrit.mxm_hw_rdma_ep, ep_hw_rdma_info[i].addr.ep_addr, &mca_spml_ikrit.mxm_peers[i].mxm_hw_rdma_conn);
            if (MXM_OK != err) {
                SPML_ERROR("MXM returned connect error: %s\n", mxm_error_string(err));
                goto bail;
            }
        } else {
            mca_spml_ikrit.mxm_peers[i].mxm_hw_rdma_conn = mca_spml_ikrit.mxm_peers[i].mxm_conn;
        }
    }

    if (ep_info)
        free(ep_info);
    if (ep_hw_rdma_info)
        free(ep_hw_rdma_info);

    if (mca_spml_ikrit.bulk_connect) {
        /* Need a barrier to ensure remote peers already created connection */
        oshmem_shmem_barrier();
        mxm_ep_wireup(mca_spml_ikrit.mxm_ep);
    }

    proc_self = oshmem_proc_group_find(oshmem_group_all, my_rank);
    /* identify local processes and change transport to SHM */
    for (i = 0; i < nprocs; i++) {
        if (procs[i]->super.proc_name.jobid != proc_self->super.proc_name.jobid ||
            !OPAL_PROC_ON_LOCAL_NODE(procs[i]->super.proc_flags)) {
            continue;
        }
        if (procs[i] == proc_self)
            continue;

        /* use zcopy for put/get via sysv shared memory with fallback to RDMA */
        mca_spml_ikrit.mxm_peers[i].ptl_id = MXM_PTL_SHM;
    }

    SPML_VERBOSE(50, "*** ADDED PROCS ***");
    return OSHMEM_SUCCESS;

bail:
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
    mxm_error_t err;
    mxm_mem_key_t *m_key;
    int my_rank = oshmem_my_proc_id();

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
        case MXM_PTL_RDMA:
            mkeys[i].va_base = addr;
            mkeys[i].spml_context = 0;

            if (mca_spml_ikrit.ud_only) {
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
            break;

        default:
            SPML_ERROR("unsupported PTL: %d", i);
            goto error_out;
        }
        SPML_VERBOSE(5,
                     "rank %d ptl %d addr %p size %llu %s",
                     my_rank, i, addr, (unsigned long long)size,
                     mca_spml_base_mkey2str(&mkeys[i]));

        mca_spml_ikrit_cache_mkeys(oshmem_ctx_default, &mkeys[i],
                                   memheap_find_segnum(addr), my_rank, i);
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

    MCA_SPML_CALL(fence(oshmem_ctx_default));
    if (!mkeys)
        return OSHMEM_SUCCESS;

    for (i = 0; i < MXM_PTL_LAST; i++) {
        switch (i) {
        case MXM_PTL_SHM:
            break;
        case MXM_PTL_RDMA:
            /* dereg memory */
            if (!mkeys[i].spml_context)
                break;
            mxm_mem_unmap(mca_spml_ikrit.mxm_context,
                    (void *)mkeys[i].va_base,
                    (unsigned long)mkeys[i].spml_context,
                    0);
            if (0 < mkeys[i].len) {
                free(mkeys[i].u.data);
            }
            break;
        }
    }
    free(mkeys);

    return OSHMEM_SUCCESS;

}

int mca_spml_ikrit_oob_get_mkeys(shmem_ctx_t ctx, int pe, uint32_t seg,
                                 sshmem_mkey_t *mkeys)
{
    int ptl;

    ptl = get_ptl_id(pe);
    if (ptl < 0)
        return OSHMEM_ERROR;

    if (ptl != MXM_PTL_RDMA)
        return OSHMEM_ERROR;

    /* we are actually registering memory in 2.0 and later.
     * So can only skip mkey exchange when ud is the only transport
     */
    if (mca_spml_ikrit.ud_only) {
        /* assumes that remote has the same va_base as we do */
        mkeys[ptl].len     = 0;
        mkeys[ptl].va_base = mca_memheap_seg2base_va(seg);
        mkeys[ptl].u.key   = MAP_SEGMENT_SHM_INVALID;
        mca_spml_ikrit_cache_mkeys(ctx, &mkeys[ptl], seg, pe, ptl);
        return OSHMEM_SUCCESS;
    }

    return OSHMEM_ERROR;
}

int mca_spml_ikrit_ctx_create(long options, shmem_ctx_t *ctx)
{
    int rc = OSHMEM_SUCCESS;
    mca_spml_ikrit_ctx_t *ctxp = malloc(sizeof(mca_spml_ikrit_ctx_t));
    *ctx = (shmem_ctx_t)ctxp;
    return rc;
}

void mca_spml_ikrit_ctx_destroy(shmem_ctx_t ctx)
{
    free(ctx);
}

static inline int mca_spml_ikrit_get_helper(mxm_send_req_t *sreq,
                                            void *src_addr,
                                            size_t size,
                                            void *dst_addr,
                                            int src)
{
    /* shmem spec states that get() operations are blocking. So it is enough
     to have single mxm request. Also we count on mxm doing copy */
    void *rva;
    mxm_mem_key_t *mkey;

    mkey = mca_spml_ikrit_get_mkey(src, src_addr, MXM_PTL_RDMA, &rva, &mca_spml_ikrit);

    SPML_VERBOSE_FASTPATH(100,
                          "get: pe:%d ptl=%d src=%p -> dst: %p sz=%d. src_rva=%p",
                          src, MXM_PTL_RDMA, src_addr, dst_addr, (int)size, (void *)rva);

    /* mxm does not really cares for get lkey */
    sreq->base.mq = mca_spml_ikrit.mxm_mq;
    sreq->base.conn = mca_spml_ikrit.mxm_peers[src].mxm_conn;
    sreq->base.data_type = MXM_REQ_DATA_BUFFER;
    sreq->base.data.buffer.ptr = dst_addr;
    sreq->base.data.buffer.length = size;
    sreq->op.mem.remote_mkey = mkey; 
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

    ptl_id = get_ptl_id(src);
    /**
     * Get the address to the remote rkey.
     **/
    if (ptl_id != MXM_PTL_SHM)
        return OSHMEM_ERROR;

    if (NULL != mca_spml_ikrit_get_mkey(src, src_addr, MXM_PTL_SHM, &rva, &mca_spml_ikrit))
        return OSHMEM_ERROR;

    SPML_VERBOSE_FASTPATH(100,
                          "shm get: pe:%d src=%p -> dst: %p sz=%d. src_rva=%p",
                          src, src_addr, dst_addr, (int)size, (void *)rva);

    memcpy(dst_addr, (void *) (unsigned long) rva, size);
    opal_progress();
    return OSHMEM_SUCCESS;
}

int mca_spml_ikrit_get_nb(shmem_ctx_t ctx,
                          void* src_addr,
                          size_t size,
                          void* dst_addr,
                          int src,
                          void **handle)
{
    return mca_spml_ikrit_get_async(src_addr, size, dst_addr, src);
}

int mca_spml_ikrit_get(shmem_ctx_t ctx, void *src_addr, size_t size, void *dst_addr, int src)
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

    sreq.base.completed_cb = NULL;
    sreq.flags = 0;

    SPML_IKRIT_MXM_POST_SEND(sreq);

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

    OPAL_THREAD_ADD_FETCH32(&mca_spml_ikrit.n_active_gets, -1);
    free_get_req(get_req);
}

static inline int mca_spml_ikrit_get_async(void *src_addr,
                                           size_t size,
                                           void *dst_addr,
                                           int src)
{
    mca_spml_ikrit_get_request_t *get_req;

    if (OSHMEM_SUCCESS == mca_spml_ikrit_get_shm(src_addr, size, dst_addr, src))
        return OSHMEM_SUCCESS;

    get_req = alloc_get_req();

    if (OSHMEM_SUCCESS != mca_spml_ikrit_get_helper(&get_req->mxm_req,
                                                    src_addr,
                                                    size,
                                                    dst_addr,
                                                    src)) {
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

    get_req->mxm_req.flags = 0;
    get_req->mxm_req.base.completed_cb = get_completion_cb;
    get_req->mxm_req.base.context = get_req;
    OPAL_THREAD_ADD_FETCH32(&mca_spml_ikrit.n_active_gets, 1);

    SPML_IKRIT_MXM_POST_SEND(get_req->mxm_req);

    return OSHMEM_SUCCESS;
}

static inline void fence_completion_cb(void *ctx)
{
    mca_spml_ikrit_get_request_t *fence_req =
            (mca_spml_ikrit_get_request_t *) ctx;

    OPAL_THREAD_ADD_FETCH32(&mca_spml_ikrit.n_mxm_fences, -1);
    free_get_req(fence_req);
}

static int mca_spml_ikrit_mxm_fence(int dst)
{
    mca_spml_ikrit_get_request_t *fence_req;

    fence_req = alloc_get_req();

    fence_req->mxm_req.base.mq = mca_spml_ikrit.mxm_mq;
    fence_req->mxm_req.base.conn = mca_spml_ikrit.mxm_peers[dst].mxm_conn;
    fence_req->mxm_req.opcode = MXM_REQ_OP_PUT_SYNC;
    fence_req->mxm_req.flags  = MXM_REQ_SEND_FLAG_FENCE;
    fence_req->mxm_req.op.mem.remote_vaddr = 0;
    fence_req->mxm_req.op.mem.remote_mkey  = &mxm_empty_mem_key;
    fence_req->mxm_req.base.data_type      = MXM_REQ_DATA_BUFFER;
    fence_req->mxm_req.base.data.buffer.ptr    = 0;
    fence_req->mxm_req.base.data.buffer.length = 0;
    fence_req->mxm_req.base.state = MXM_REQ_NEW;
    fence_req->mxm_req.base.completed_cb = fence_completion_cb;
    fence_req->mxm_req.base.context = fence_req;
    OPAL_THREAD_ADD_FETCH32(&mca_spml_ikrit.n_mxm_fences, 1);

    SPML_IKRIT_MXM_POST_SEND(fence_req->mxm_req);
    return OSHMEM_SUCCESS;
}

static inline void put_completion_cb(void *ctx)
{
    mca_spml_ikrit_put_request_t *put_req = (mca_spml_ikrit_put_request_t *) ctx;
    mxm_peer_t *peer;

    OPAL_THREAD_ADD_FETCH32(&mca_spml_ikrit.n_active_puts, -1);
    /* TODO: keep pointer to peer in the request */
    peer = &mca_spml_ikrit.mxm_peers[put_req->pe];

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
        if (0 == peer->n_active_puts &&
                (put_req->mxm_req.opcode == MXM_REQ_OP_PUT_SYNC)) {
            opal_list_remove_item(&mca_spml_ikrit.active_peers, &peer->link);
            peer->need_fence = 0;
        }
    }

    free_put_req(put_req);
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
    static int count;
    int need_progress = 0;
    mxm_mem_key_t *mkey;

    if (OPAL_UNLIKELY(0 >= size)) {
        return OSHMEM_SUCCESS;
    }

    ptl_id = get_ptl_id(dst);
    mkey = mca_spml_ikrit_get_mkey(dst, dst_addr, ptl_id, &rva, &mca_spml_ikrit);

    if (OPAL_UNLIKELY(NULL == mkey)) {
        memcpy((void *) (unsigned long) rva, src_addr, size);
        /* call progress as often as we would have with regular put */
        if (++count % SPML_IKRIT_PACKETS_PER_SYNC == 0)
            mxm_progress(mca_spml_ikrit.mxm_context);
        return OSHMEM_SUCCESS;
    }

    SPML_VERBOSE_FASTPATH(100, "put: pe:%d ptl=%d dst=%p <- src: %p sz=%d. dst_rva=%p, %s",
                          dst, ptl_id, dst_addr, src_addr, (int)size, (void *)rva);

    put_req = alloc_put_req();

    if (handle)
        *handle = put_req;

    /* fill out request */
    put_req->mxm_req.base.mq = mca_spml_ikrit.mxm_mq;
    /* request immediate responce if we are getting low on send buffers. We only get responce from remote on ack timeout.
     * Also request explicit ack once in a while  */
    put_req->mxm_req.flags = 0;
    if (mca_spml_ikrit.free_list_max - mca_spml_ikrit.n_active_puts <= SPML_IKRIT_PUT_LOW_WATER ||
            (int)opal_list_get_size(&mca_spml_ikrit.active_peers) > mca_spml_ikrit.unsync_conn_max ||
            (mca_spml_ikrit.mxm_peers[dst].n_active_puts + 1) % SPML_IKRIT_PACKETS_PER_SYNC == 0) {
        need_progress = 1;
        put_req->mxm_req.opcode = MXM_REQ_OP_PUT_SYNC;
    } else  {
        put_req->mxm_req.opcode = MXM_REQ_OP_PUT;
    }
    if (!zcopy) {
        if (size < mca_spml_ikrit.put_zcopy_threshold) {
            put_req->mxm_req.flags |= MXM_REQ_SEND_FLAG_BLOCKING;
        } else {
            put_req->mxm_req.opcode = MXM_REQ_OP_PUT_SYNC;
        }
    }

    put_req->mxm_req.base.conn = mca_spml_ikrit.mxm_peers[dst].mxm_conn;
    put_req->mxm_req.base.data_type = MXM_REQ_DATA_BUFFER;
    put_req->mxm_req.base.data.buffer.ptr = src_addr;
    put_req->mxm_req.base.data.buffer.length = size;
    put_req->mxm_req.base.completed_cb = put_completion_cb;
    put_req->mxm_req.base.context = put_req;
    put_req->mxm_req.op.mem.remote_vaddr = (intptr_t) rva;
    put_req->mxm_req.base.state = MXM_REQ_NEW;
    put_req->pe = dst;

    put_req->mxm_req.op.mem.remote_mkey = mkey; 

    OPAL_THREAD_ADD_FETCH32(&mca_spml_ikrit.n_active_puts, 1);
    if (mca_spml_ikrit.mxm_peers[dst].need_fence == 0) {
        opal_list_append(&mca_spml_ikrit.active_peers,
                         &mca_spml_ikrit.mxm_peers[dst].link);
        mca_spml_ikrit.mxm_peers[dst].need_fence = 1;
    }

    mca_spml_ikrit.mxm_peers[dst].n_active_puts++;

    SPML_IKRIT_MXM_POST_SEND(put_req->mxm_req);

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
    mxm_mem_key_t *mkey;
    static int count;

    ptl_id = get_ptl_id(dst);
    mkey = mca_spml_ikrit_get_mkey(dst, dst_addr, ptl_id, &rva, &mca_spml_ikrit);

    SPML_VERBOSE_FASTPATH(100, "put: pe:%d ptl=%d dst=%p <- src: %p sz=%d. dst_rva=%p, %s",
                          dst, ptl_id, dst_addr, src_addr, (int)size, (void *)rva);

    if (NULL == mkey) {
        memcpy((void *) (unsigned long) rva, src_addr, size);
        /* call progress as often as we would have with regular put */
        if (++count % SPML_IKRIT_PACKETS_PER_SYNC == 0)
            mxm_progress(mca_spml_ikrit.mxm_context);
        return OSHMEM_SUCCESS;
    }

    SPML_VERBOSE_FASTPATH(100, "put: pe:%d ptl=%d dst=%p <- src: %p sz=%d. dst_rva=%p, %s",
                          dst, MXM_PTL_RDMA, dst_addr, src_addr, (int)size, (void *)rva);

    /* fill out request */
    mxm_req.base.mq = mca_spml_ikrit.mxm_mq;
    mxm_req.flags = MXM_REQ_SEND_FLAG_BLOCKING;
    mxm_req.base.conn = mca_spml_ikrit.mxm_peers[dst].mxm_conn;
    mxm_req.base.data_type = MXM_REQ_DATA_BUFFER;
    mxm_req.base.data.buffer.ptr = src_addr;
    mxm_req.base.data.buffer.length = size;
    mxm_req.base.completed_cb = 0;
    mxm_req.base.context = 0;
    mxm_req.opcode = MXM_REQ_OP_PUT;
    mxm_req.op.mem.remote_vaddr = (intptr_t) rva;
    mxm_req.base.state = MXM_REQ_NEW;
    mxm_req.base.error = MXM_OK;

    mxm_req.op.mem.remote_mkey = mkey;

    if (mca_spml_ikrit.mxm_peers[dst].need_fence == 0) {
        opal_list_append(&mca_spml_ikrit.active_peers,
                         &mca_spml_ikrit.mxm_peers[dst].link);
        mca_spml_ikrit.mxm_peers[dst].need_fence = 1;
    }

    SPML_IKRIT_MXM_POST_SEND(mxm_req);

    wait.req = &mxm_req.base;
    wait.state = (mxm_req_state_t)(MXM_REQ_SENT | MXM_REQ_COMPLETED);
    wait.progress_cb = NULL;
    wait.progress_arg = NULL;
    mxm_wait(&wait);

    return OSHMEM_SUCCESS;
}

int mca_spml_ikrit_put_nb(shmem_ctx_t ctx,
                          void* dst_addr,
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

int mca_spml_ikrit_put(shmem_ctx_t ctx, void* dst_addr, size_t size, void* src_addr, int dst)
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


int mca_spml_ikrit_fence(shmem_ctx_t ctx)
{
    mxm_peer_t *peer;
    opal_list_item_t *item;

    SPML_VERBOSE(20,
                 "Into fence with %d active puts on %d pes",
                 mca_spml_ikrit.n_active_puts, (int)opal_list_get_size(&mca_spml_ikrit.active_peers));

    /* puts(unless are send sync) are completed by remote side lazily. That is either when remote decides to
     * ack window which can take hundreds of ms. So speed things up by doing fence */
    while (NULL != (item = opal_list_remove_first(&mca_spml_ikrit.active_peers))) {
        peer = spml_ikrit_container_of(item, mxm_peer_t, link);
        peer->n_active_puts = 0;
        peer->need_fence = 0;
        mca_spml_ikrit_mxm_fence(peer - mca_spml_ikrit.mxm_peers);
    }

    while (0 < mca_spml_ikrit.n_mxm_fences || 0 < mca_spml_ikrit.n_active_gets) {
        opal_progress();
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
    req.base.conn = mca_spml_ikrit.mxm_peers[dst].mxm_conn;
    req.flags             = MXM_REQ_SEND_FLAG_BLOCKING;
    req.base.completed_cb = NULL;

    req.base.data_type = MXM_REQ_DATA_BUFFER;
    req.base.data.buffer.ptr = buf == NULL ? dummy_buf : buf;
    req.base.data.buffer.length = size == 0 ? sizeof(dummy_buf) : size;
    req.base.data.buffer.memh = NULL;

    SPML_IKRIT_MXM_POST_SEND(req);

    mca_spml_irkit_req_wait(&req.base);
    if (req.base.error != MXM_OK) {
        return OSHMEM_ERROR;
    }

    return OSHMEM_SUCCESS;
}
