/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      ARM, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 *  @file 
 */

#ifndef MCA_SPML_UCX_H
#define MCA_SPML_UCX_H

#include "oshmem_config.h"
#include "oshmem/request/request.h"
#include "oshmem/mca/spml/base/base.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/util/oshmem_util.h"
#include "oshmem/mca/spml/base/spml_base_putreq.h"
#include "oshmem/proc/proc.h"
#include "oshmem/mca/spml/base/spml_base_request.h"
#include "oshmem/mca/spml/base/spml_base_getreq.h"
#include "oshmem/runtime/runtime.h"

#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"

#include "opal/class/opal_free_list.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_bitmap.h"

#include "orte/runtime/orte_globals.h"
#include "opal/mca/common/ucx/common_ucx.h"

#include <ucp/api/ucp.h>

BEGIN_C_DECLS

#define SPML_UCX_ASSERT  MCA_COMMON_UCX_ASSERT
#define SPML_UCX_ERROR   MCA_COMMON_UCX_ERROR
#define SPML_UCX_VERBOSE MCA_COMMON_UCX_VERBOSE

/**
 * UCX SPML module
 */
struct spml_ucx_mkey {
    ucp_rkey_h rkey;
    ucp_mem_h  mem_h;
}; 
typedef struct spml_ucx_mkey spml_ucx_mkey_t;

struct spml_ucx_cached_mkey {
    mkey_segment_t   super;
    spml_ucx_mkey_t  key;
};
typedef struct spml_ucx_cached_mkey spml_ucx_cached_mkey_t;

struct ucp_peer {
    ucp_ep_h                 ucp_conn;
    spml_ucx_cached_mkey_t   mkeys[MCA_MEMHEAP_MAX_SEGMENTS];
};
typedef struct ucp_peer ucp_peer_t;
 
struct mca_spml_ucx_ctx {
    ucp_worker_h             ucp_worker;
    ucp_peer_t              *ucp_peers;
    long                     options;
    opal_bitmap_t            put_op_bitmap;
    int                     *put_proc_indexes;
    unsigned                 put_proc_count;
};
typedef struct mca_spml_ucx_ctx mca_spml_ucx_ctx_t;

extern mca_spml_ucx_ctx_t mca_spml_ucx_ctx_default;

typedef spml_ucx_mkey_t * (*mca_spml_ucx_get_mkey_slow_fn_t)(shmem_ctx_t ctx, int pe, void *va, void **rva);

typedef struct mca_spml_ucx_ctx_array {
    int                      ctxs_count;
    int                      ctxs_num;
    mca_spml_ucx_ctx_t       **ctxs;
} mca_spml_ucx_ctx_array_t;

struct mca_spml_ucx {
    mca_spml_base_module_t   super;
    ucp_context_h            ucp_context;
    int                      num_disconnect;
    int                      heap_reg_nb;
    bool                     enabled;
    mca_spml_ucx_get_mkey_slow_fn_t get_mkey_slow;
    char                     **remote_addrs_tbl;
    mca_spml_ucx_ctx_array_t active_array;
    mca_spml_ucx_ctx_array_t idle_array;
    int                      priority; /* component priority */
    shmem_internal_mutex_t   internal_mutex;
    pthread_mutex_t          ctx_create_mutex;
    /* Fields controlling aux context for put_all_nb SPML routine */
    bool                     async_progress;
    int                      async_tick;
    opal_event_base_t        *async_event_base;
    opal_event_t             *tick_event;
    mca_spml_ucx_ctx_t       *aux_ctx;
    pthread_spinlock_t       async_lock;
    int                      aux_refcnt;
    bool                     synchronized_quiet;
};
typedef struct mca_spml_ucx mca_spml_ucx_t;

extern mca_spml_ucx_t mca_spml_ucx;

extern int mca_spml_ucx_enable(bool enable);
extern int mca_spml_ucx_ctx_create(long options,
                                   shmem_ctx_t *ctx);
extern void mca_spml_ucx_ctx_destroy(shmem_ctx_t ctx);
extern int mca_spml_ucx_get(shmem_ctx_t ctx,
                              void* dst_addr,
                              size_t size,
                              void* src_addr,
                              int src);
extern int mca_spml_ucx_get_nb(shmem_ctx_t ctx,
                              void* dst_addr,
                              size_t size,
                              void* src_addr,
                              int src,
                              void **handle);

extern int mca_spml_ucx_put(shmem_ctx_t ctx,
                            void* dst_addr,
                            size_t size,
                            void* src_addr,
                            int dst);

extern int mca_spml_ucx_put_nb(shmem_ctx_t ctx,
                               void* dst_addr,
                               size_t size,
                               void* src_addr,
                               int dst,
                               void **handle);

extern int mca_spml_ucx_recv(void* buf, size_t size, int src);
extern int mca_spml_ucx_send(void* buf,
                             size_t size,
                             int dst,
                             mca_spml_base_put_mode_t mode);

extern int mca_spml_ucx_put_all_nb(void *target,
                                   const void *source,
                                   size_t size,
                                   long *counter);

extern sshmem_mkey_t *mca_spml_ucx_register(void* addr,
                                                size_t size,
                                                uint64_t shmid,
                                                int *count);
extern int mca_spml_ucx_deregister(sshmem_mkey_t *mkeys);

extern void mca_spml_ucx_memuse_hook(void *addr, size_t length);

extern void mca_spml_ucx_rmkey_unpack(shmem_ctx_t ctx, sshmem_mkey_t *mkey, uint32_t segno, int pe, int tr_id);
extern void mca_spml_ucx_rmkey_free(sshmem_mkey_t *mkey);
extern void *mca_spml_ucx_rmkey_ptr(const void *dst_addr, sshmem_mkey_t *, int pe);

extern int mca_spml_ucx_add_procs(ompi_proc_t** procs, size_t nprocs);
extern int mca_spml_ucx_del_procs(ompi_proc_t** procs, size_t nprocs);
extern int mca_spml_ucx_fence(shmem_ctx_t ctx);
extern int mca_spml_ucx_quiet(shmem_ctx_t ctx);
extern int spml_ucx_default_progress(void);
extern int spml_ucx_ctx_progress(void);
extern int spml_ucx_progress_aux_ctx(void);
void mca_spml_ucx_async_cb(int fd, short event, void *cbdata);

int mca_spml_ucx_init_put_op_mask(mca_spml_ucx_ctx_t *ctx, size_t nprocs);
int mca_spml_ucx_clear_put_op_mask(mca_spml_ucx_ctx_t *ctx);

static inline void mca_spml_ucx_aux_lock(void)
{
    if (mca_spml_ucx.async_progress) {
        pthread_spin_lock(&mca_spml_ucx.async_lock);
    }
}

static inline void mca_spml_ucx_aux_unlock(void)
{
    if (mca_spml_ucx.async_progress) {
        pthread_spin_unlock(&mca_spml_ucx.async_lock);
    }
}

static void mca_spml_ucx_cache_mkey(mca_spml_ucx_ctx_t *ucx_ctx, sshmem_mkey_t *mkey, uint32_t segno, int dst_pe)
{
    ucp_peer_t *peer;

    peer = &(ucx_ctx->ucp_peers[dst_pe]);
    mkey_segment_init(&peer->mkeys[segno].super, mkey, segno);
}

static inline spml_ucx_mkey_t * 
mca_spml_ucx_get_mkey(shmem_ctx_t ctx, int pe, void *va, void **rva, mca_spml_ucx_t* module)
{
    spml_ucx_cached_mkey_t *mkey;
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;

    mkey = ucx_ctx->ucp_peers[pe].mkeys;
    mkey = (spml_ucx_cached_mkey_t *)map_segment_find_va(&mkey->super.super, sizeof(*mkey), va);
    assert(mkey != NULL);
    *rva = map_segment_va2rva(&mkey->super, va);
    return &mkey->key;
}

static inline int ucx_status_to_oshmem(ucs_status_t status)
{
#if OSHMEM_PARAM_CHECK == 1
    return OPAL_LIKELY(UCS_OK == status) ? OSHMEM_SUCCESS : OSHMEM_ERROR;
#else
    return OSHMEM_SUCCESS;
#endif
}

static inline int ucx_status_to_oshmem_nb(ucs_status_t status)
{
#if OSHMEM_PARAM_CHECK == 1
    return OPAL_LIKELY(status >= 0) ? OSHMEM_SUCCESS : OSHMEM_ERROR;
#else
    return OSHMEM_SUCCESS;
#endif
}

static inline void mca_spml_ucx_remote_op_posted(mca_spml_ucx_ctx_t *ctx, int dst)
{
    if (OPAL_UNLIKELY(mca_spml_ucx.synchronized_quiet)) {
        if (!opal_bitmap_is_set_bit(&ctx->put_op_bitmap, dst)) {
            ctx->put_proc_indexes[ctx->put_proc_count++] = dst;
            opal_bitmap_set_bit(&ctx->put_op_bitmap, dst);
        }
    }
}

#define MCA_SPML_UCX_CTXS_ARRAY_SIZE 64
#define MCA_SPML_UCX_CTXS_ARRAY_INC 64

END_C_DECLS

#endif

