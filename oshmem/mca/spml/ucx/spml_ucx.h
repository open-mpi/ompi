/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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

#include "opal/mca/common/ucx/common_ucx.h"

#include <ucp/api/ucp.h>

BEGIN_C_DECLS

#define SPML_UCX_ASSERT  MCA_COMMON_UCX_ASSERT
#define SPML_UCX_ERROR   MCA_COMMON_UCX_ERROR
#define SPML_UCX_VERBOSE MCA_COMMON_UCX_VERBOSE
#define SPML_UCX_TRANSP_IDX 0
#define SPML_UCX_TRANSP_CNT 1
#define SPML_UCX_SERVICE_SEG 0

enum {
    SPML_UCX_STRONG_ORDERING_NONE  = 0, /* don't use strong ordering */
    SPML_UCX_STRONG_ORDERING_GETNB = 1, /* use non-blocking read to provide ordering */
    SPML_UCX_STRONG_ORDERING_GET   = 2, /* use blocking read to provide ordering*/
    SPML_UCX_STRONG_ORDERING_FLUSH = 3  /* flush EP to provide ordering */
};

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
    spml_ucx_cached_mkey_t **mkeys;
    size_t                   mkeys_cnt;
};
typedef struct ucp_peer ucp_peer_t;

/* An rkey_store entry */
typedef struct mca_spml_ucx_rkey {
    ucp_rkey_h rkey;
    int        refcnt;
} mca_spml_ucx_rkey_t;

typedef struct mca_spml_ucx_rkey_store {
    mca_spml_ucx_rkey_t *array;
    int                  size;
    int                  count;
} mca_spml_ucx_rkey_store_t;

struct mca_spml_ucx_ctx {
    ucp_worker_h             *ucp_worker;
    ucp_peer_t               *ucp_peers;
    long                      options;
    opal_bitmap_t             put_op_bitmap;
    unsigned long             nb_progress_cnt;
    unsigned int              ucp_workers;
    int                      *put_proc_indexes;
    unsigned                  put_proc_count;
    bool                      synchronized_quiet;
    int                       strong_sync;
    mca_spml_ucx_rkey_store_t rkey_store;
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
    char                     ***remote_addrs_tbl;
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
    unsigned long            nb_progress_thresh_global;
    unsigned long            nb_put_progress_thresh;
    unsigned long            nb_get_progress_thresh;
    unsigned long            nb_ucp_worker_progress;
    unsigned int             ucp_workers;
    unsigned int             ucp_worker_cnt;
    int                      symmetric_rkey_max_count;
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

extern int mca_spml_ucx_get_nb_wprogress(shmem_ctx_t ctx,
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

extern int mca_spml_ucx_put_nb_wprogress(shmem_ctx_t ctx,
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
extern void mca_spml_ucx_rmkey_free(sshmem_mkey_t *mkey, int pe);
extern void *mca_spml_ucx_rmkey_ptr(const void *dst_addr, sshmem_mkey_t *, int pe);

extern int mca_spml_ucx_add_procs(oshmem_group_t* group, size_t nprocs);
extern int mca_spml_ucx_del_procs(oshmem_group_t* group, size_t nprocs);
extern int mca_spml_ucx_fence(shmem_ctx_t ctx);
extern int mca_spml_ucx_quiet(shmem_ctx_t ctx);
extern int spml_ucx_default_progress(void);
extern int spml_ucx_ctx_progress(void);
extern int spml_ucx_progress_aux_ctx(void);
void mca_spml_ucx_async_cb(int fd, short event, void *cbdata);

int mca_spml_ucx_init_put_op_mask(mca_spml_ucx_ctx_t *ctx, size_t nprocs);
int mca_spml_ucx_clear_put_op_mask(mca_spml_ucx_ctx_t *ctx);
int mca_spml_ucx_peer_mkey_cache_add(ucp_peer_t *ucp_peer, int index);
int mca_spml_ucx_peer_mkey_cache_del(ucp_peer_t *ucp_peer, int segno);
void mca_spml_ucx_peer_mkey_cache_release(ucp_peer_t *ucp_peer);
void mca_spml_ucx_peer_mkey_cache_init(mca_spml_ucx_ctx_t *ucx_ctx, int pe);

extern int mca_spml_ucx_put_signal(shmem_ctx_t ctx, void* dst_addr, size_t size, void*
        src_addr, uint64_t *sig_addr, uint64_t signal, int sig_op, int dst);

extern int mca_spml_ucx_put_signal_nb(shmem_ctx_t ctx, void* dst_addr, size_t size,
        void* src_addr, uint64_t *sig_addr, uint64_t signal, int sig_op, int
        dst);
extern void mca_spml_ucx_wait_until_all(void *ivars, int cmp, void
        *cmp_value, size_t nelems, const int *status, int datatype);
extern size_t mca_spml_ucx_wait_until_any(void *ivars, int cmp, void
        *cmp_value, size_t nelems, const int *status, int datatype);
extern size_t mca_spml_ucx_wait_until_some(void *ivars, int cmp, void
        *cmp_value, size_t nelems, size_t *indices, const int *status, int
        datatype);
extern void mca_spml_ucx_wait_until_all_vector(void *ivars, int cmp, void
        *cmp_values, size_t nelems, const int *status, int datatype);
extern size_t mca_spml_ucx_wait_until_any_vector(void *ivars, int cmp, void
        *cmp_value, size_t nelems, const int *status, int datatype);
extern size_t mca_spml_ucx_wait_until_some_vector(void *ivars, int cmp, void
        *cmp_value, size_t nelems, size_t *indices, const int *status, int
        datatype);
extern int mca_spml_ucx_test_all(void *ivars, int cmp, void *cmp_value,
        size_t nelems, const int *status, int datatype);
extern size_t mca_spml_ucx_test_any(void *ivars, int cmp, void *cmp_value,
        size_t nelems, const int *status, int datatype);
extern size_t mca_spml_ucx_test_some(void *ivars, int cmp, void *cmp_value,
        size_t nelems, size_t *indices, const int *status, int datatype);
extern int mca_spml_ucx_test_all_vector(void *ivars, int cmp, void
        *cmp_values, size_t nelems, const int *status, int datatype);
extern int mca_spml_ucx_test_any_vector(void *ivars, int cmp, void
        *cmp_values, size_t nelems, const int *status, int datatype);
extern int mca_spml_ucx_test_some_vector(void *ivars, int cmp, void
        *cmp_values, size_t nelems, size_t *indices, const int *status, int
        datatype);
extern int mca_spml_ucx_team_sync(shmem_team_t team);
extern int mca_spml_ucx_team_my_pe(shmem_team_t team);
extern int mca_spml_ucx_team_n_pes(shmem_team_t team);
extern int mca_spml_ucx_team_get_config(shmem_team_t team, long config_mask,
        shmem_team_config_t *config);
extern int mca_spml_ucx_team_translate_pe(shmem_team_t src_team, int src_pe,
        shmem_team_t dest_team);
extern int mca_spml_ucx_team_split_strided(shmem_team_t parent_team, int start, int
        stride, int size, const shmem_team_config_t *config, long config_mask,
        shmem_team_t *new_team);
extern int mca_spml_ucx_team_split_2d(shmem_team_t parent_team, int xrange, const
        shmem_team_config_t *xaxis_config, long xaxis_mask, shmem_team_t
        *xaxis_team, const shmem_team_config_t *yaxis_config, long yaxis_mask,
        shmem_team_t *yaxis_team);
extern int mca_spml_ucx_team_destroy(shmem_team_t team);
extern int mca_spml_ucx_team_get(shmem_ctx_t ctx, shmem_team_t *team);
extern int mca_spml_ucx_team_create_ctx(shmem_team_t team, long options, shmem_ctx_t *ctx);
extern int mca_spml_ucx_team_alltoall(shmem_team_t team, void
        *dest, const void *source, size_t nelems, int datatype);
extern int mca_spml_ucx_team_alltoalls(shmem_team_t team, void
        *dest, const void *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems,
        int datatype);
extern int mca_spml_ucx_team_broadcast(shmem_team_t team, void
        *dest, const void *source, size_t nelems, int PE_root, int datatype);
extern int mca_spml_ucx_team_collect(shmem_team_t team, void
        *dest, const void *source, size_t nelems, int datatype);
extern int mca_spml_ucx_team_fcollect(shmem_team_t team, void
        *dest, const void *source, size_t nelems, int datatype);
extern int mca_spml_ucx_team_reduce(shmem_team_t team, void
        *dest, const void *source, size_t nreduce, int operation, int datatype);

extern unsigned
mca_spml_ucx_mem_map_flags_symmetric_rkey(struct mca_spml_ucx *spml_ucx);

extern void mca_spml_ucx_rkey_store_init(mca_spml_ucx_rkey_store_t *store);
extern void mca_spml_ucx_rkey_store_cleanup(mca_spml_ucx_rkey_store_t *store);

static inline int
mca_spml_ucx_peer_mkey_get(ucp_peer_t *ucp_peer, int index, spml_ucx_cached_mkey_t **out_rmkey)
{
    *out_rmkey = NULL;
    if (OPAL_UNLIKELY((index >= (int)ucp_peer->mkeys_cnt) || (0 > index))) {
        SPML_UCX_ERROR("Failed to get mkey for segment: bad index = %d, cached mkeys count: %zu",
                       index, ucp_peer->mkeys_cnt);
        return OSHMEM_ERR_BAD_PARAM;
    }
    *out_rmkey = ucp_peer->mkeys[index];
    return OSHMEM_SUCCESS;
}

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

int mca_spml_ucx_ctx_mkey_new(mca_spml_ucx_ctx_t *ucx_ctx, int pe, uint32_t segno, spml_ucx_mkey_t **mkey);
int mca_spml_ucx_ctx_mkey_cache(mca_spml_ucx_ctx_t *ucx_ctx, sshmem_mkey_t *mkey, uint32_t segno, int dst_pe);
int mca_spml_ucx_ctx_mkey_add(mca_spml_ucx_ctx_t *ucx_ctx, int pe, uint32_t segno, sshmem_mkey_t *mkey, spml_ucx_mkey_t **ucx_mkey);
int mca_spml_ucx_ctx_mkey_del(mca_spml_ucx_ctx_t *ucx_ctx, int pe, uint32_t segno, spml_ucx_mkey_t *ucx_mkey);

static inline int
mca_spml_ucx_ctx_mkey_by_seg(mca_spml_ucx_ctx_t *ucx_ctx, int pe, uint32_t segno, spml_ucx_mkey_t **mkey)
{
    ucp_peer_t *ucp_peer;
    spml_ucx_cached_mkey_t *ucx_cached_mkey;
    int rc;
    ucp_peer = &(ucx_ctx->ucp_peers[pe]);
    rc = mca_spml_ucx_peer_mkey_get(ucp_peer, segno, &ucx_cached_mkey);
    if (OSHMEM_SUCCESS != rc) {
        return rc;
    }
    *mkey = &(ucx_cached_mkey->key);
    return OSHMEM_SUCCESS;
}

static inline spml_ucx_mkey_t * 
mca_spml_ucx_ctx_mkey_by_va(shmem_ctx_t ctx, int pe, void *va, void **rva, mca_spml_ucx_t* module)
{
    spml_ucx_cached_mkey_t **mkey;
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;
    size_t i;

    mkey = ucx_ctx->ucp_peers[pe].mkeys;
    for (i = 0; i < ucx_ctx->ucp_peers[pe].mkeys_cnt; i++) {
        if (NULL == mkey[i]) {
            continue;
        }
        if (OPAL_LIKELY(map_segment_is_va_in(&mkey[i]->super.super, va))) {
            *rva = map_segment_va2rva(&mkey[i]->super, va);
            return &mkey[i]->key;
        }
    }
    return NULL;
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

static inline int mca_spml_ucx_is_strong_ordering(mca_spml_ucx_ctx_t *ctx)
{
    return (ctx->strong_sync != SPML_UCX_STRONG_ORDERING_NONE) ||
           ctx->synchronized_quiet;
}

static inline void mca_spml_ucx_remote_op_posted(mca_spml_ucx_ctx_t *ctx, int dst)
{
    if (OPAL_UNLIKELY(mca_spml_ucx_is_strong_ordering(ctx))) {
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
