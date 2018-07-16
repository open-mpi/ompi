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
    spml_ucx_cached_mkey_t   mkeys[MCA_MEMHEAP_SEG_COUNT];
};
typedef struct ucp_peer ucp_peer_t;
 
struct mca_spml_ucx_ctx {
    ucp_worker_h             ucp_worker;
    ucp_peer_t              *ucp_peers;
    long                     options;
};
typedef struct mca_spml_ucx_ctx mca_spml_ucx_ctx_t;

extern mca_spml_ucx_ctx_t mca_spml_ucx_ctx_default;

struct mca_spml_ucx_ctx_list_item {
    opal_list_item_t super;
    mca_spml_ucx_ctx_t ctx;
};
typedef struct mca_spml_ucx_ctx_list_item mca_spml_ucx_ctx_list_item_t;

typedef spml_ucx_mkey_t * (*mca_spml_ucx_get_mkey_slow_fn_t)(int pe, void *va, void **rva);

struct mca_spml_ucx {
    mca_spml_base_module_t   super;
    ucp_context_h            ucp_context;
    int                      num_disconnect;
    int                      heap_reg_nb;
    bool                     enabled;
    mca_spml_ucx_get_mkey_slow_fn_t get_mkey_slow;
    char                     **remote_addrs_tbl;
    opal_list_t              ctx_list;
    int                      priority; /* component priority */
    shmem_internal_mutex_t   internal_mutex;
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

extern sshmem_mkey_t *mca_spml_ucx_register(void* addr,
                                                size_t size,
                                                uint64_t shmid,
                                                int *count);
extern int mca_spml_ucx_deregister(sshmem_mkey_t *mkeys);

extern void mca_spml_ucx_memuse_hook(void *addr, size_t length);

extern void mca_spml_ucx_rmkey_unpack(sshmem_mkey_t *mkey, uint32_t segno, int pe, int tr_id);
extern void mca_spml_ucx_rmkey_free(sshmem_mkey_t *mkey);
extern void *mca_spml_ucx_rmkey_ptr(const void *dst_addr, sshmem_mkey_t *, int pe);

extern int mca_spml_ucx_add_procs(ompi_proc_t** procs, size_t nprocs);
extern int mca_spml_ucx_del_procs(ompi_proc_t** procs, size_t nprocs);
extern int mca_spml_ucx_fence(shmem_ctx_t ctx);
extern int mca_spml_ucx_quiet(shmem_ctx_t ctx);
extern int spml_ucx_progress(void);


static inline spml_ucx_mkey_t * 
mca_spml_ucx_get_mkey(mca_spml_ucx_ctx_t *ucx_ctx, int pe, void *va, void **rva, mca_spml_ucx_t* module)
{
    spml_ucx_cached_mkey_t *mkey;

    mkey = ucx_ctx->ucp_peers[pe].mkeys;
    mkey = (spml_ucx_cached_mkey_t *)map_segment_find_va(&mkey->super.super, sizeof(*mkey), va);
    if (OPAL_UNLIKELY(NULL == mkey)) {
        assert(module->get_mkey_slow);
        return module->get_mkey_slow(pe, va, rva);
    }
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

END_C_DECLS

#endif

