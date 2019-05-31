/*
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"
#include <stdio.h>

#include "oshmem/constants.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/proc/proc.h"
#include "atomic_ucx.h"

/*
 * Initial query function that is invoked during initialization, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_atomic_ucx_startup(bool enable_progress_threads, bool enable_threads)
{
    return OSHMEM_SUCCESS;
}

int mca_atomic_ucx_finalize(void)
{
    return OSHMEM_SUCCESS;
}

static inline
int mca_atomic_ucx_op(shmem_ctx_t ctx,
                      void *target,
                      uint64_t value,
                      size_t size,
                      int pe,
                      ucp_atomic_post_op_t op)
{
    ucs_status_t status;
    spml_ucx_mkey_t *ucx_mkey;
    uint64_t rva;
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;

    assert((8 == size) || (4 == size));

    ucx_mkey = mca_spml_ucx_get_mkey(ctx, pe, target, (void *)&rva, mca_spml_self);
    status = ucp_atomic_post(ucx_ctx->ucp_peers[pe].ucp_conn,
                             op, value, size, rva,
                             ucx_mkey->rkey);

    if (OPAL_LIKELY(UCS_OK == status)) {
        mca_spml_ucx_remote_op_posted(ucx_ctx, pe);
    }

    return ucx_status_to_oshmem(status);
}

static inline
int mca_atomic_ucx_fop(shmem_ctx_t ctx,
                       void *target,
                       void *prev,
                       uint64_t value,
                       size_t size,
                       int pe,
                       ucp_atomic_fetch_op_t op)
{
    ucs_status_ptr_t status_ptr;
    spml_ucx_mkey_t *ucx_mkey;
    uint64_t rva;
    mca_spml_ucx_ctx_t *ucx_ctx = (mca_spml_ucx_ctx_t *)ctx;

    assert((8 == size) || (4 == size));

    ucx_mkey = mca_spml_ucx_get_mkey(ctx, pe, target, (void *)&rva, mca_spml_self);
    status_ptr = ucp_atomic_fetch_nb(ucx_ctx->ucp_peers[pe].ucp_conn,
                                     op, value, prev, size,
                                     rva, ucx_mkey->rkey,
                                     opal_common_ucx_empty_complete_cb);
    return opal_common_ucx_wait_request(status_ptr, ucx_ctx->ucp_worker,
                                        "ucp_atomic_fetch_nb");
}

static int mca_atomic_ucx_add(shmem_ctx_t ctx,
                              void *target,
                              uint64_t value,
                              size_t size,
                              int pe)
{
    return mca_atomic_ucx_op(ctx, target, value, size, pe, UCP_ATOMIC_POST_OP_ADD);
}

static int mca_atomic_ucx_and(shmem_ctx_t ctx,
                              void *target,
                              uint64_t value,
                              size_t size,
                              int pe)
{
#if HAVE_DECL_UCP_ATOMIC_POST_OP_AND
    return mca_atomic_ucx_op(ctx, target, value, size, pe, UCP_ATOMIC_POST_OP_AND);
#else
    return OSHMEM_ERR_NOT_IMPLEMENTED;
#endif
}

static int mca_atomic_ucx_or(shmem_ctx_t ctx,
                              void *target,
                              uint64_t value,
                              size_t size,
                              int pe)
{
#if HAVE_DECL_UCP_ATOMIC_POST_OP_OR
    return mca_atomic_ucx_op(ctx, target, value, size, pe, UCP_ATOMIC_POST_OP_OR);
#else
    return OSHMEM_ERR_NOT_IMPLEMENTED;
#endif
}

static int mca_atomic_ucx_xor(shmem_ctx_t ctx,
                              void *target,
                              uint64_t value,
                              size_t size,
                              int pe)
{
#if HAVE_DECL_UCP_ATOMIC_POST_OP_XOR
    return mca_atomic_ucx_op(ctx, target, value, size, pe, UCP_ATOMIC_POST_OP_XOR);
#else
    return OSHMEM_ERR_NOT_IMPLEMENTED;
#endif
}

static int mca_atomic_ucx_fadd(shmem_ctx_t ctx,
                               void *target,
                               void *prev,
                               uint64_t value,
                               size_t size,
                               int pe)
{
    return mca_atomic_ucx_fop(ctx, target, prev, value, size, pe, UCP_ATOMIC_FETCH_OP_FADD);
}

static int mca_atomic_ucx_fand(shmem_ctx_t ctx,
                               void *target,
                               void *prev,
                               uint64_t value,
                               size_t size,
                               int pe)
{
#if HAVE_DECL_UCP_ATOMIC_FETCH_OP_FAND
    return mca_atomic_ucx_fop(ctx, target, prev, value, size, pe, UCP_ATOMIC_FETCH_OP_FAND);
#else
    return OSHMEM_ERR_NOT_IMPLEMENTED;
#endif
}

static int mca_atomic_ucx_for(shmem_ctx_t ctx,
                               void *target,
                               void *prev,
                               uint64_t value,
                               size_t size,
                               int pe)
{
#if HAVE_DECL_UCP_ATOMIC_FETCH_OP_FOR
    return mca_atomic_ucx_fop(ctx, target, prev, value, size, pe, UCP_ATOMIC_FETCH_OP_FOR);
#else
    return OSHMEM_ERR_NOT_IMPLEMENTED;
#endif
}

static int mca_atomic_ucx_fxor(shmem_ctx_t ctx,
                               void *target,
                               void *prev,
                               uint64_t value,
                               size_t size,
                               int pe)
{
#if HAVE_DECL_UCP_ATOMIC_FETCH_OP_FXOR
    return mca_atomic_ucx_fop(ctx, target, prev, value, size, pe, UCP_ATOMIC_FETCH_OP_FXOR);
#else
    return OSHMEM_ERR_NOT_IMPLEMENTED;
#endif
}

static int mca_atomic_ucx_swap(shmem_ctx_t ctx,
                               void *target,
                               void *prev,
                               uint64_t value,
                               size_t size,
                               int pe)
{
    return mca_atomic_ucx_fop(ctx, target, prev, value, size, pe, UCP_ATOMIC_FETCH_OP_SWAP);
}


mca_atomic_base_module_t *
mca_atomic_ucx_query(int *priority)
{
    mca_atomic_ucx_module_t *module;

    *priority = mca_atomic_ucx_component.priority;

    module = OBJ_NEW(mca_atomic_ucx_module_t);
    if (module) {
        module->super.atomic_add   = mca_atomic_ucx_add;
        module->super.atomic_and   = mca_atomic_ucx_and;
        module->super.atomic_or    = mca_atomic_ucx_or;
        module->super.atomic_xor   = mca_atomic_ucx_xor;
        module->super.atomic_fadd  = mca_atomic_ucx_fadd;
        module->super.atomic_fand  = mca_atomic_ucx_fand;
        module->super.atomic_for   = mca_atomic_ucx_for;
        module->super.atomic_fxor  = mca_atomic_ucx_fxor;
        module->super.atomic_swap  = mca_atomic_ucx_swap;
        module->super.atomic_cswap = mca_atomic_ucx_cswap;
        return &(module->super);
    }

    return NULL ;
}
