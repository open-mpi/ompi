/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020-2021 Google, LLC. All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/include/opal/align.h"
#include "opal/mca/memchecker/base/base.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/mca/smsc/base/base.h"
#include "opal/mca/smsc/xpmem/smsc_xpmem_internal.h"
#include "opal/util/minmax.h"
#include "opal/util/sys_limits.h"

OBJ_CLASS_INSTANCE(mca_smsc_xpmem_endpoint_t, opal_object_t, NULL, NULL);

mca_smsc_endpoint_t *mca_smsc_xpmem_get_endpoint(opal_proc_t *peer_proc)
{
    mca_smsc_xpmem_endpoint_t *endpoint = OBJ_NEW(mca_smsc_xpmem_endpoint_t);
    if (OPAL_UNLIKELY(NULL == endpoint)) {
        return NULL;
    }

    endpoint->super.proc = peer_proc;

    int rc;
    size_t modex_size;
    mca_smsc_xpmem_modex_t *modex;
    OPAL_MODEX_RECV_IMMEDIATE(rc, &mca_smsc_xpmem_component.super.smsc_version,
                              &peer_proc->proc_name, (void **) &modex, &modex_size);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        OBJ_RELEASE(endpoint);
        return NULL;
    }

    endpoint->apid = xpmem_get(modex->seg_id, XPMEM_RDWR, XPMEM_PERMIT_MODE, (void *) 0666);
    endpoint->address_max = modex->address_max;

    return &endpoint->super;
}

struct mca_smsc_xpmem_cleanup_reg_ctx_t {
    mca_smsc_xpmem_endpoint_t *endpoint;
    opal_list_t *registrations;
};

typedef struct mca_smsc_xpmem_cleanup_reg_ctx_t mca_smsc_xpmem_cleanup_reg_ctx_t;

struct mca_smsc_xpmem_check_reg_ctx_t {
    mca_smsc_xpmem_endpoint_t *endpoint;
    mca_rcache_base_registration_t **reg;
    uintptr_t base;
    uintptr_t bound;
};
typedef struct mca_smsc_xpmem_check_reg_ctx_t mca_smsc_xpmem_check_reg_ctx_t;

static int mca_smsc_xpmem_check_reg(mca_rcache_base_registration_t *reg, void *ctx)
{
    mca_smsc_xpmem_check_reg_ctx_t *xpmem_ctx = (mca_smsc_xpmem_check_reg_ctx_t *) ctx;

    if (reg->alloc_base != (void *) xpmem_ctx->endpoint) {
        /* ignore this registration */
        return OPAL_SUCCESS;
    }

    xpmem_ctx->reg[0] = reg;

    if (xpmem_ctx->bound <= (uintptr_t) reg->bound && xpmem_ctx->base >= (uintptr_t) reg->base) {
        if (0 == opal_atomic_fetch_add_32(&reg->ref_count, 1)) {
            /* registration is being deleted by a thread in sm_return_registration. the
             * VMA tree implementation will block in mca_rcache_delete until we finish
             * iterating over the VMA tree so it is safe to just ignore this registration
             * and continue. */
            xpmem_ctx->reg[0] = NULL;
            return OPAL_SUCCESS;
        }
        return 1;
    }

    if (MCA_RCACHE_FLAGS_INVALID & opal_atomic_fetch_or_32(&reg->flags, MCA_RCACHE_FLAGS_INVALID)) {
        /* another thread has already marked this registration as invalid. ignore and continue. */
        xpmem_ctx->reg[0] = NULL;
        return OPAL_SUCCESS;
    }

    /* let the caller know we found an overlapping registration that can be coalesced into
     * the requested interval. the caller will remove the last reference and delete the
     * registration. */
    return 2;
}

/* look up the remote pointer in the peer rcache and attach if
 * necessary */
void *mca_smsc_xpmem_map_peer_region(mca_smsc_endpoint_t *endpoint, uint64_t flags,
                                     void *remote_ptr, size_t size, void **local_ptr)
{
    mca_smsc_xpmem_endpoint_t *xpmem_endpoint = (mca_smsc_xpmem_endpoint_t *) endpoint;
    mca_rcache_base_vma_module_t *vma_module = mca_smsc_xpmem_module.vma_module;
    uint64_t attach_align = 1 << mca_smsc_xpmem_component.log_attach_align;
    mca_rcache_base_registration_t *reg = NULL;
    mca_smsc_xpmem_check_reg_ctx_t check_ctx = {.endpoint = xpmem_endpoint, .reg = &reg};
    xpmem_addr_t xpmem_addr;
    uintptr_t base, bound;
    int rc;

    base = OPAL_DOWN_ALIGN((uintptr_t) remote_ptr, attach_align, uintptr_t);
    bound = OPAL_ALIGN((uintptr_t) remote_ptr + size, attach_align, uintptr_t);
    if (OPAL_UNLIKELY(bound > xpmem_endpoint->address_max)) {
        bound = xpmem_endpoint->address_max;
    }

    check_ctx.base = base;
    check_ctx.bound = bound;

    /* several segments may match the base pointer */
    rc = mca_rcache_base_vma_iterate(vma_module, (void *) base, bound - base, true,
                                     mca_smsc_xpmem_check_reg, &check_ctx);
    if (2 == rc) {
        bound = bound < (uintptr_t) reg->bound ? (uintptr_t) reg->bound : bound;
        base = base > (uintptr_t) reg->base ? (uintptr_t) reg->base : base;
        mca_smsc_xpmem_unmap_peer_region(reg);
        reg = NULL;
    }

    if (NULL == reg) {
        reg = OBJ_NEW(mca_rcache_base_registration_t);
        if (OPAL_LIKELY(NULL != reg)) {
            /* stick around for awhile */
            reg->ref_count = 2;
            reg->base = (unsigned char *) base;
            reg->bound = (unsigned char *) bound;
            reg->alloc_base = (void *) endpoint;

#if defined(HAVE_SN_XPMEM_H)
            xpmem_addr.id = xpmem_endpoint->apid;
#else
            xpmem_addr.apid = xpmem_endpoint->apid;
#endif
            xpmem_addr.offset = base;

            opal_output_verbose(MCA_BASE_VERBOSE_INFO, opal_smsc_base_framework.framework_output,
                                "mca_smsc_xpmem_map_peer_region: creating region mapping "
                                "for endpoint %p address range %p-%p",
                                endpoint, reg->base, reg->bound);

            reg->rcache_context = xpmem_attach(xpmem_addr, bound - base, NULL);
            if (OPAL_UNLIKELY((void *) -1 == reg->rcache_context)) {
                /* retry with the page as upper bound */
                bound = OPAL_ALIGN((uintptr_t) remote_ptr + size, opal_getpagesize(), uintptr_t);
                reg->bound = (unsigned char *) bound;
                reg->rcache_context = xpmem_attach(xpmem_addr, bound - base, NULL);
                if (OPAL_UNLIKELY((void *) -1 == reg->rcache_context)) {
                    OBJ_RELEASE(reg);
                    return NULL;
                }
            }

            opal_memchecker_base_mem_defined(reg->rcache_context, bound - base);

            mca_rcache_base_vma_insert(vma_module, reg, 0);
        }
    }

    opal_atomic_wmb();
    *local_ptr = (void *) ((uintptr_t) reg->rcache_context
                           + (ptrdiff_t)((uintptr_t) remote_ptr - (uintptr_t) reg->base));

    return (void *) reg;
}

void mca_smsc_xpmem_unmap_peer_region(void *ctx)
{
    mca_rcache_base_registration_t *reg = (mca_rcache_base_registration_t *) ctx;
    mca_rcache_base_vma_module_t *vma_module = mca_smsc_xpmem_module.vma_module;
    int32_t ref_count;

    ref_count = opal_atomic_add_fetch_32(&reg->ref_count, -1);
    if (OPAL_UNLIKELY(0 == ref_count && !(reg->flags & MCA_RCACHE_FLAGS_PERSIST))) {
        opal_output_verbose(MCA_BASE_VERBOSE_INFO, opal_smsc_base_framework.framework_output,
                            "mca_smsc_xpmem_unmap_peer_region: deleting region mapping for "
                            "endpoint %p address range %p-%p",
                            reg->alloc_base, reg->base, reg->bound);
#if OPAL_ENABLE_DEBUG
        int ret = mca_rcache_base_vma_delete(vma_module, reg);
        assert(OPAL_SUCCESS == ret);
#else
        (void) mca_rcache_base_vma_delete(vma_module, reg);
#endif
        opal_memchecker_base_mem_noaccess(reg->rcache_context, (uintptr_t)(reg->bound - reg->base));
        (void) xpmem_detach(reg->rcache_context);
        OBJ_RELEASE(reg);
    }
}

static int mca_smsc_xpmem_endpoint_rcache_cleanup(mca_rcache_base_registration_t *reg, void *ctx)
{
    mca_smsc_xpmem_cleanup_reg_ctx_t *cleanup_ctx = (mca_smsc_xpmem_cleanup_reg_ctx_t *) ctx;
    if (reg->alloc_base == (void *) cleanup_ctx->endpoint) {
        opal_list_append(cleanup_ctx->registrations, &reg->super.super);
    }

    return OPAL_SUCCESS;
}

static void mca_smsc_xpmem_cleanup_endpoint(mca_smsc_xpmem_endpoint_t *endpoint)
{
    mca_rcache_base_registration_t *reg;
    opal_list_t registrations;
    mca_smsc_xpmem_cleanup_reg_ctx_t cleanup_ctx = {.endpoint = endpoint,
                                                    .registrations = &registrations};

    opal_output_verbose(MCA_BASE_VERBOSE_INFO, opal_smsc_base_framework.framework_output,
                        "mca_smsc_xpmem_cleanup_endpoint: cleaning up endpoint %p", endpoint);

    OBJ_CONSTRUCT(&registrations, opal_list_t);

    /* clean out the registration cache */
    (void) mca_rcache_base_vma_iterate(mca_smsc_xpmem_module.vma_module, NULL, (size_t) -1, true,
                                       mca_smsc_xpmem_endpoint_rcache_cleanup,
                                       (void *) &cleanup_ctx);
    opal_output_verbose(MCA_BASE_VERBOSE_INFO, opal_smsc_base_framework.framework_output,
                        "mca_smsc_xpmem_cleanup_endpoint: deleting %" PRIsize_t " region mappings",
                        opal_list_get_size(&registrations));
    while (NULL
           != (reg = (mca_rcache_base_registration_t *) opal_list_remove_first(&registrations))) {
        mca_smsc_xpmem_unmap_peer_region(reg);
    }
    OBJ_DESTRUCT(&registrations);

    xpmem_release(endpoint->apid);
    endpoint->apid = 0;
}

void mca_smsc_xpmem_return_endpoint(mca_smsc_endpoint_t *endpoint)
{
    mca_smsc_xpmem_cleanup_endpoint((mca_smsc_xpmem_endpoint_t *) endpoint);
    OBJ_RELEASE(endpoint);
}

/* memcpy is faster at larger sizes but is undefined if the
   pointers are aliased (TODO -- readd alias check) */
static inline void mca_smsc_xpmem_memmove(void *dst, void *src, size_t size)
{
    while (size > 0) {
        size_t copy_size = opal_min(size, mca_smsc_xpmem_component.memcpy_chunk_size);
        memcpy(dst, src, copy_size);
        dst = (void *) ((uintptr_t) dst + copy_size);
        src = (void *) ((uintptr_t) src + copy_size);
        size -= copy_size;
    }
}

int mca_smsc_xpmem_copy_to(mca_smsc_endpoint_t *endpoint, void *local_address, void *remote_address,
                           size_t size, void *reg_handle)
{
    /* ignore the registration handle as it is not used for XPMEM */
    (void) reg_handle;

    void *remote_ptr, *ctx;
    ctx = mca_smsc_xpmem_map_peer_region(endpoint, /*flags=*/0, remote_address, size, &remote_ptr);
    mca_smsc_xpmem_memmove(remote_ptr, local_address, size);

    mca_smsc_xpmem_unmap_peer_region(ctx);

    return OPAL_SUCCESS;
}

int mca_smsc_xpmem_copy_from(mca_smsc_endpoint_t *endpoint, void *local_address,
                             void *remote_address, size_t size, void *reg_handle)
{
    /* ignore the registration handle as it is not used for XPMEM */
    (void) reg_handle;

    void *remote_ptr, *ctx;

    struct timespec start, stop;
    ctx = mca_smsc_xpmem_map_peer_region(endpoint, /*flags=*/0, remote_address, size, &remote_ptr);
    mca_smsc_xpmem_memmove(local_address, remote_ptr, size);

    mca_smsc_xpmem_unmap_peer_region(ctx);

    return OPAL_SUCCESS;
}

/* unsupported interfaces defined to support MCA direct */
void *mca_smsc_xpmem_register_region(void *local_address, size_t size)
{
    return NULL;
}

void mca_smsc_xpmem_deregister_region(void *reg_data)
{
}

mca_smsc_xpmem_module_t mca_smsc_xpmem_module = {
    .super = {
        .features = MCA_SMSC_FEATURE_CAN_MAP,
        .get_endpoint = mca_smsc_xpmem_get_endpoint,
        .return_endpoint = mca_smsc_xpmem_return_endpoint,
        .copy_to = mca_smsc_xpmem_copy_to,
        .copy_from = mca_smsc_xpmem_copy_from,
        .map_peer_region = mca_smsc_xpmem_map_peer_region,
        .unmap_peer_region = mca_smsc_xpmem_unmap_peer_region,
    },
};
