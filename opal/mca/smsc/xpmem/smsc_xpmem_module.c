/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020-2021 Google, LLC. All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2022-2023 Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
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
    int rc;
    size_t modex_size;
    mca_smsc_xpmem_modex_t *modex;

    OPAL_MODEX_RECV_IMMEDIATE(rc, &mca_smsc_xpmem_component.super.smsc_version,
                              &peer_proc->proc_name, (void **) &modex, &modex_size);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return NULL;
    }

    mca_smsc_xpmem_endpoint_t *endpoint = OBJ_NEW(mca_smsc_xpmem_endpoint_t);
    if (OPAL_UNLIKELY(NULL == endpoint)) {
        return NULL;
    }

    endpoint->super.proc = peer_proc;
    endpoint->address_max = modex->address_max;

    endpoint->vma_module = mca_rcache_base_vma_module_alloc();
    if (OPAL_UNLIKELY(NULL == endpoint->vma_module)) {
        OBJ_RELEASE(endpoint);
        return NULL;
    }

    endpoint->apid = xpmem_get(modex->seg_id, XPMEM_RDWR, XPMEM_PERMIT_MODE, (void *) 0666);

    if(OPAL_UNLIKELY(-1 == endpoint->apid)) {
        OBJ_RELEASE(endpoint->vma_module);
        OBJ_RELEASE(endpoint);
        return NULL;
    }

    return &endpoint->super;
}

/* look up the remote pointer in the peer rcache and attach if necessary */
void *mca_smsc_xpmem_map_peer_region(mca_smsc_endpoint_t *endpoint, uint64_t flags,
                                     void *remote_ptr, size_t size, void **local_ptr)
{
    mca_smsc_xpmem_endpoint_t *xpmem_endpoint = (mca_smsc_xpmem_endpoint_t *) endpoint;
    mca_rcache_base_vma_module_t *vma_module = xpmem_endpoint->vma_module;
    uintptr_t attach_align = 1 << mca_smsc_xpmem_component.log_attach_align;
    mca_rcache_base_registration_t *reg = NULL;
    xpmem_addr_t xpmem_addr;
    uintptr_t base, bound;
    int rc;

    // base is the first byte of the region, bound is the last (inclusive)
    base = OPAL_DOWN_ALIGN((uintptr_t) remote_ptr, attach_align, uintptr_t);
    bound = OPAL_ALIGN((uintptr_t) remote_ptr + size, attach_align, uintptr_t) - 1;
    if (OPAL_UNLIKELY(bound > xpmem_endpoint->address_max)) {
        bound = xpmem_endpoint->address_max;
    }

    rc = mca_rcache_base_vma_find(vma_module, (void *) base, bound - base + 1, &reg);
    assert(OPAL_SUCCESS == rc);

    if (reg) {
        int32_t old_ref_count = opal_atomic_fetch_add_32(&reg->ref_count, 1);

        if (0 == old_ref_count) {
            /* Registration is being deleted by another thread
             * in mca_smsc_xpmem_unmap_peer_region, ignore it. */
            reg = NULL;
        }
    } else {
        /* If there is a registration that overlaps with the requested range, but
         * does not fully cover it, we destroy it and make in its place a new one
         * that covers both the existing and the new range. */

        /* The search settings below will also match areas that would be right next to
         * the new one (technically not overlapping, but uniteable under a single area).
         * Whether we want this is debatable (re-establishing an XPMEM attachment can
         * incur significant overhead). The current choice matches legacy behaviour. */

        // Ideally, we would want a find() method capable of partial matching
        uintptr_t search_base[] = {base, bound, base - 1, bound + 1};
        for (size_t i = 0; i < sizeof(search_base)/sizeof(search_base[0]); i++) {
            mca_rcache_base_registration_t *ov_reg = NULL;

            rc = mca_rcache_base_vma_find(vma_module, (void *) search_base[i], 1, &ov_reg);
            assert(OPAL_SUCCESS == rc);

            if (ov_reg) {
                /* Found an overlapping area. Set the invalid flag, to mark the deletion
                 * of this old registration (will eventually take place in unmap_peer_region).
                 * If another thread has already marked deletion, do nothing. */

                uint32_t old_flags = opal_atomic_fetch_or_32(
                    (volatile opal_atomic_int32_t *) &ov_reg->flags, MCA_RCACHE_FLAGS_INVALID);

                if (!(old_flags & MCA_RCACHE_FLAGS_INVALID)) {
                    base = opal_min(base, (uintptr_t) ov_reg->base);
                    bound = opal_max(bound, (uintptr_t) ov_reg->bound);

                    /* unmap_peer_region will decrement the ref count and dealloc the attachment
                     * if it drops to 0. But we didn't increment the ref count when we found the
                     * reg as is customary. If PERSIST was set, there is superfluous ref present
                     * from when we initialized ref_count to 2 instead of 1, so we good. If not,
                     * manually add the missing reference here; otherwise the count would drop to
                     * -1, or the reg might be deleted while still in use elsewhere. */
                    if (!(MCA_RCACHE_FLAGS_PERSIST & ov_reg->flags))
                        opal_atomic_add(&ov_reg->ref_count, 1);

                    mca_smsc_xpmem_unmap_peer_region(ov_reg);
                }
            }
        }
    }

    if (NULL == reg) {
        reg = OBJ_NEW(mca_rcache_base_registration_t);
        if (OPAL_LIKELY(NULL == reg)) {
            return NULL;
        }

        // PERSIST is implemented by keeping an extra reference around
        reg->ref_count = ((flags & MCA_RCACHE_FLAGS_PERSIST)
            && !(flags & MCA_RCACHE_FLAGS_CACHE_BYPASS) ? 2 : 1);
        reg->flags = flags;
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
                            (void *) endpoint, reg->base, reg->bound);

        reg->rcache_context = xpmem_attach(xpmem_addr, bound - base + 1, NULL);
        if (OPAL_UNLIKELY((void *) -1 == reg->rcache_context)) {
            uintptr_t old_bound = bound;

            /* retry with the page as upper bound */
            bound = OPAL_ALIGN((uintptr_t) remote_ptr + size, opal_getpagesize(), uintptr_t) - 1;
            reg->bound = (unsigned char *) bound;

            opal_output_verbose(MCA_BASE_VERBOSE_INFO, opal_smsc_base_framework.framework_output,
                                "mca_smsc_xpmem_map_peer_region: region mapping "
                                "for endpoint %p address range %p-%p failed. "
                                "retrying with range %p-%p",
                                (void *) endpoint, reg->base, (void *) old_bound,
                                reg->base, reg->bound);

            reg->rcache_context = xpmem_attach(xpmem_addr, bound - base + 1, NULL);
            if (OPAL_UNLIKELY((void *) -1 == reg->rcache_context)) {
                OBJ_RELEASE(reg);
                return NULL;
            }
        }

        opal_memchecker_base_mem_defined(reg->rcache_context, bound - base + 1);

        if (!(reg->flags & MCA_RCACHE_FLAGS_CACHE_BYPASS)) {
            rc = mca_rcache_base_vma_insert(vma_module, reg, 0);
            assert(OPAL_SUCCESS == rc);

            if (OPAL_SUCCESS != rc) {
                reg->flags |= MCA_RCACHE_FLAGS_CACHE_BYPASS;
            }
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
    mca_smsc_xpmem_endpoint_t *endpoint = (mca_smsc_xpmem_endpoint_t *) reg->alloc_base;
    int32_t ref_count;

    ref_count = opal_atomic_add_fetch_32(&reg->ref_count, -1);
    if (OPAL_UNLIKELY(0 == ref_count)) {
        opal_output_verbose(MCA_BASE_VERBOSE_INFO, opal_smsc_base_framework.framework_output,
                            "mca_smsc_xpmem_unmap_peer_region: deleting region mapping for "
                            "endpoint %p address range %p-%p",
                            (void *) endpoint, reg->base, reg->bound);

        if (!(reg->flags & MCA_RCACHE_FLAGS_CACHE_BYPASS)) {
            int ret = mca_rcache_base_vma_delete(endpoint->vma_module, reg);
            assert(OPAL_SUCCESS == ret);
            (void) ret;
        }

        opal_memchecker_base_mem_noaccess(reg->rcache_context, (uintptr_t)(reg->bound - reg->base + 1));
        (void) xpmem_detach(reg->rcache_context);

        OBJ_RELEASE(reg);
    }
}

static int mca_smsc_xpmem_endpoint_rcache_entry_cleanup(mca_rcache_base_registration_t *reg, void *ctx)
{
    // See respective comment in mca_smsc_xpmem_map_peer_region
    if (!(MCA_RCACHE_FLAGS_PERSIST & reg->flags))
        opal_atomic_add(&reg->ref_count, 1);

    mca_smsc_xpmem_unmap_peer_region(reg);
    return OPAL_SUCCESS;
}

static void mca_smsc_xpmem_cleanup_endpoint(mca_smsc_xpmem_endpoint_t *endpoint)
{
    opal_output_verbose(MCA_BASE_VERBOSE_INFO, opal_smsc_base_framework.framework_output,
                        "mca_smsc_xpmem_cleanup_endpoint: cleaning up endpoint %p", (void *) endpoint);

    opal_output_verbose(MCA_BASE_VERBOSE_INFO, opal_smsc_base_framework.framework_output,
                        "mca_smsc_xpmem_cleanup_endpoint: deleting %" PRIsize_t " region mappings",
                        endpoint->vma_module->tree.tree_size);

    /* clean out the registration cache */
    (void) mca_rcache_base_vma_iterate(endpoint->vma_module, NULL, (size_t) -1, true,
                                       mca_smsc_xpmem_endpoint_rcache_entry_cleanup, NULL);

    OBJ_RELEASE(endpoint->vma_module);
    xpmem_release(endpoint->apid);

    endpoint->vma_module = NULL;
    endpoint->apid = 0;
}

void mca_smsc_xpmem_return_endpoint(mca_smsc_endpoint_t *endpoint)
{
    mca_smsc_xpmem_cleanup_endpoint((mca_smsc_xpmem_endpoint_t *) endpoint);
    OBJ_RELEASE(endpoint);
}

/* memcpy is faster at larger sizes but is undefined if the
   pointers are aliased (TODO -- read alias check) */
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
    ctx = mca_smsc_xpmem_map_peer_region(endpoint,
        MCA_RCACHE_FLAGS_PERSIST, remote_address, size, &remote_ptr);
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

    ctx = mca_smsc_xpmem_map_peer_region(endpoint,
        MCA_RCACHE_FLAGS_PERSIST, remote_address, size, &remote_ptr);
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
