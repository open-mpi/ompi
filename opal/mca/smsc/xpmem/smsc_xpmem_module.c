/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020-2021 Google, LLC. All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2022      Computer Architecture and VLSI Systems (CARV)
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

    endpoint->vma_module = mca_rcache_base_vma_module_alloc();
    if (OPAL_UNLIKELY(NULL == endpoint->vma_module)) {
        OBJ_RELEASE(endpoint);
        return NULL;
    }

    endpoint->apid = xpmem_get(modex->seg_id, XPMEM_RDWR, XPMEM_PERMIT_MODE, (void *) 0666);
    endpoint->address_max = modex->address_max;

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

    /* base is the first byte of the region, bound is the last (inclusive) */
    base = OPAL_DOWN_ALIGN((uintptr_t) remote_ptr, attach_align, uintptr_t);
    bound = OPAL_ALIGN((uintptr_t) remote_ptr + size, attach_align, uintptr_t) - 1;
    if (OPAL_UNLIKELY(bound > xpmem_endpoint->address_max)) {
        bound = xpmem_endpoint->address_max;
    }

    printf("user ptr %p size %lu base %p bound %p\n", remote_ptr, size, base, bound);
    printf("search base %p len %p\n", base, bound - base + 1);

    rc = mca_rcache_base_vma_find(vma_module, (void *) base, bound - base + 1, &reg);
    assert(OPAL_SUCCESS == rc);

    // TODO Add rcache stats?

    // TODO what if reg is deleted between finding it and atomically fetching the
    // ref count? Or will the tree block? (this could also happen inside the tree's code)

    if (reg) {
        printf("region match %p-%p\n", reg->base, reg->bound);

        int32_t old_ref_count = opal_atomic_fetch_add_32(&reg->ref_count, 1);
        if(0 == old_ref_count) {
            /* Registration is being deleted by another thread
             * in mca_smsc_xpmem_unmap_peer_region, ignore it. */
            reg = NULL;
        }

        // TODO what if two threads increment the ref counter while a third one is
        // deleting it? One of the increment-threads will see 1 as the old value
        // and go ahead with using the registration, while the writer will delete it!

        // Do we ultimately have to do something like this?

        // int32_t ref_count = opal_atomic_load_32(&reg->ref_count);

        // while(true) {
            // if(0 == ref_count) {
                // reg = NULL;
                // break;
            // }

            // if(opal_atomic_compare_exchange_strong_32(
                    // &reg->ref_count, &ref_count, ref_count + 1)) {
                // break;
            // }
        // }
    } else {
        printf("no region match\n");

        /* If there is a registration that overlaps with the requested range, but
         * does not fully cover it, we destroy it and make in its place a new one
         * that covers both the existing and the new range. */

        // uintptr_t search_begin[4] = {base, bound, base - 1, bound + 1};
        uintptr_t search_begin[2] = {base, bound};
        for (size_t i = 0; i < 2; i++) {
            printf("search overlapping %p-%p\n",
                search_begin[i], search_begin[i]+1);

            rc = mca_rcache_base_vma_find(vma_module, (void *) search_begin[i], 1, &reg);
            assert(OPAL_SUCCESS == rc);

            if (reg) {
                break;
            }
        }

        if (reg) {
            printf("found overlapping\n");

            /* Set the invalid flag, to mark the deletion of this registration
             * (will take place in unmap_peer_region). If another thread has
             * already marked deletion, ignore. */

            uint32_t old_flags = opal_atomic_fetch_or_32(
                (volatile opal_atomic_int32_t *) &reg->flags, MCA_RCACHE_FLAGS_INVALID);

            if (!(old_flags & MCA_RCACHE_FLAGS_INVALID)) {
                printf("handling merge\n");

                base = opal_min(base, (uintptr_t) reg->base);
                bound = opal_max(bound, (uintptr_t) reg->bound);

                /* unmap_peer_region will decrement the ref count, but we did not
                 * increment it when we found the reg. If persist was not set,
                 * a superflous ref is present, so all is fine. If not, we need
                 * to manually adjust before calling unmap_peer_region, to avoid
                 * deallocation while someone is still using the reg. */
                if(!(MCA_RCACHE_FLAGS_PERSIST & reg->flags))
                    opal_atomic_add(&reg->ref_count, 1);

                printf("set invalid, ref count before unmap call %d\n", reg->ref_count);

                mca_smsc_xpmem_unmap_peer_region(reg);
            }

            reg = NULL;
        } else
            printf("no overlapping\n");
    }

    if (NULL == reg) {
        reg = OBJ_NEW(mca_rcache_base_registration_t);
        if (OPAL_LIKELY(NULL == reg)) {
            return NULL;
        }

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
        printf("xpmem attach(%p, 0x%lx) -> %p\n", base, bound - base + 1, reg->rcache_context);

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

        printf("new reg %p-%p ref count %d\n", reg->base, reg->bound, reg->ref_count);

        opal_memchecker_base_mem_defined(reg->rcache_context, bound - base + 1);

        if(!(reg->flags & MCA_RCACHE_FLAGS_CACHE_BYPASS)) {
            rc = mca_rcache_base_vma_insert(vma_module, reg, 0);
            assert(OPAL_SUCCESS == rc);

            if(OPAL_SUCCESS != rc) {
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
        printf("UNMAP reg %p-%p\n", reg->base, reg->bound);

        opal_output_verbose(MCA_BASE_VERBOSE_INFO, opal_smsc_base_framework.framework_output,
                            "mca_smsc_xpmem_unmap_peer_region: deleting region mapping for "
                            "endpoint %p address range %p-%p",
                            (void *) endpoint, reg->base, reg->bound);
        if (!(reg->flags & MCA_RCACHE_FLAGS_CACHE_BYPASS)) {
#if OPAL_ENABLE_DEBUG
            int ret = mca_rcache_base_vma_delete(endpoint->vma_module, reg);
            assert(OPAL_SUCCESS == ret);
#else
            (void) mca_rcache_base_vma_delete(endpoint->vma_module, reg);
#endif
        }

        opal_memchecker_base_mem_noaccess(reg->rcache_context, (uintptr_t)(reg->bound - reg->base + 1));
        (void) xpmem_detach(reg->rcache_context);

        OBJ_RELEASE(reg);
    }
}

static int mca_smsc_xpmem_endpoint_rcache_cleanup(mca_rcache_base_registration_t *reg, void *ctx)
{
    /* See respective comment in mca_smsc_xpmem_map_peer_region */
    if(!(MCA_RCACHE_FLAGS_PERSIST & reg->flags))
        opal_atomic_add(&reg->ref_count, 1);

    printf("cleanup reg %p-%p count %d\n", reg->base, reg->bound, reg->ref_count);

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
                                       mca_smsc_xpmem_endpoint_rcache_cleanup, NULL);

    OBJ_RELEASE(endpoint->vma_module);
    endpoint->vma_module = NULL;

    xpmem_release(endpoint->apid);
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
