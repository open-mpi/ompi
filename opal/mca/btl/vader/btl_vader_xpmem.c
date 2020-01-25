/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_vader.h"

#include "opal/include/opal/align.h"
#include "opal/mca/memchecker/base/base.h"

#if OPAL_BTL_VADER_HAVE_XPMEM

int mca_btl_vader_xpmem_init (void)
{
    /* Any attachment that goes past the Linux TASK_SIZE will always fail. To prevent this we need to
     * determine the value of TASK_SIZE. On x86_64 the value was hard-coded in vader to be
     * 0x7ffffffffffful but this approach does not work with AARCH64 (and possibly other architectures).
     * Since there is really no way to directly determine the value we can (in all cases?) look through
     * the mapping for this process to determine what the largest address is. This should be the top
     * of the stack. No heap allocations should be larger than this value. Since the largest address
     * may differ between processes the value must be shared as part of the modex and stored in the
     * endpoint. */
    FILE *fh = fopen("/proc/self/maps", "r");
    if (NULL == fh) {
        BTL_ERROR(("could not open /proc/self/maps for reading. disabling XPMEM"));
        return OPAL_ERR_NOT_AVAILABLE;
    }

    char buffer[1024];
    uintptr_t address_max = 0;
    while (fgets(buffer, sizeof(buffer), fh)) {
        uintptr_t low, high;
        char *tmp;
        /* each line of /proc/self/maps starts with low-high in hexidecimal (without a 0x) */
        low = strtoul(buffer, &tmp, 16);
        high = strtoul(tmp+1, NULL, 16);
        if (address_max < high) {
            address_max = high;
        }
    }

    fclose (fh);

    if (0 == address_max) {
        BTL_ERROR(("could not determine the address max"));
        return OPAL_ERR_NOT_AVAILABLE;
    }

    /* save the calcuated maximum */
    mca_btl_vader_component.my_address_max = address_max - 1;

    /* it is safe to use XPMEM_MAXADDR_SIZE here (which is always (size_t)-1 even though
     * it is not safe for attach */
    mca_btl_vader_component.my_seg_id = xpmem_make (0, XPMEM_MAXADDR_SIZE, XPMEM_PERMIT_MODE,
                                                    (void *)0666);
    if (-1 == mca_btl_vader_component.my_seg_id) {
        return OPAL_ERR_NOT_AVAILABLE;
    }

    mca_btl_vader.super.btl_get = mca_btl_vader_get_xpmem;
    mca_btl_vader.super.btl_put = mca_btl_vader_put_xpmem;

    return OPAL_SUCCESS;
}

struct vader_check_reg_ctx_t {
    mca_btl_base_endpoint_t *ep;
    mca_rcache_base_registration_t **reg;
    uintptr_t base;
    uintptr_t bound;
};
typedef struct vader_check_reg_ctx_t vader_check_reg_ctx_t;

static int vader_check_reg (mca_rcache_base_registration_t *reg, void *ctx)
{
    vader_check_reg_ctx_t *vader_ctx = (vader_check_reg_ctx_t *) ctx;

    if ((intptr_t) reg->alloc_base != vader_ctx->ep->peer_smp_rank) {
        /* ignore this registration */
        return OPAL_SUCCESS;
    }

    vader_ctx->reg[0] = reg;

    if (vader_ctx->bound <= (uintptr_t) reg->bound && vader_ctx->base >= (uintptr_t) reg->base) {
        if (0 == opal_atomic_fetch_add_32 (&reg->ref_count, 1)) {
            /* registration is being deleted by a thread in vader_return_registration. the
             * VMA tree implementation will block in mca_rcache_delete until we finish
             * iterating over the VMA tree so it is safe to just ignore this registration
             * and continue. */
            vader_ctx->reg[0] = NULL;
            return OPAL_SUCCESS;
        }
        return 1;
    }

    if (MCA_RCACHE_FLAGS_INVALID & opal_atomic_fetch_or_32(&reg->flags, MCA_RCACHE_FLAGS_INVALID)) {
        /* another thread has already marked this registration as invalid. ignore and continue. */
        vader_ctx->reg[0] = NULL;
        return OPAL_SUCCESS;
    }

    /* let the caller know we found an overlapping registration that can be coalesced into
     * the requested interval. the caller will remove the last reference and delete the
     * registration. */
    return 2;
}

void vader_return_registration (mca_rcache_base_registration_t *reg, struct mca_btl_base_endpoint_t *ep)
{
    mca_rcache_base_vma_module_t *vma_module =  mca_btl_vader_component.vma_module;
    int32_t ref_count;

    ref_count = opal_atomic_add_fetch_32 (&reg->ref_count, -1);
    if (OPAL_UNLIKELY(0 == ref_count && !(reg->flags & MCA_RCACHE_FLAGS_PERSIST))) {
#if OPAL_DEBUG
        int ret = mca_rcache_base_vma_delete (vma_module, reg);
        assert (OPAL_SUCCESS == ret);
#else
        (void) mca_rcache_base_vma_delete (vma_module, reg);
#endif
        opal_memchecker_base_mem_noaccess (reg->rcache_context, (uintptr_t)(reg->bound - reg->base));
        (void)xpmem_detach (reg->rcache_context);
        OBJ_RELEASE (reg);
    }
}

/* look up the remote pointer in the peer rcache and attach if
 * necessary */
mca_rcache_base_registration_t *vader_get_registation (struct mca_btl_base_endpoint_t *ep, void *rem_ptr,
                                                       size_t size, int flags, void **local_ptr)
{
    mca_rcache_base_vma_module_t *vma_module = mca_btl_vader_component.vma_module;
    uint64_t attach_align = 1 << mca_btl_vader_component.log_attach_align;
    mca_rcache_base_registration_t *reg = NULL;
    vader_check_reg_ctx_t check_ctx = {.ep = ep, .reg = &reg};
    xpmem_addr_t xpmem_addr;
    uintptr_t base, bound;
    int rc;

    base = OPAL_DOWN_ALIGN((uintptr_t) rem_ptr, attach_align, uintptr_t);
    bound = OPAL_ALIGN((uintptr_t) rem_ptr + size - 1, attach_align, uintptr_t) + 1;
    if (OPAL_UNLIKELY(bound > ep->segment_data.xpmem.address_max)) {
        bound = ep->segment_data.xpmem.address_max;
    }

    check_ctx.base = base;
    check_ctx.bound = bound;

    /* several segments may match the base pointer */
    rc = mca_rcache_base_vma_iterate (vma_module, (void *) base, bound - base, true, vader_check_reg, &check_ctx);
    if (2 == rc) {
        bound = bound < (uintptr_t) reg->bound ? (uintptr_t) reg->bound : bound;
        base = base > (uintptr_t) reg->base ? (uintptr_t) reg->base : base;
        vader_return_registration(reg, ep);
        reg = NULL;
    }

    if (NULL == reg) {
        reg = OBJ_NEW(mca_rcache_base_registration_t);
        if (OPAL_LIKELY(NULL != reg)) {
            /* stick around for awhile */
            reg->ref_count = 2;
            reg->base  = (unsigned char *) base;
            reg->bound = (unsigned char *) bound;
            reg->flags = flags;
            reg->alloc_base = (void *) (intptr_t) ep->peer_smp_rank;

#if defined(HAVE_SN_XPMEM_H)
            xpmem_addr.id     = ep->segment_data.xpmem.apid;
#else
            xpmem_addr.apid   = ep->segment_data.xpmem.apid;
#endif
            xpmem_addr.offset = base;

            reg->rcache_context = xpmem_attach (xpmem_addr, bound - base, NULL);
            if (OPAL_UNLIKELY((void *)-1 == reg->rcache_context)) {
                OBJ_RELEASE(reg);
                return NULL;
            }

            opal_memchecker_base_mem_defined (reg->rcache_context, bound - base);

            if (!(flags & MCA_RCACHE_FLAGS_PERSIST)) {
                mca_rcache_base_vma_insert (vma_module, reg, 0);
            }
        }
    }

    opal_atomic_wmb ();
    *local_ptr = (void *) ((uintptr_t) reg->rcache_context +
                           (ptrdiff_t)((uintptr_t) rem_ptr - (uintptr_t) reg->base));

    return reg;
}

struct vader_cleanup_reg_ctx {
    mca_btl_vader_endpoint_t *ep;
    opal_list_t *registrations;
};

static int mca_btl_vader_endpoint_xpmem_rcache_cleanup (mca_rcache_base_registration_t *reg, void *ctx)
{
    struct vader_cleanup_reg_ctx *cleanup_ctx = (struct vader_cleanup_reg_ctx *) ctx;
    if ((intptr_t) reg->alloc_base == cleanup_ctx->ep->peer_smp_rank) {
        opal_list_append(cleanup_ctx->registrations, &reg->super.super);
    }

    return OPAL_SUCCESS;
}

void mca_btl_vader_xpmem_cleanup_endpoint (struct mca_btl_base_endpoint_t *ep)
{
    mca_rcache_base_registration_t *reg;
    opal_list_t registrations;
    struct vader_cleanup_reg_ctx cleanup_ctx = {.ep = ep, .registrations = &registrations};

    OBJ_CONSTRUCT(&registrations, opal_list_t);

    /* clean out the registration cache */
    (void) mca_rcache_base_vma_iterate (mca_btl_vader_component.vma_module,
                                        NULL, (size_t) -1, true,
                                        mca_btl_vader_endpoint_xpmem_rcache_cleanup,
                                        (void *) &cleanup_ctx);
    while (NULL != (reg = (mca_rcache_base_registration_t *) opal_list_remove_first(&registrations))) {
        vader_return_registration (reg, ep);
    }
    OBJ_DESTRUCT(&registrations);

    if (ep->segment_base) {
        xpmem_release (ep->segment_data.xpmem.apid);
        ep->segment_data.xpmem.apid = 0;
    }
}

#endif /* OPAL_BTL_VADER_HAVE_XPMEM */
