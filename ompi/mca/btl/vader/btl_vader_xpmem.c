/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/mca/btl/vader/btl_vader.h"
#include "opal/include/opal/align.h"

/* largest address we can attach to using xpmem */
#define VADER_MAX_ADDRESS ((uintptr_t)0x7ffffffff000)

/* look up the remote pointer in the peer rcache and attach if
 * necessary */
mca_mpool_base_registration_t *vader_get_registation (struct mca_btl_base_endpoint_t *endpoint, void *rem_ptr,
						      size_t size, int flags)
{
    struct mca_rcache_base_module_t *rcache = endpoint->rcache;
    mca_mpool_base_registration_t *regs[10], *reg = NULL;
    struct xpmem_addr xpmem_addr;
    uintptr_t base, bound;
    int rc, i;

    if (OPAL_UNLIKELY(endpoint->peer_smp_rank == mca_btl_vader_component.my_smp_rank)) {
        return rem_ptr;
    }

    base = (uintptr_t) down_align_addr(rem_ptr, mca_btl_vader_component.log_attach_align);
    bound = (uintptr_t) up_align_addr((void *)((uintptr_t) rem_ptr + size - 1),
                                      mca_btl_vader_component.log_attach_align) + 1;
    if (OPAL_UNLIKELY(bound > VADER_MAX_ADDRESS)) {
        bound = VADER_MAX_ADDRESS;
    }

    /* several segments may match the base pointer */
    rc = rcache->rcache_find_all (rcache, (void *) base, bound - base, regs, 10);
    for (i = 0 ; i < rc ; ++i) {
        if (bound <= (uintptr_t)regs[i]->bound && base  >= (uintptr_t)regs[i]->base) {
            opal_atomic_add (&regs[i]->ref_count, 1);
            return regs[i];
        }

        if (regs[i]->flags & MCA_MPOOL_FLAGS_PERSIST) {
            continue;
        }

        /* remove this pointer from the rcache and decrement its reference count
           (so it is detached later) */
        rc = rcache->rcache_delete (rcache, regs[i]);
        if (OPAL_UNLIKELY(0 != rc)) {
            /* someone beat us to it? */
            break;
        }

        /* start the new segment from the lower of the two bases */
        base = (uintptr_t) regs[i]->base < base ? (uintptr_t) regs[i]->base : base;                        

        opal_atomic_add (&regs[i]->ref_count, -1);

        if (OPAL_LIKELY(0 == regs[i]->ref_count)) {
            /* this pointer is not in use */
            (void) xpmem_detach (regs[i]->alloc_base);
            OBJ_RELEASE(regs[i]);
        }

        break;
    }

    reg = OBJ_NEW(mca_mpool_base_registration_t);
    if (OPAL_LIKELY(NULL != reg)) {
        /* stick around for awhile */
        reg->ref_count = 2;
        reg->base  = (unsigned char *) base;
        reg->bound = (unsigned char *) bound;
        reg->flags = flags;
        
        xpmem_addr.apid   = endpoint->apid;
        xpmem_addr.offset = base;

        reg->alloc_base = xpmem_attach (xpmem_addr, bound - base, NULL);
        if (OPAL_UNLIKELY((void *)-1 == reg->alloc_base)) {
            OBJ_RELEASE(reg);
            reg = NULL;
        } else {
            rcache->rcache_insert (rcache, reg, 0);
        }
    }

    return reg;
}

void vader_return_registration (mca_mpool_base_registration_t *reg, struct mca_btl_base_endpoint_t *endpoint)
{
    struct mca_rcache_base_module_t *rcache = endpoint->rcache;

    opal_atomic_add (&reg->ref_count, -1);
    if (OPAL_UNLIKELY(0 == reg->ref_count && !(reg->flags & MCA_MPOOL_FLAGS_PERSIST))) {
        rcache->rcache_delete (rcache, reg);
        (void)xpmem_detach (reg->alloc_base);
        OBJ_RELEASE (reg);
    }
}

void *vader_reg_to_ptr (mca_mpool_base_registration_t *reg, void *rem_ptr)
{
    return (void *) ((uintptr_t) reg->alloc_base +
                     (ptrdiff_t)((uintptr_t) rem_ptr - (uintptr_t) reg->base));
}
