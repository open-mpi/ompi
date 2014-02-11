/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/mca/btl/vader/btl_vader.h"
#include "opal/include/opal/align.h"
#include "btl_vader_xpmem.h"
#include "opal/mca/memchecker/base/base.h"

#if OMPI_BTL_VADER_HAVE_XPMEM

/* largest address we can attach to using xpmem */
#define VADER_MAX_ADDRESS ((uintptr_t)0x7ffffffff000)

/* look up the remote pointer in the peer rcache and attach if
 * necessary */
mca_mpool_base_registration_t *vader_get_registation (struct mca_btl_base_endpoint_t *endpoint, void *rem_ptr,
						      size_t size, int flags, void **local_ptr)
{
    struct mca_rcache_base_module_t *rcache = endpoint->rcache;
    mca_mpool_base_registration_t *regs[10], *reg = NULL;
    xpmem_addr_t xpmem_addr;
    uintptr_t base, bound;
    int rc, i;

    /* use btl/self for self communication */
    assert (endpoint->peer_smp_rank != MCA_BTL_VADER_LOCAL_RANK);

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
            reg = regs[i];
            goto reg_found;
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

#if defined(HAVE_SN_XPMEM_H)
        xpmem_addr.id     = endpoint->apid;
#else
        xpmem_addr.apid   = endpoint->apid;
#endif
        xpmem_addr.offset = base;

        reg->alloc_base = xpmem_attach (xpmem_addr, bound - base, NULL);
        if (OPAL_UNLIKELY((void *)-1 == reg->alloc_base)) {
            OBJ_RELEASE(reg);
            return NULL;
        }

        opal_memchecker_base_mem_defined (reg->alloc_base, bound - base);

        rcache->rcache_insert (rcache, reg, 0);
    }

reg_found:
    opal_atomic_wmb ();
    *local_ptr = (void *) ((uintptr_t) reg->alloc_base +
                           (ptrdiff_t)((uintptr_t) rem_ptr - (uintptr_t) reg->base));

    return reg;
}

void vader_return_registration (mca_mpool_base_registration_t *reg, struct mca_btl_base_endpoint_t *endpoint)
{
    struct mca_rcache_base_module_t *rcache = endpoint->rcache;

    opal_atomic_add (&reg->ref_count, -1);
    if (OPAL_UNLIKELY(0 == reg->ref_count && !(reg->flags & MCA_MPOOL_FLAGS_PERSIST))) {
        rcache->rcache_delete (rcache, reg);
        opal_memchecker_base_mem_noaccess (reg->alloc_base, (uintptr_t)(reg->bound - reg->base));
        (void)xpmem_detach (reg->alloc_base);
        OBJ_RELEASE (reg);
    }
}

#endif /* OMPI_BTL_VADER_HAVE_XPMEM */
