/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.  
 *                         All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_VADER_H
#define MCA_BTL_VADER_H

#include "ompi_config.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif  /* HAVE_STDINT_H */
#ifdef HAVE_SCHED_H
# include <sched.h>
#endif  /* HAVE_SCHED_H */
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif /* HAVE_UNISTD_H */

/* xpmem is required by vader atm */
#include <xpmem.h>

#include "opal/include/opal/align.h"
#include "opal/class/opal_free_list.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/btl/btl.h"

#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/sm/mpool_sm.h"
#include "ompi/mca/common/sm/common_sm.h"

#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"


BEGIN_C_DECLS

#define min(a,b) ((a) < (b) ? (a) : (b))

extern int mca_btl_vader_memcpy_limit;
extern int mca_btl_vader_log_align;
extern int mca_btl_vader_max_inline_send;

#define VADER_FIFO_FREE  (void *) (-2)
/* We can't use opal_cache_line_size here because we need a
   compile-time constant for padding the struct.  We can't really have
   a compile-time constant that is portable, either (e.g., compile on
   one machine and run on another).  So just use a big enough cache
   line that should hopefully be good in most places. */
#define VADER_CACHE_LINE_PAD 128

/* largest address we can attach to using xpmem */
#define VADER_MAX_ADDRESS ((uintptr_t)0x7ffffffff000)

/*
 * Shared Memory resource managment
 */

struct vader_fifo_t;

/**
 * Shared Memory (VADER) BTL module.
 */
struct mca_btl_vader_component_t {
    mca_btl_base_component_2_0_0_t super;   /**< base BTL component */
    int vader_free_list_num;                /**< initial size of free lists */
    int vader_free_list_max;                /**< maximum size of free lists */
    int vader_free_list_inc;                /**< number of elements to alloc
                                             * when growing free lists */
    char *vader_mpool_name;                 /**< name of shared memory pool module */
    mca_mpool_base_module_t *vader_mpool;   /**< mpool on local node */
    void *vader_mpool_base;                 /**< base address of shared memory pool */
    size_t eager_limit;                     /**< send fragment size */
    mca_common_sm_module_t *vader_seg;      /**< description of shared memory segment */
    volatile struct vader_fifo_t **shm_fifo;/**< pointer to fifo 2D array in
                                             * shared memory */
    char **shm_bases;                       /**< pointer to base pointers in
                                             * shared memory */
    xpmem_segid_t *shm_seg_ids;             /* xpmem segment ids */
    struct vader_fifo_t **fifo;             /**< cached copy of the pointer to
                                             * the 2D fifo array. */
    struct mca_rcache_base_module_t **xpmem_rcaches;
    xpmem_apid_t *apids;                    /* xpmem apids */
    int32_t num_smp_procs;                  /**< current number of smp procs on this host */
    int32_t my_smp_rank;                    /**< My SMP process rank.  Used for accessing
                                             * SMP specfic data structures. */
    ompi_free_list_t vader_frags_eager;     /**< free list of vader send frags */
    ompi_free_list_t vader_frags_user;      /**< free list of vader put/get frags */

    opal_list_t active_sends;               /**< list of outstanding fragments */

    char **vader_fboxes_in;                 /**< incomming fast boxes (memory belongs to this process) */
    char **vader_fboxes_out;                /**< outgoing fast boxes (memory belongs to remote peers) */

    unsigned char *vader_next_fbox_in;      /**< indices of fast boxes to poll */
    unsigned char *vader_next_fbox_out;     /**< indices of fast boxes to write */

    struct mca_btl_base_endpoint_t **vader_peers;
};
typedef struct mca_btl_vader_component_t mca_btl_vader_component_t;
OMPI_MODULE_DECLSPEC extern mca_btl_vader_component_t mca_btl_vader_component;

/**
 * VADER BTL Interface
 */
struct mca_btl_vader_t {
    mca_btl_base_module_t  super;       /**< base BTL interface */
    bool btl_inited;  /**< flag indicating if btl has been inited */
    mca_btl_base_module_error_cb_fn_t error_cb;
};
typedef struct mca_btl_vader_t mca_btl_vader_t;
OMPI_MODULE_DECLSPEC extern mca_btl_vader_t mca_btl_vader;

/***
 * One or more FIFO components may be a pointer that must be
 * accessed by multiple processes.  Since the shared region may
 * be mmapped differently into each process's address space,
 * these pointers will be relative to some base address.  Here,
 * we define macros to translate between relative addresses and
 * virtual addresses.
 */
#define VIRTUAL2RELATIVE(VADDR ) ((long)(VADDR)  - (long)mca_btl_vader_component.shm_bases[mca_btl_vader_component.my_smp_rank])
#define RELATIVE2VIRTUAL(OFFSET) ((long)(OFFSET) + (long)mca_btl_vader_component.shm_bases[mca_btl_vader_component.my_smp_rank])

/* look up the remote pointer in the peer rcache and attach if
 * necessary */
static inline mca_mpool_base_registration_t *vader_get_registation (int peer_smp_rank, void *rem_ptr,
                                                                    size_t size, int flags)
{
    struct mca_rcache_base_module_t *rcache = mca_btl_vader_component.xpmem_rcaches[peer_smp_rank];
    mca_mpool_base_registration_t *regs[10], *reg = NULL;
    struct xpmem_addr xpmem_addr;
    uintptr_t base, bound;
    int rc, i;

    if (OPAL_UNLIKELY(peer_smp_rank == mca_btl_vader_component.my_smp_rank)) {
        return rem_ptr;
    }

    base = (uintptr_t) down_align_addr(rem_ptr, mca_btl_vader_log_align);
    bound = (uintptr_t) up_align_addr((void *)((uintptr_t) rem_ptr + size - 1),
                                      mca_btl_vader_log_align) + 1;
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
        
        xpmem_addr.apid   = mca_btl_vader_component.apids[peer_smp_rank];
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

static inline void vader_return_registration (mca_mpool_base_registration_t *reg, int peer_smp_rank)
{
    struct mca_rcache_base_module_t *rcache = mca_btl_vader_component.xpmem_rcaches[peer_smp_rank];

    opal_atomic_add (&reg->ref_count, -1);
    if (OPAL_UNLIKELY(0 == reg->ref_count && !(reg->flags & MCA_MPOOL_FLAGS_PERSIST))) {
        rcache->rcache_delete (rcache, reg);
        (void)xpmem_detach (reg->alloc_base);
        OBJ_RELEASE (reg);
    }
}

static inline void *vader_reg_to_ptr (mca_mpool_base_registration_t *reg, void *rem_ptr)
{
    return (void *) ((uintptr_t) reg->alloc_base +
                     (ptrdiff_t)((uintptr_t) rem_ptr - (uintptr_t) reg->base));
}

/* memcpy is faster at larger sizes but is undefined if the
   pointers are aliased (TODO -- readd alias check) */
static inline void vader_memmove (void *dst, void *src, size_t size)
{
    if (size >= mca_btl_vader_memcpy_limit) {
        memcpy (dst, src, size);
    } else {
        memmove (dst, src, size);
    }
}

/**
 * Initiate a send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
int mca_btl_vader_send(struct mca_btl_base_module_t *btl,
                       struct mca_btl_base_endpoint_t *endpoint,
                       struct mca_btl_base_descriptor_t *descriptor,
                       mca_btl_base_tag_t tag);

/**
 * Initiate an inline send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
int mca_btl_vader_sendi (struct mca_btl_base_module_t *btl,
                         struct mca_btl_base_endpoint_t *endpoint,
                         struct opal_convertor_t *convertor,
                         void *header, size_t header_size,
                         size_t payload_size, uint8_t order,
                         uint32_t flags, mca_btl_base_tag_t tag,
                         mca_btl_base_descriptor_t **descriptor);

/**
 * Initiate an synchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
int mca_btl_vader_put (struct mca_btl_base_module_t *btl,
                       struct mca_btl_base_endpoint_t *endpoint,
                       struct mca_btl_base_descriptor_t *des);

/**
 * Initiate an synchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
int mca_btl_vader_get (struct mca_btl_base_module_t *btl,
                       struct mca_btl_base_endpoint_t *endpoint,
                       struct mca_btl_base_descriptor_t *des);

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
mca_btl_base_descriptor_t* mca_btl_vader_alloc (struct mca_btl_base_module_t* btl,
                                                struct mca_btl_base_endpoint_t* endpoint,
                                                uint8_t order, size_t size, uint32_t flags);


END_C_DECLS

#endif
