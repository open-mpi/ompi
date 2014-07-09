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
 * Copyright (c) 2010-2014 Los Alamos National Security, LLC.
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
#ifndef MCA_BTL_VADER_FIFO_H
#define MCA_BTL_VADER_FIFO_H

#include "btl_vader.h"
#include "btl_vader_endpoint.h"
#include "btl_vader_frag.h"

#if SIZEOF_VOID_P == 8
  #define vader_item_cmpset(x, y, z) opal_atomic_cmpset_64((volatile int64_t *)(x), (int64_t)(y), (int64_t)(z))
  #define vader_item_swap(x, y)      opal_atomic_swap_64((volatile int64_t *)(x), (int64_t)(y))

  #define MCA_BTL_VADER_OFFSET_MASK 0xffffffffll
  #define MCA_BTL_VADER_OFFSET_BITS 32
  #define MCA_BTL_VADER_BITNESS     64

  typedef int64_t fifo_value_t;
#else
  #define vader_item_cmpset(x, y, z) opal_atomic_cmpset_32((volatile int32_t *)(x), (int32_t)(y), (int32_t)(z))
  #define vader_item_swap(x, y)      opal_atomic_swap_32((volatile int32_t *)(x), (int32_t)(y))

  #define MCA_BTL_VADER_OFFSET_MASK 0x00ffffffl
  #define MCA_BTL_VADER_OFFSET_BITS 24
  #define MCA_BTL_VADER_BITNESS     32

  typedef int32_t fifo_value_t;
#endif

#define VADER_FIFO_FREE  ((fifo_value_t)-2)

/*
 * Shared Memory FIFOs
 *
 * The FIFO is implemented as a linked list of frag headers. The fifo has multiple
 * producers and a single consumer (in the single thread case) so the tail needs
 * to be modified by an atomic or protected by a atomic lock.
 *
 * Since the frags live in shared memory that is mapped differently into
 * each address space, the head and tail pointers are relative (each process must
 * add its own offset).
 *
 * We introduce some padding at the end of the structure but it is probably unnecessary.
 */

/* lock free fifo */
typedef struct vader_fifo_t {
    volatile fifo_value_t fifo_head;
    volatile fifo_value_t fifo_tail;
} vader_fifo_t;

/* large enough to ensure the fifo is on its own cache line */
#define MCA_BTL_VADER_FIFO_SIZE 128

/* This only works for finding the relative address for a pointer within my_segment */
static inline fifo_value_t virtual2relative (char *addr)
{
    return (fifo_value_t) ((intptr_t) (addr - mca_btl_vader_component.my_segment)) | ((fifo_value_t)MCA_BTL_VADER_LOCAL_RANK << MCA_BTL_VADER_OFFSET_BITS);
}

static inline fifo_value_t virtual2relativepeer (struct mca_btl_base_endpoint_t *endpoint, char *addr)
{
    return (fifo_value_t) ((intptr_t) (addr - endpoint->segment_base)) | ((fifo_value_t)endpoint->peer_smp_rank << MCA_BTL_VADER_OFFSET_BITS);
}

static inline void *relative2virtual (fifo_value_t offset)
{
    return (void *)(intptr_t)((offset & MCA_BTL_VADER_OFFSET_MASK) + mca_btl_vader_component.endpoints[offset >> MCA_BTL_VADER_OFFSET_BITS].segment_base);
}

static inline mca_btl_vader_hdr_t *vader_fifo_read (vader_fifo_t *fifo, struct mca_btl_base_endpoint_t **ep)
{
    mca_btl_vader_hdr_t *hdr;
    fifo_value_t value;
    static volatile int32_t lock = 0;

    if (opal_atomic_swap_32 (&lock, 1)) {
        return NULL;
    }

    if (VADER_FIFO_FREE == fifo->fifo_head) {
        lock = 0;
        return NULL;
    }

    opal_atomic_rmb ();

    value = fifo->fifo_head;

    *ep = &mca_btl_vader_component.endpoints[value >> MCA_BTL_VADER_OFFSET_BITS];
    hdr = (mca_btl_vader_hdr_t *) relative2virtual (value);

    if (OPAL_UNLIKELY(!(hdr->flags & MCA_BTL_VADER_FLAG_COMPLETE) && ((*ep)->expected_sequence != hdr->seqn))) {
        lock = 0;
        return NULL;
    }

    fifo->fifo_head = VADER_FIFO_FREE;
    ++(*ep)->expected_sequence;

    assert (hdr->next != value);

    if (OPAL_UNLIKELY(VADER_FIFO_FREE == hdr->next)) {
        opal_atomic_rmb();

        if (!vader_item_cmpset (&fifo->fifo_tail, value, VADER_FIFO_FREE)) {
            while (VADER_FIFO_FREE == hdr->next) {
                opal_atomic_rmb ();
            }

            fifo->fifo_head = hdr->next;
        }
    } else {
        fifo->fifo_head = hdr->next;
    }

    opal_atomic_wmb ();
    lock = 0;
    return hdr; 
}

static inline int vader_fifo_init (vader_fifo_t *fifo)
{
    fifo->fifo_head = fifo->fifo_tail = VADER_FIFO_FREE;
    mca_btl_vader_component.my_fifo = fifo;

    return OMPI_SUCCESS;
}

static inline void vader_fifo_write (vader_fifo_t *fifo, fifo_value_t value)
{
    fifo_value_t prev;

    opal_atomic_wmb ();
    prev = vader_item_swap (&fifo->fifo_tail, value);
    opal_atomic_rmb ();

    assert (prev != value);

    if (OPAL_LIKELY(VADER_FIFO_FREE != prev)) {
        mca_btl_vader_hdr_t *hdr = (mca_btl_vader_hdr_t *) relative2virtual (prev);
        hdr->next = value;
    } else {
        fifo->fifo_head = value;
    }

    opal_atomic_wmb ();
}

/* write a frag (relative to this process' base) to another rank's fifo */
static inline void vader_fifo_write_ep (mca_btl_vader_hdr_t *hdr, struct mca_btl_base_endpoint_t *ep)
{
    hdr->next = VADER_FIFO_FREE;
    hdr->seqn = ep->next_sequence++;
    vader_fifo_write (ep->fifo, virtual2relative ((char *) hdr));
}

/* write a frag (relative to the remote process' base) to the remote fifo. note the remote peer must own hdr */
static inline void vader_fifo_write_back (mca_btl_vader_hdr_t *hdr, struct mca_btl_base_endpoint_t *ep)
{
    hdr->next = VADER_FIFO_FREE;
    vader_fifo_write(ep->fifo, virtual2relativepeer (ep, (char *) hdr));
}

#endif /* MCA_BTL_VADER_FIFO_H */
