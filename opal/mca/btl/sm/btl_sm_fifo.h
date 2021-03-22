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
 * Copyright (c) 2010-2018 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_SM_FIFO_H
#define MCA_BTL_SM_FIFO_H

#include "opal_config.h"

#include "opal/mca/btl/sm/btl_sm_fbox.h"
#include "opal/mca/btl/sm/btl_sm_types.h"
#include "opal/mca/btl/sm/btl_sm_virtual.h"

#define sm_item_compare_exchange(x, y, z)                                                   \
    opal_atomic_compare_exchange_strong_ptr((opal_atomic_intptr_t *) (x), (intptr_t *) (y), \
                                            (intptr_t)(z))

#define SM_FIFO_FREE ((fifo_value_t) -2)

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

/* large enough to ensure the fifo is on its own cache line */
#define MCA_BTL_SM_FIFO_SIZE 128

/**
 * sm_fifo_read:
 *
 * @brief reads a single fragment from a local fifo
 *
 * @param[inout]   fifo - FIFO to read from
 * @param[out]     ep   - returns the endpoint the fifo element was read from
 *
 * @returns a fragment header or NULL
 *
 * This function does not currently support multiple readers.
 */
static inline mca_btl_sm_hdr_t *sm_fifo_read(sm_fifo_t *fifo, struct mca_btl_base_endpoint_t **ep)
{
    mca_btl_sm_hdr_t *hdr;
    fifo_value_t value;

    if (SM_FIFO_FREE == fifo->fifo_head) {
        return NULL;
    }

    opal_atomic_rmb();

    value = fifo->fifo_head;

    *ep = &mca_btl_sm_component.endpoints[value >> MCA_BTL_SM_OFFSET_BITS];
    hdr = (mca_btl_sm_hdr_t *) relative2virtual(value);

    fifo->fifo_head = SM_FIFO_FREE;

    assert(hdr->next != value);

    if (OPAL_UNLIKELY(SM_FIFO_FREE == hdr->next)) {
        opal_atomic_rmb();

        if (!sm_item_compare_exchange(&fifo->fifo_tail, &value, SM_FIFO_FREE)) {
            while (SM_FIFO_FREE == hdr->next) {
                opal_atomic_rmb();
            }

            fifo->fifo_head = hdr->next;
        }
    } else {
        fifo->fifo_head = hdr->next;
    }

    opal_atomic_wmb();
    return hdr;
}

static inline void sm_fifo_init(sm_fifo_t *fifo)
{
    /* due to a compiler bug in Oracle C 5.15 the following line was broken into two. Not
     * ideal but oh well. See #5814 */
    /* fifo->fifo_head = fifo->fifo_tail = SM_FIFO_FREE; */
    fifo->fifo_head = SM_FIFO_FREE;
    fifo->fifo_tail = SM_FIFO_FREE;
    fifo->fbox_available = mca_btl_sm_component.fbox_max;
    mca_btl_sm_component.my_fifo = fifo;
}

static inline void sm_fifo_write(sm_fifo_t *fifo, fifo_value_t value)
{
    fifo_value_t prev;

    opal_atomic_wmb();
    prev = opal_atomic_swap_ptr(&fifo->fifo_tail, value);
    opal_atomic_rmb();

    assert(prev != value);

    if (OPAL_LIKELY(SM_FIFO_FREE != prev)) {
        mca_btl_sm_hdr_t *hdr = (mca_btl_sm_hdr_t *) relative2virtual(prev);
        hdr->next = value;
    } else {
        fifo->fifo_head = value;
    }

    opal_atomic_wmb();
}

/**
 * sm_fifo_write_ep:
 *
 * @brief write a frag (relative to this process' base) to another rank's fifo
 *
 * @param[in]  hdr - fragment header to write
 * @param[in]  ep  - endpoint to write the fragment to
 *
 * This function is used to send a fragment to a remote peer. {hdr} must belong
 * to the current process.
 */
static inline bool sm_fifo_write_ep(mca_btl_sm_hdr_t *hdr, struct mca_btl_base_endpoint_t *ep)
{
    fifo_value_t rhdr = virtual2relative((char *) hdr);
    if (ep->fbox_out.buffer) {
        /* if there is a fast box for this peer then use the fast box to send the fragment header.
         * this is done to ensure fragment ordering */
        opal_atomic_wmb();
        return mca_btl_sm_fbox_sendi(ep, 0xfe, &rhdr, sizeof(rhdr), NULL, 0);
    }
    mca_btl_sm_try_fbox_setup(ep, hdr);
    hdr->next = SM_FIFO_FREE;
    sm_fifo_write(ep->fifo, rhdr);

    return true;
}

/**
 * sm_fifo_write_back:
 *
 * @brief write a frag (relative to the remote process' base) to the remote fifo
 *
 * @param[in]  hdr - fragment header to write
 * @param[in]  ep  - endpoint the fragment belongs to
 *
 * This function is used to return a fragment to the sending process. It differs from
 * sm_fifo_write_ep in that it uses the {ep} to produce the relative address.
 */
static inline void sm_fifo_write_back(mca_btl_sm_hdr_t *hdr, struct mca_btl_base_endpoint_t *ep)
{
    hdr->next = SM_FIFO_FREE;
    sm_fifo_write(ep->fifo, virtual2relativepeer(ep, (char *) hdr));
}

#endif /* MCA_BTL_SM_FIFO_H */
