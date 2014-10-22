/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2012-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */

#ifndef MCA_BTL_VADER_ENDPOINT_H
#define MCA_BTL_VADER_ENDPOINT_H

#include "ompi_config.h"
#include "btl_vader_xpmem.h"

#define MCA_BTL_VADER_FBOX_ALIGNMENT      32
#define MCA_BTL_VADER_FBOX_ALIGNMENT_MASK (MCA_BTL_VADER_FBOX_ALIGNMENT - 1)

struct vader_fifo_t;

/**
 *  An abstraction that represents a connection to a endpoint process.
 *  An instance of mca_ptl_base_endpoint_t is associated w/ each process
 *  and BTL pair at startup.
 */

struct mca_btl_vader_fbox_t;

typedef struct mca_btl_base_endpoint_t {
    opal_list_item_t super;

    /* per peer buffers */
    struct {
        unsigned char *buffer; /**< starting address of peer's fast box out */
        unsigned int start, seq;
        uint32_t *startp;
    } fbox_in;

    struct {
        unsigned char *buffer; /**< starting address of peer's fast box in */
        unsigned int start, end, seq;
        uint32_t *startp;      /**< pointer to location storing start offset */
    } fbox_out;

    int32_t peer_smp_rank;  /**< my peer's SMP process rank.  Used for accessing
                             *   SMP specfic data structures. */
    uint32_t send_count;    /**< number of fragments sent to this peer */
    char *segment_base;     /**< start of the peer's segment (in the address space
                             *   of this process) */

    struct vader_fifo_t *fifo; /**< */

    opal_mutex_t lock;      /**< lock to protect endpoint structures from concurrent
                             *   access */

    union {
#if OMPI_BTL_VADER_HAVE_XPMEM
        struct {
            struct mca_rcache_base_module_t *rcache;
            xpmem_apid_t    apid;       /**< xpmem apid for remote peer */
        } xpmem;
#endif
        struct {
            pid_t           pid;        /**< pid of remote peer (used for CMA) */
            opal_shmem_ds_t *seg_ds;    /**< stored segment information for detach */
        } other;
    } segment_data;

    opal_list_t pending_frags; /**< fragments pending fast box space */
    bool waiting;           /**< endpoint is on the component wait list */
} mca_btl_base_endpoint_t;

typedef mca_btl_base_endpoint_t mca_btl_vader_endpoint_t;

OBJ_CLASS_DECLARATION(mca_btl_vader_endpoint_t);

static inline void mca_btl_vader_endpoint_setup_fbox_recv (struct mca_btl_base_endpoint_t *endpoint, void *base)
{
    endpoint->fbox_in.buffer = base;
    endpoint->fbox_in.startp = (uint32_t *) base;
    endpoint->fbox_in.start = MCA_BTL_VADER_FBOX_ALIGNMENT;
    endpoint->fbox_in.seq = 0;
}

static inline void mca_btl_vader_endpoint_setup_fbox_send (struct mca_btl_base_endpoint_t *endpoint, void *base)
{
    endpoint->fbox_out.buffer = base;
    endpoint->fbox_out.start = MCA_BTL_VADER_FBOX_ALIGNMENT;
    endpoint->fbox_out.end = MCA_BTL_VADER_FBOX_ALIGNMENT;
    endpoint->fbox_out.startp = (uint32_t *) base;
    endpoint->fbox_out.startp[0] = MCA_BTL_VADER_FBOX_ALIGNMENT;
    endpoint->fbox_out.seq = 0;

    /* zero out the first header in the fast box */
    memset ((char *) base + MCA_BTL_VADER_FBOX_ALIGNMENT, 0, MCA_BTL_VADER_FBOX_ALIGNMENT);
}

#endif /* MCA_BTL_VADER_ENDPOINT_H */
