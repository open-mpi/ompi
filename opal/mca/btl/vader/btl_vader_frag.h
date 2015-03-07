/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_VADER_SEND_FRAG_H
#define MCA_BTL_VADER_SEND_FRAG_H

#include "opal_config.h"

enum {
    MCA_BTL_VADER_FLAG_INLINE      = 0,
    MCA_BTL_VADER_FLAG_SINGLE_COPY = 1,
    MCA_BTL_VADER_FLAG_COMPLETE    = 2,
    MCA_BTL_VADER_FLAG_SETUP_FBOX  = 4,
};

struct mca_btl_vader_frag_t;
struct mca_btl_vader_fbox_t;

/**
 * FIFO fragment header
 */
struct mca_btl_vader_hdr_t {
    /** next item in fifo. many peers may touch this */
    volatile intptr_t next;
    /** pointer back the the fragment */
    struct mca_btl_vader_frag_t *frag;
    /** tag associated with this fragment (used to lookup callback) */
    mca_btl_base_tag_t tag;
    /** vader send flags (inline, complete, setup fbox, etc) */
    uint8_t flags;
    /** length of data following this header */
    int32_t len;
    /** io vector containing pointer to single-copy data */
    struct iovec sc_iov;
    /** if the fragment indicates to setup a fast box the base is stored here */
    intptr_t fbox_base;
};
typedef struct mca_btl_vader_hdr_t mca_btl_vader_hdr_t;

/**
 * shared memory send fragment derived type.
 */
struct mca_btl_vader_frag_t {
    /** base object */
    mca_btl_base_descriptor_t base;
    /** storage for segment data (max 2) */
    mca_btl_base_segment_t segments[2];
    /** endpoint this fragment is active on */
    struct mca_btl_base_endpoint_t *endpoint;
    /** fast box in use (or NULL) */
    unsigned char * restrict fbox;
    /** fragment header (in the shared memory region) */
    mca_btl_vader_hdr_t *hdr;
    /** free list this fragment was allocated within */
    opal_free_list_t *my_list;
};

typedef struct mca_btl_vader_frag_t mca_btl_vader_frag_t;

static inline int mca_btl_vader_frag_alloc (mca_btl_vader_frag_t **frag, opal_free_list_t *list,
                                            struct mca_btl_base_endpoint_t *endpoint) {
    *frag = (mca_btl_vader_frag_t *) opal_free_list_get (list);
    if (OPAL_LIKELY(NULL != *frag)) {
        (*frag)->endpoint = endpoint;
    }

    return OPAL_SUCCESS;
}

static inline void mca_btl_vader_frag_return (mca_btl_vader_frag_t *frag)
{
    if (frag->hdr) {
        frag->hdr->flags = 0;
    }

    frag->segments[0].seg_addr.pval = (char *)(frag->hdr + 1);
    frag->base.des_segment_count = 1;
    frag->fbox = NULL;

    opal_free_list_return (frag->my_list, (opal_free_list_item_t *)frag);
}

OBJ_CLASS_DECLARATION(mca_btl_vader_frag_t);

#define MCA_BTL_VADER_FRAG_ALLOC_EAGER(frag, endpoint)                  \
    mca_btl_vader_frag_alloc (&(frag), &mca_btl_vader_component.vader_frags_eager, endpoint)

#define MCA_BTL_VADER_FRAG_ALLOC_MAX(frag, endpoint)                    \
    mca_btl_vader_frag_alloc (&(frag), &mca_btl_vader_component.vader_frags_max_send, endpoint)

#define MCA_BTL_VADER_FRAG_ALLOC_USER(frag, endpoint)                   \
    mca_btl_vader_frag_alloc (&(frag), &mca_btl_vader_component.vader_frags_user, endpoint)

#define MCA_BTL_VADER_FRAG_RETURN(frag) mca_btl_vader_frag_return(frag)


static inline void mca_btl_vader_frag_complete (mca_btl_vader_frag_t *frag) {
    if (OPAL_UNLIKELY(MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags)) {
        /* completion callback */
        frag->base.des_cbfunc (&mca_btl_vader.super, frag->endpoint, &frag->base, OPAL_SUCCESS);
    }

    if (OPAL_LIKELY(frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
        MCA_BTL_VADER_FRAG_RETURN(frag);
    }
}

int mca_btl_vader_frag_init (opal_free_list_item_t *item, void *ctx);

#endif /* MCA_BTL_VADER_SEND_FRAG_H */
