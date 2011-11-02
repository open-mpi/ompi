/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011      Los Alamos National Security, LLC.  
 *                         All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MCA_BTL_VADER_FBOX_H)
#define MCA_BTL_VADER_FBOX_H

#include "btl_vader.h"
#include "btl_vader_endpoint.h"

/* XXX -- FIXME -- make no assumptions if possible */
/* Assumptions: page size: 4096, cache line: 64 or 128 bytes, tag = 1 byte */
#define FBOX_SIZE 128 /* 2-4 cache lines */
#define LAST_FBOX 31  /* page size assumtion: 4096 */
#define MAX_MSG   126 /* 1 byte used each for size and tag */

enum {MCA_BTL_VADER_FBOX_FREE = 0xfe, MCA_BTL_VADER_FBOX_RESERVED = 0xff};

#define MCA_BTL_VADER_FBOX_OUT_PTR(peer_smp_rank, fbox)                        \
    (mca_btl_vader_component.vader_fboxes_out[peer_smp_rank] + FBOX_SIZE * (fbox))

#define MCA_BTL_VADER_FBOX_IN_PTR(peer_smp_rank, fbox)                        \
    (mca_btl_vader_component.vader_fboxes_in[peer_smp_rank] + FBOX_SIZE * (fbox))

static inline unsigned char *mca_btl_vader_reserve_fbox (int peer_smp_rank, size_t size)
{
    int next_fbox = mca_btl_vader_component.vader_next_fbox_out[peer_smp_rank];
    unsigned char *fbox = MCA_BTL_VADER_FBOX_OUT_PTR(peer_smp_rank, next_fbox);  

    /* todo -- need thread locks here for the multi-threaded case */

    if (size > MAX_MSG || fbox[0] != MCA_BTL_VADER_FBOX_FREE) {
        /* fall back on fifo */
        return NULL;
    }

    mca_btl_vader_component.vader_next_fbox_out[peer_smp_rank] =
        next_fbox == LAST_FBOX ? 0 : next_fbox + 1;

    /* mark this fast box as in use */
    fbox[0] = MCA_BTL_VADER_FBOX_RESERVED;

    return fbox + 2;
}

static inline void mca_btl_vader_fbox_send (unsigned char *fbox, unsigned char tag, size_t size)
{
    fbox[-1] = tag;

    /* ensure data writes have completed before we mark the data as available */
    opal_atomic_wmb ();

    fbox[-2] = size;
}

static inline int mca_btl_vader_fbox_sendi (struct mca_btl_base_endpoint_t *endpoint, char tag,
                                            void *header, size_t header_size,
                                            void *payload, size_t payload_size)
{
    unsigned char *fbox;

    fbox = mca_btl_vader_reserve_fbox(endpoint->peer_smp_rank, header_size + payload_size);
    if (NULL == fbox) {
        return 0;
    }

    memcpy (fbox, header, header_size);
    if (OPAL_UNLIKELY(payload)) {
        /* inline sends are typically just pml headers (due to MCA_BTL_FLAGS_SEND_INPLACE) */
        memcpy (fbox + header_size, payload, payload_size);
    }

    /* mark the fbox as sent */
    mca_btl_vader_fbox_send (fbox, tag, header_size + payload_size);

    /* send complete */
    return 1;
}

static inline void mca_btl_vader_check_fboxes (void)
{
    int my_smp_rank = mca_btl_vader_component.my_smp_rank;
    mca_btl_active_message_callback_t *reg;
    mca_btl_vader_frag_t frag;
    unsigned char size, tag;
    int i;

    for (i = 0 ; i < mca_btl_vader_component.num_smp_procs ; ++i) {
        int next_fbox = mca_btl_vader_component.vader_next_fbox_in[i];
        unsigned char *fbox = MCA_BTL_VADER_FBOX_IN_PTR(i, next_fbox);

        if (my_smp_rank == i) {
            continue;
        }

        /* process all fast-box messages */
        while (0xfe != ((size = fbox[0]) & 0xfe)) {
            opal_atomic_rmb ();

            tag = fbox[1];

            reg = mca_btl_base_active_message_trigger + tag;

            frag.segment.seg_addr.pval = fbox + 2;
            frag.segment.seg_len       = size;

            frag.base.des_dst     = &frag.segment;
            frag.base.des_dst_cnt = 1;
            reg->cbfunc(&mca_btl_vader.super, tag, &(frag.base), reg->cbdata);

            fbox[0] = MCA_BTL_VADER_FBOX_FREE;

            next_fbox = next_fbox == LAST_FBOX ? 0 : next_fbox + 1;
            fbox = MCA_BTL_VADER_FBOX_IN_PTR(i, next_fbox);
        }

        mca_btl_vader_component.vader_next_fbox_in[i] = next_fbox;
    }
}

#endif /* !defined(MCA_BTL_VADER_FBOX_H) */
