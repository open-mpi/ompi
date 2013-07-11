/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  
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
#include "btl_vader_xpmem.h"

/* these hard-coded settings are based on the ideal setup for an Opteron 61xx chip and
 * may need to be adjusted for other systems. adding an MCA variable is possible but
 * can cost 20-40 ns on the fast path. this size is limited to 256 maximum bytes */
#define MCA_BTL_VADER_FBOX_SIZE 64
/* there should be a power of two number of fast boxes to simplify the math in the
 * critical path */
#define MCA_BTL_VADER_LAST_FBOX 63
/* two bytes are reserved for tag and size */
#define MCA_BTL_VADER_FBOX_MAX_SIZE (MCA_BTL_VADER_FBOX_SIZE - 2)
/* total size of all the fast boxes assigned to a particular peer */
#define MCA_BTL_VADER_FBOX_PEER_SIZE (MCA_BTL_VADER_FBOX_SIZE * (MCA_BTL_VADER_LAST_FBOX + 1))

enum {MCA_BTL_VADER_FBOX_FREE = 0, MCA_BTL_VADER_FBOX_RESERVED = 0x80};

#define MCA_BTL_VADER_FBOX_OUT_PTR(ep, fbox) ((ep)->fbox_out + MCA_BTL_VADER_FBOX_SIZE * (fbox))
#define MCA_BTL_VADER_FBOX_IN_PTR(ep, fbox) ((ep)->fbox_in + MCA_BTL_VADER_FBOX_SIZE * (fbox))
#define MCA_BTL_VADER_NEXT_FBOX(fbox) (((fbox) + 1) & MCA_BTL_VADER_LAST_FBOX)

static inline unsigned char * restrict mca_btl_vader_reserve_fbox (struct mca_btl_base_endpoint_t *ep, const size_t size)
{
    const int next_fbox = ep->next_fbox_out;
    unsigned char * restrict fbox = (unsigned char * restrict) MCA_BTL_VADER_FBOX_OUT_PTR(ep, next_fbox);  

    /* todo -- need thread locks/atomics here for the multi-threaded case */
    if (OPAL_LIKELY(size <= MCA_BTL_VADER_FBOX_MAX_SIZE && fbox[0] == MCA_BTL_VADER_FBOX_FREE)) {
        /* mark this fast box as in use */
        fbox[0] = MCA_BTL_VADER_FBOX_RESERVED;

        ep->next_fbox_out = MCA_BTL_VADER_NEXT_FBOX(next_fbox);
        return fbox + 2;
    } else if (OPAL_LIKELY(size <= (MCA_BTL_VADER_FBOX_MAX_SIZE + MCA_BTL_VADER_FBOX_SIZE) && MCA_BTL_VADER_LAST_FBOX != next_fbox &&
                           MCA_BTL_VADER_FBOX_FREE == fbox[0] && MCA_BTL_VADER_FBOX_FREE == fbox[MCA_BTL_VADER_FBOX_SIZE])) {
        /* aggregate two fast boxes */
        fbox[0] = MCA_BTL_VADER_FBOX_RESERVED;
        ep->next_fbox_out = MCA_BTL_VADER_NEXT_FBOX(next_fbox + 1);
        return fbox + 2;
    }

    return NULL;
}

static inline void mca_btl_vader_fbox_send (unsigned char * restrict fbox, unsigned char tag,
                                            size_t size)
{
    fbox[-1] = tag;

    /* ensure data writes have completed before we mark the data as available */
    opal_atomic_wmb ();

    fbox[-2] = size;
}

static inline int mca_btl_vader_fbox_sendi (struct mca_btl_base_endpoint_t *endpoint, char tag,
                                            void * restrict header, const size_t header_size,
                                            void * restrict payload, const size_t payload_size)
{
    unsigned char * restrict fbox;

    fbox = mca_btl_vader_reserve_fbox(endpoint, header_size + payload_size);
    if (OPAL_UNLIKELY(NULL == fbox)) {
        return 0;
    }

    memmove (fbox, header, header_size);
    if (OPAL_UNLIKELY(payload)) {
        /* inline sends are typically just pml headers (due to MCA_BTL_FLAGS_SEND_INPLACE) */
        memmove (fbox + header_size, payload, payload_size);
    }

    /* mark the fbox as sent */
    mca_btl_vader_fbox_send (fbox, tag, header_size + payload_size);

    /* send complete */
    return 1;
}

static inline void mca_btl_vader_check_fboxes (void)
{
    mca_btl_vader_frag_t frag = {.base = {.des_dst = frag.segments, .des_dst_cnt = 1}};
    const int num_smp_procs = MCA_BTL_VADER_NUM_LOCAL_PEERS + 1;
    const mca_btl_active_message_callback_t *reg;
    struct mca_btl_base_endpoint_t *endpoint;
    unsigned char * restrict fbox;
    int i, next_fbox;

    for (i = 0, endpoint = mca_btl_vader_component.endpoints ; i < num_smp_procs ; ++i, ++endpoint) {
        if (NULL == endpoint->fbox_in) {
            continue;
        }

        next_fbox = endpoint->next_fbox_in;
        fbox = (unsigned char *) MCA_BTL_VADER_FBOX_IN_PTR(endpoint, next_fbox);

        /* process all fast-box messages */
        while ((frag.segments[0].seg_len = fbox[0]) & 0x7f) {
            const unsigned char tag = fbox[1];

            opal_atomic_rmb ();

            reg = mca_btl_base_active_message_trigger + tag;

            frag.segments[0].seg_addr.pval = fbox + 2;

            reg->cbfunc(&mca_btl_vader.super, tag, &(frag.base), reg->cbdata);

            if (fbox[0] > MCA_BTL_VADER_FBOX_MAX_SIZE) {
                fbox[MCA_BTL_VADER_FBOX_SIZE] = MCA_BTL_VADER_FBOX_FREE;
                ++next_fbox;
            }
            fbox[0] = MCA_BTL_VADER_FBOX_FREE;

            next_fbox = MCA_BTL_VADER_NEXT_FBOX(next_fbox);
            fbox = (unsigned char *) MCA_BTL_VADER_FBOX_IN_PTR(endpoint, next_fbox);
        }

        endpoint->next_fbox_in = next_fbox;
    }
}

#endif /* !defined(MCA_BTL_VADER_FBOX_H) */








