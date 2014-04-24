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

#if !defined(MCA_BTL_VADER_FBOX_H)
#define MCA_BTL_VADER_FBOX_H

#include "btl_vader.h"
#include "btl_vader_endpoint.h"
#include "btl_vader_xpmem.h"

#include <string.h>

/* these hard-coded settings are based on the ideal setup for an Opteron 61xx chip and
 * may need to be adjusted for other systems. adding an MCA variable is possible but
 * can cost 20-40 ns on the fast path. this size is limited to 256 maximum bytes */
#define MCA_BTL_VADER_FBOX_SIZE 64
/* there should be a power of two number of fast boxes to simplify the math in the
 * critical path */
#define MCA_BTL_VADER_LAST_FBOX 63
#define MCA_BTL_VADER_POLL_COUNT 31
/* two bytes are reserved for tag and size (update if the header is modified) */
#define MCA_BTL_VADER_FBOX_HDR_SIZE 4
#define MCA_BTL_VADER_FBOX_MAX_SIZE (MCA_BTL_VADER_FBOX_SIZE - MCA_BTL_VADER_FBOX_HDR_SIZE)
/* total size of all the fast boxes assigned to a particular peer */
#define MCA_BTL_VADER_FBOX_PEER_SIZE (MCA_BTL_VADER_FBOX_SIZE * (MCA_BTL_VADER_LAST_FBOX + 1))

typedef struct mca_btl_vader_fbox_t {
    union {
        struct {
            uint8_t  size;
            uint8_t  tag;
            uint16_t seqn;
        } hdr_data;
        uint32_t ival;
    } hdr;

    uint8_t data[MCA_BTL_VADER_FBOX_MAX_SIZE];
} mca_btl_vader_fbox_t;

#define MCA_BTL_VADER_FBOX_OUT_PTR(ep, fbox) ((ep)->fbox_out + (fbox))
#define MCA_BTL_VADER_FBOX_IN_PTR(ep, fbox) ((ep)->fbox_in + (fbox))
#define MCA_BTL_VADER_NEXT_FBOX(fbox) (((fbox) + 1) & MCA_BTL_VADER_LAST_FBOX)

static inline mca_btl_vader_fbox_t * restrict mca_btl_vader_reserve_fbox (struct mca_btl_base_endpoint_t *ep, const size_t size)
{
    const int next_fbox = ep->next_fbox_out;
    mca_btl_vader_fbox_t * restrict fbox = MCA_BTL_VADER_FBOX_OUT_PTR(ep, next_fbox);

    opal_atomic_mb ();

    /* todo -- need thread locks/atomics here for the multi-threaded case */
    if (OPAL_LIKELY(size <= MCA_BTL_VADER_FBOX_MAX_SIZE && 0 == fbox->hdr.ival)) {
        /* mark this fast box as in use */
        fbox->hdr.hdr_data.size = size;
        ep->next_fbox_out = MCA_BTL_VADER_NEXT_FBOX(next_fbox);
        opal_atomic_mb ();
        return fbox;
    }

    return NULL;
}

static inline void mca_btl_vader_fbox_send (mca_btl_vader_fbox_t * restrict fbox, unsigned char tag,
                                            struct mca_btl_base_endpoint_t *endpoint)
{
    /* ensure data writes have completed before we mark the data as available */
    opal_atomic_wmb ();
    fbox->hdr.hdr_data.seqn = endpoint->next_sequence++;
    fbox->hdr.hdr_data.tag = tag;
    opal_atomic_wmb ();
}

static inline int mca_btl_vader_fbox_sendi (struct mca_btl_base_endpoint_t *endpoint, char tag,
                                            void * restrict header, const size_t header_size,
                                            void * restrict payload, const size_t payload_size)
{
    mca_btl_vader_fbox_t * restrict fbox;

    fbox = mca_btl_vader_reserve_fbox(endpoint, header_size + payload_size);
    if (OPAL_UNLIKELY(NULL == fbox)) {
        return 0;
    }

    memcpy (fbox->data, header, header_size);
    if (payload) {
        /* inline sends are typically just pml headers (due to MCA_BTL_FLAGS_SEND_INPLACE) */
        memcpy (fbox->data + header_size, payload, payload_size);
    }

    /* mark the fbox as sent */
    mca_btl_vader_fbox_send (fbox, tag, endpoint);

    /* send complete */
    return 1;
}

static inline bool mca_btl_vader_check_fboxes (void)
{
    const mca_btl_active_message_callback_t *reg;
    struct mca_btl_base_endpoint_t *endpoint;
    mca_btl_vader_fbox_t * restrict fbox;
    mca_btl_base_segment_t segment;
    mca_btl_base_descriptor_t desc;
    bool processed = false;
    int next_fbox;

    for (endpoint = mca_btl_vader_component.endpoints ; endpoint->peer_smp_rank != -1 ; ++endpoint) {
        next_fbox = endpoint->next_fbox_in;
        fbox = MCA_BTL_VADER_FBOX_IN_PTR(endpoint, next_fbox);

        if (NULL == endpoint->fbox_in || 0 == fbox->hdr.hdr_data.tag) {
            continue;
        }

        desc.des_dst = &segment;
        desc.des_dst_cnt = 1;

        processed = true;

        /* process all fast-box messages */
        for (int count = 0 ; count <= MCA_BTL_VADER_POLL_COUNT && 0 != fbox->hdr.hdr_data.tag ; ++count) {
            if (OPAL_UNLIKELY(endpoint->expected_sequence != fbox->hdr.hdr_data.seqn)) {
                break;
            }
            opal_atomic_mb ();
            ++endpoint->expected_sequence;

            reg = mca_btl_base_active_message_trigger + fbox->hdr.hdr_data.tag;

            segment.seg_addr.pval = fbox->data;
            segment.seg_len       = fbox->hdr.hdr_data.size;

            reg->cbfunc(&mca_btl_vader.super, fbox->hdr.hdr_data.tag, &desc, reg->cbdata);

            if (segment.seg_len > MCA_BTL_VADER_FBOX_MAX_SIZE) {
                fbox[1].hdr.ival = 0;
                opal_atomic_mb ();
                ++next_fbox;
            }
            fbox->hdr.ival = 0;

            next_fbox = MCA_BTL_VADER_NEXT_FBOX(next_fbox);
            fbox = (mca_btl_vader_fbox_t * restrict) MCA_BTL_VADER_FBOX_IN_PTR(endpoint, next_fbox);
        }

        opal_atomic_mb ();

        endpoint->next_fbox_in = next_fbox;
    }

    return processed;
}

#endif /* !defined(MCA_BTL_VADER_FBOX_H) */
