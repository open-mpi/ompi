/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020-2025 Google, LLC. All rights reserveed.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_SM_FBOX_H
#define MCA_BTL_SM_FBOX_H

#include "opal/mca/btl/sm/btl_sm_types.h"
#include "opal/mca/btl/sm/btl_sm_virtual.h"
#include "opal/util/minmax.h"

#define MCA_BTL_SM_POLL_COUNT          31
#define MCA_BTL_SM_FBOX_ALIGNMENT      32
#define MCA_BTL_SM_FBOX_ALIGNMENT_MASK (MCA_BTL_SM_FBOX_ALIGNMENT - 1)

typedef union mca_btl_sm_fbox_hdr_t {
    struct {
        /* NTH: on 32-bit platforms loading/unloading the header may be completed
         * in multiple instructions. To ensure that seq is never loaded before tag
         * and the tag is never read before seq put them in the same 32-bits of the
         * header. */
        /** message size */
        uint32_t size;
        /** message tag */
        uint16_t tag;
        /** sequence number */
        uint16_t seq;
    } data;
    struct {
        uint32_t value0;
        uint32_t value1;
    } data_i32;
    uint64_t ival;
} mca_btl_sm_fbox_hdr_t;

/**
 *  An abstraction that represents a connection to a endpoint process.
 *  An instance of mca_ptl_base_endpoint_t is associated w/ each process
 *  and BTL pair at startup.
 */

static inline void mca_btl_sm_endpoint_setup_fbox_recv(struct mca_btl_base_endpoint_t *endpoint,
                                                       void *base)
{
    endpoint->fbox_in.metadata = (mca_btl_sm_fbox_metadata_t *) base;
    endpoint->fbox_in.start = endpoint->fbox_in.metadata->start;
    endpoint->fbox_in.seq = 0;
    endpoint->fbox_in.buffer = (unsigned char *)(endpoint->fbox_in.metadata + 1);
}

static inline void mca_btl_sm_endpoint_setup_fbox_send(struct mca_btl_base_endpoint_t *endpoint,
                                                       opal_free_list_item_t *fbox)
{
    void *base = fbox->ptr;

    endpoint->fbox_out.start = 0;
    endpoint->fbox_out.end = 0;
    endpoint->fbox_out.seq = 0;
    endpoint->fbox_out.fbox = fbox;

    endpoint->fbox_out.metadata = (mca_btl_sm_fbox_metadata_t *) base;
    endpoint->fbox_out.metadata->start = 0;

    endpoint->fbox_out.buffer = (unsigned char *)(endpoint->fbox_out.metadata + 1);

    /* zero out the first header in the fast box */
    ((mca_btl_sm_fbox_hdr_t *)endpoint->fbox_out.buffer)->ival = 0;

    opal_atomic_wmb();
}

#define MCA_BTL_SM_FBOX_HDR(x) ((mca_btl_sm_fbox_hdr_t *) (x))

static inline unsigned int mca_btl_sm_fbox_out_free(mca_btl_sm_fbox_out_t *fbox_out, unsigned int fbox_size) {
    unsigned int fbox_offset_mask = fbox_size - 1;
    unsigned int masked_end = fbox_out->end & fbox_offset_mask;
    unsigned int free;

    /* start pointer will always trail the end pointer. check for rollover of the end pointer where
     * the start pointer has not yet rolled over. */
    if (OPAL_UNLIKELY(fbox_out->end <= fbox_size && fbox_out->start > fbox_size)) {
        unsigned int masked_start = fbox_out->start & fbox_offset_mask;
        free = masked_start - masked_end;
    } else {
        free = fbox_size - (fbox_out->end - fbox_out->start);
    }

    return opal_min(free, fbox_size - masked_end);
}

void mca_btl_sm_poll_handle_frag(mca_btl_sm_hdr_t *hdr, mca_btl_base_endpoint_t *endpoint);

static inline void mca_btl_sm_fbox_set_header(mca_btl_sm_fbox_hdr_t *hdr, uint16_t tag,
                                              uint16_t seq, uint32_t size)
{
    mca_btl_sm_fbox_hdr_t tmp = {.data = {.tag = tag, .seq = seq, .size = size}};
    opal_atomic_wmb();
    hdr->ival = tmp.ival;
}

static inline mca_btl_sm_fbox_hdr_t mca_btl_sm_fbox_read_header(mca_btl_sm_fbox_hdr_t *hdr)
{
    mca_btl_sm_fbox_hdr_t tmp = {.data_i32 = {.value1 = hdr->data_i32.value1}};

    opal_atomic_rmb();
    tmp.data_i32.value0 = hdr->data_i32.value0;
    return tmp;
}

static inline unsigned int mca_btl_sm_fbox_align(unsigned int size) {
    return (size + MCA_BTL_SM_FBOX_ALIGNMENT_MASK) & ~MCA_BTL_SM_FBOX_ALIGNMENT_MASK;
}

static inline unsigned char *mca_btl_sm_fbox_reserve_locked(mca_btl_base_endpoint_t *ep, unsigned int data_size) {
    const unsigned int fbox_size = mca_btl_sm_component.fbox_size;
    const unsigned int fbox_offset_mask = fbox_size - 1;
    unsigned int buffer_free;
    unsigned char *dst;
    size_t aligned_entry_size;

    /* don't try to use the per-peer buffer for messages that will fill up more than 25% of the
     * buffer */
    if (OPAL_UNLIKELY(NULL == ep->fbox_out.buffer || data_size > (fbox_size >> 2))) {
        return NULL;
    }

    assert ((fbox_size & fbox_offset_mask) == 0);

    buffer_free = mca_btl_sm_fbox_out_free(&ep->fbox_out, fbox_size);

    /* need space for the fragment + the header */
    aligned_entry_size = mca_btl_sm_fbox_align(data_size + sizeof(mca_btl_sm_fbox_hdr_t));

    dst = ep->fbox_out.buffer + (ep->fbox_out.end & fbox_offset_mask);

    if (OPAL_UNLIKELY(buffer_free < aligned_entry_size)) {
        /* check if we need to free up space for this fragment */
        BTL_VERBOSE(("not enough room for a fragment of size %u. in use buffer segment: {start: "
                     "%x, end: %x}",
                     (unsigned) aligned_entry_size, ep->fbox_out.start, ep->fbox_out.end));

        /* read the current start pointer from the remote peer and recalculate the available buffer
         * space */
        ep->fbox_out.start = ep->fbox_out.metadata->start;
        opal_atomic_rmb();

        buffer_free = mca_btl_sm_fbox_out_free(&ep->fbox_out, fbox_size);

        /* if this is the end of the buffer and the fragment doesn't fit then mark the remaining
         * buffer space to be skipped and check if the fragment can be written at the beginning of
         * the buffer. */
        if (OPAL_UNLIKELY(buffer_free > 0 && buffer_free < aligned_entry_size &&
                          ((ep->fbox_out.end + buffer_free) & fbox_offset_mask) == 0)) {
#if OPAL_ENABLE_DEBUG
            unsigned int old_end = ep->fbox_out.end;
#endif
            unsigned int remaining = buffer_free;

            BTL_VERBOSE(("space needed for message: %" PRIsize_t ", remaining space in buffer: "
                         "%u, checking for space at beginning of buffer",
                         aligned_entry_size, remaining));

            ep->fbox_out.end += remaining;
            buffer_free = mca_btl_sm_fbox_out_free(&ep->fbox_out, fbox_size);
            if (OPAL_UNLIKELY(buffer_free < aligned_entry_size)) {
                /* not writing the skip token so give this space back */
                ep->fbox_out.end -= remaining;
                return NULL;
            }

            MCA_BTL_SM_FBOX_HDR(ep->fbox_out.buffer)->ival = 0;
            opal_atomic_wmb();

            BTL_VERBOSE(("writing a skip token at offset %u", old_end));
            /* space is available. go ahead and mark remaining space to skip */
            mca_btl_sm_fbox_set_header(MCA_BTL_SM_FBOX_HDR(dst), 0xff, ep->fbox_out.seq++,
                                       remaining - sizeof(mca_btl_sm_fbox_hdr_t));
            dst = ep->fbox_out.buffer;
        }

        if (buffer_free < aligned_entry_size) {
            return NULL;
        }
    }

    BTL_VERBOSE(("writing fragment of size %u {start: 0x%x, end: 0x%x} of "
                 "peer's buffer. free = %u",
                 (unsigned int) aligned_entry_size, ep->fbox_out.start, ep->fbox_out.end, buffer_free));

    ep->fbox_out.end += aligned_entry_size;
    
    /* zero-out the next */
    if (buffer_free > aligned_entry_size) {
        MCA_BTL_SM_FBOX_HDR(ep->fbox_out.buffer + (ep->fbox_out.end & fbox_offset_mask))->ival = 0;
        opal_atomic_wmb();
    }

    return dst;
}

/* attempt to reserve a contiguous segment from the remote ep */
static inline bool mca_btl_sm_fbox_sendi(mca_btl_base_endpoint_t *ep, unsigned char tag,
                                         void *restrict header, const size_t header_size,
                                         void *restrict payload, const size_t payload_size)
{
    size_t data_size = header_size + payload_size;

    OPAL_THREAD_LOCK(&ep->lock);
    unsigned char *dst = mca_btl_sm_fbox_reserve_locked(ep, (unsigned int) data_size);
    OPAL_THREAD_UNLOCK(&ep->lock);
    if (OPAL_UNLIKELY(NULL == dst)) {
        return false;
    }

    unsigned char *data = dst + sizeof(mca_btl_sm_fbox_hdr_t);

    memcpy(data, header, header_size);
    if (payload) {
        /* inline sends are typically just pml headers (due to MCA_BTL_FLAGS_SEND_INPLACE) */
        memcpy(data + header_size, payload, payload_size);
    }

    opal_atomic_wmb();
    /* write out part of the header now. the tag will be written when the data is available */
    mca_btl_sm_fbox_set_header(MCA_BTL_SM_FBOX_HDR(dst), tag, ep->fbox_out.seq++,
                               (uint32_t) data_size);

    return true;
}

static inline bool mca_btl_sm_poll_fbox(mca_btl_base_endpoint_t *ep)
{
    const unsigned int fbox_offset_mask = mca_btl_sm_component.fbox_size - 1;
    unsigned int start_offset = ep->fbox_in.start & fbox_offset_mask;
    const mca_btl_sm_fbox_hdr_t hdr = mca_btl_sm_fbox_read_header(
        MCA_BTL_SM_FBOX_HDR(ep->fbox_in.buffer + start_offset));

    /* check for a valid tag a sequence number */
    if (0 == hdr.data.tag || hdr.data.seq != ep->fbox_in.seq) {
        return false;
    }

    ++ep->fbox_in.seq;

    /* force all prior reads to complete before continuing */
    opal_atomic_rmb();

    BTL_VERBOSE(
        ("got frag from %d with header {.tag = %d, .size = %d, .seq = %u} from offset %u",
         ep->peer_smp_rank, hdr.data.tag, hdr.data.size, hdr.data.seq, start_offset));

    /* the 0xff tag indicates we should skip the rest of the buffer */
    if (OPAL_LIKELY((0xfe & hdr.data.tag) != 0xfe)) {
        mca_btl_base_segment_t segment;
        const mca_btl_active_message_callback_t *reg = mca_btl_base_active_message_trigger
            + hdr.data.tag;
        mca_btl_base_receive_descriptor_t desc = {.endpoint = ep,
                                                  .des_segments = &segment,
                                                  .des_segment_count = 1,
                                                  .tag = hdr.data.tag,
                                                  .cbdata = reg->cbdata};

        /* fragment fits entirely in the remaining buffer space. some
         * btl users do not handle fragmented data so we can't split
         * the fragment without introducing another copy here. this
         * limitation has not appeared to cause any performance
         * degradation. */
        segment.seg_len = hdr.data.size;
        segment.seg_addr.pval = (void *) (ep->fbox_in.buffer + start_offset + sizeof(hdr));

        /* call the registered callback function */
        reg->cbfunc(&mca_btl_sm.super, &desc);
    } else if (OPAL_LIKELY(0xfe == hdr.data.tag)) {
        /* process fragment header */
        fifo_value_t *value = (fifo_value_t *) (ep->fbox_in.buffer + start_offset + sizeof(hdr));
        mca_btl_sm_hdr_t *sm_hdr = relative2virtual(*value);
        mca_btl_sm_poll_handle_frag(sm_hdr, ep);
    }

    ep->fbox_in.start += mca_btl_sm_fbox_align(hdr.data.size + sizeof(hdr));

    return true;
}

static inline int mca_btl_sm_check_fboxes(void)
{
    int total_processed = 0;

    for (unsigned int i = 0; i < mca_btl_sm_component.num_fbox_in_endpoints; ++i) {
        mca_btl_base_endpoint_t *ep = mca_btl_sm_component.fbox_in_endpoints[i];

        int frag_count = 0;
        for (int j = 0 ; j < MCA_BTL_SM_POLL_COUNT ; ++j) {
            if (!mca_btl_sm_poll_fbox(ep)) {
                break;
            }
            ++frag_count;
        }

        if (frag_count) {
            BTL_VERBOSE(("finished processing at offset %x", ep->fbox_in.start));

            /* let the sender know where we stopped */
            opal_atomic_mb();
            ep->fbox_in.metadata->start = ep->fbox_in.start;
            total_processed += frag_count;
        }
    }

    return total_processed;
}

static inline void mca_btl_sm_try_fbox_setup(mca_btl_base_endpoint_t *ep, mca_btl_sm_hdr_t *hdr)
{
    if (OPAL_UNLIKELY(NULL == ep->fbox_out.buffer
                      && mca_btl_sm_component.fbox_threshold
                             == OPAL_THREAD_ADD_FETCH_SIZE_T(&ep->send_count, 1))) {
        /* protect access to mca_btl_sm_component.segment_offset */
        OPAL_THREAD_LOCK(&mca_btl_sm_component.lock);

        /* verify the remote side will accept another fbox */
        if (0 <= opal_atomic_add_fetch_32(&ep->fifo->fbox_available, -1)) {
            opal_free_list_item_t *fbox = opal_free_list_get(&mca_btl_sm_component.sm_fboxes);

            if (NULL != fbox) {
                /* zero out the fast box */
                memset(fbox->ptr, 0, mca_btl_sm_component.fbox_size);
                mca_btl_sm_endpoint_setup_fbox_send(ep, fbox);

                hdr->flags |= MCA_BTL_SM_FLAG_SETUP_FBOX;
                hdr->fbox_base = virtual2relative((char *) ep->fbox_out.metadata);
            } else {
                opal_atomic_add_fetch_32(&ep->fifo->fbox_available, 1);
            }

            opal_atomic_wmb();
        }

        OPAL_THREAD_UNLOCK(&mca_btl_sm_component.lock);
    }
}

#endif /* MCA_BTL_SM_FBOX_H */
