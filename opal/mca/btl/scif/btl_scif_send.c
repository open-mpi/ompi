/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_scif.h"
#include "btl_scif_frag.h"

#define BUFFER_FREE(s,e,hbm) (((s) > (e) || ((s) == (e) && !hbm)) ? (s) - (e) : (mca_btl_scif_component.segment_size - (e)))

/* attempt to reserve a contiguous segment from the remote endpoint */
static inline int mca_btl_scif_send_get_buffer (mca_btl_base_endpoint_t *endpoint, size_t size, unsigned char * restrict *dst)
{
    /* the high bit helps determine if the buffer is empty or full */
    bool hbm = (endpoint->send_buffer.start >> 31) == (endpoint->send_buffer.end >> 31);
    const unsigned int segment_size = mca_btl_scif_component.segment_size;
    unsigned int start = endpoint->send_buffer.start & ~ (1 << 31);
    unsigned int end = endpoint->send_buffer.end & ~ (1 << 31);
    unsigned int buffer_free = BUFFER_FREE(start, end, hbm);
#if defined(SCIF_TIMING)
    struct timespec ts;

    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
#endif

    /* need space for the fragment + the header */
    size += sizeof (mca_btl_scif_frag_hdr_t);

    /* check if we need to free up space for this fragment */
    if (OPAL_UNLIKELY(buffer_free < size)) {
        BTL_VERBOSE(("not enough room for a fragment of size %u. in use buffer segment: {start: %x, end: %x, high bit matches: %d}\n",
                     (unsigned) size, start, end, (int) hbm));

        /* read the current start pointer from the remote peer */
        start = endpoint->send_buffer.start = endpoint->send_buffer.startp[0];
        start &= ~ (1 << 31);
        hbm = (endpoint->send_buffer.start >> 31) == (endpoint->send_buffer.end >> 31);
        buffer_free = BUFFER_FREE(start, end, hbm);

        opal_atomic_rmb ();

        /* if this is the end of the buffer. does the fragment fit? */
        if (OPAL_UNLIKELY(buffer_free > 0 && buffer_free < size && start <= end)) {
            mca_btl_scif_frag_hdr_t hdr;

            hdr.size = buffer_free - sizeof (mca_btl_scif_frag_hdr_t);
            hdr.tag  = 0xff;
#if defined(SCIF_USE_SEQ)
            hdr.seq  = endpoint->seq_next++;
            ((uint64_t *) (endpoint->send_buffer.buffer + end))[0] = *((uint64_t *) &hdr);
#else
            ((uint32_t *) (endpoint->send_buffer.buffer + end))[0] = *((uint32_t *) &hdr);
#endif

            /* toggle the high bit */
            end = 64;
            endpoint->send_buffer.end = ((endpoint->send_buffer.end & (1 << 31)) ^ (1 << 31)) | end;
            hbm = (endpoint->send_buffer.start >> 31) == (endpoint->send_buffer.end >> 31);
            buffer_free = BUFFER_FREE(start, end, hbm);
        }

        if (OPAL_UNLIKELY(buffer_free < size)) {
#if defined(SCIF_TIMING)
            SCIF_UPDATE_TIMER(mca_btl_scif_component.aquire_buffer_time, mca_btl_scif_component.aquire_buffer_time_max, ts);
#endif
            return OPAL_ERR_OUT_OF_RESOURCE;
       }
    }

    BTL_VERBOSE(("writing fragment of size %u to offset %u {start: %x, end: %x} of peer's buffer. free = %u",
                 (unsigned int) size, end, start, end, buffer_free));

    *dst = endpoint->send_buffer.buffer + end;

    /* align the buffer on a 64 byte boundary */
    end = (end + size + 63) & ~63;

    if (OPAL_UNLIKELY(segment_size == end)) {
        endpoint->send_buffer.end = ((endpoint->send_buffer.end & (1 << 31)) ^ (1 << 31)) | 64;
    } else {
        endpoint->send_buffer.end = (endpoint->send_buffer.end & (1 << 31)) | end;
    }

#if defined(SCIF_TIMING)
    SCIF_UPDATE_TIMER(mca_btl_scif_component.aquire_buffer_time, mca_btl_scif_component.aquire_buffer_time_max, ts);
#endif

    return OPAL_SUCCESS;
}

static void mark_buffer (struct mca_btl_base_endpoint_t *endpoint)
{
    if (endpoint->port_id.node != mca_btl_scif_module.port_id.node) {
        /* force the PCIe bus to flush by reading from the remote node */
        volatile uint32_t start = endpoint->send_buffer.startp[0]; (void)start;

        endpoint->send_buffer.endp[0] = endpoint->send_buffer.end;

        endpoint->send_buffer.start = endpoint->send_buffer.startp[0];
    } else {
        MB();
        endpoint->send_buffer.endp[0] = endpoint->send_buffer.end; 
   }
}

static int mca_btl_scif_send_frag (struct mca_btl_base_endpoint_t *endpoint,
                                   mca_btl_scif_base_frag_t *frag)
{
    size_t size = frag->hdr.size;
    unsigned char * restrict dst;

    BTL_VERBOSE(("btl/scif sending descriptor %p from %d -> %d. length = %" PRIu64, (void *) frag,
                 OPAL_PROC_MY_NAME.vpid, endpoint->peer_proc->proc_name.vpid, frag->segments[0].seg_len));

    if (OPAL_LIKELY(OPAL_SUCCESS == mca_btl_scif_send_get_buffer (endpoint, size, &dst))) {
        unsigned char * restrict data = (unsigned char * restrict) frag->segments[0].seg_addr.pval;
#if defined(SCIF_TIMING)
        struct timespec ts;

        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
#endif

        memcpy (dst + sizeof (frag->hdr), data, frag->segments[0].seg_len);

        if (frag->segments[1].seg_len) {
            memcpy (dst + sizeof (frag->hdr) + frag->segments[0].seg_len,
                     frag->segments[1].seg_addr.pval,
                     frag->segments[1].seg_len);
        }

#if defined(SCIF_USE_SEQ)
        frag->hdr.seq  = endpoint->seq_next++;
        /* write the tag to signal the fragment is available */
        ((uint64_t *) dst)[0] = *((uint64_t *) &frag->hdr);
#else
        ((uint32_t *) dst)[0] = *((uint32_t *) &frag->hdr);
#endif

        opal_atomic_wmb ();

#if defined(SCIF_TIMING)
        SCIF_UPDATE_TIMER(mca_btl_scif_component.send_time, mca_btl_scif_component.send_time_max, ts);
#endif

        /* fragment is gone */
        mca_btl_scif_frag_complete (frag, OPAL_SUCCESS);

        return 1;
    }

    return OPAL_ERR_OUT_OF_RESOURCE;
}

int mca_btl_scif_send (struct mca_btl_base_module_t *btl,
                       struct mca_btl_base_endpoint_t *endpoint,
                       struct mca_btl_base_descriptor_t *descriptor,
                       mca_btl_base_tag_t tag)
{
    mca_btl_scif_base_frag_t *frag = (mca_btl_scif_base_frag_t *) descriptor;
    size_t size = frag->segments[0].seg_len + frag->segments[1].seg_len;
    int rc;

    frag->hdr.tag  = tag;
    frag->hdr.size = size;

    if (OPAL_UNLIKELY(MCA_BTL_SCIF_EP_STATE_CONNECTED != endpoint->state)) {
        rc = mca_btl_scif_ep_connect (endpoint);
        if (OPAL_UNLIKELY(MCA_BTL_SCIF_EP_STATE_CONNECTED != endpoint->state)) {
            /* the receiver was not ready to handle the fragment. queue up the fragment. */
            descriptor->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
            opal_list_append (&endpoint->frag_wait_list, (opal_list_item_t *) descriptor);
            return OPAL_SUCCESS;
        }
    }

    rc = mca_btl_scif_send_frag (endpoint, frag);
    if (OPAL_LIKELY(1 == rc)) {
        mark_buffer (endpoint);
        return 1;
    }

    /* the receiver was not ready to handle the fragment. queue up the fragment. */
    descriptor->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    opal_list_append (&endpoint->frag_wait_list, (opal_list_item_t *) descriptor);

    return OPAL_SUCCESS;
}

int mca_btl_scif_sendi (struct mca_btl_base_module_t *btl,
                         struct mca_btl_base_endpoint_t *endpoint,
                         struct opal_convertor_t *convertor,
                         void *header, size_t header_size,
                         size_t payload_size, uint8_t order,
                         uint32_t flags, mca_btl_base_tag_t tag,
                         mca_btl_base_descriptor_t **descriptor)
{
    size_t length = (header_size + payload_size);
    unsigned char * restrict base;
    mca_btl_scif_frag_hdr_t hdr;
    size_t max_data;
    int rc;
#if defined(SCIF_TIMING)
    struct timespec ts;
#endif

    assert (length < mca_btl_scif_module.super.btl_eager_limit);
    assert (0 == (flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK));

    if (OPAL_UNLIKELY(MCA_BTL_SCIF_EP_STATE_CONNECTED != endpoint->state)) {
        rc = mca_btl_scif_ep_connect (endpoint);
        if (OPAL_UNLIKELY(MCA_BTL_SCIF_EP_STATE_CONNECTED != endpoint->state)) {
            return OPAL_ERR_RESOURCE_BUSY;
        }
    }

    rc = mca_btl_scif_send_get_buffer (endpoint, length, &base);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        if (NULL != descriptor) {
            *descriptor = NULL;
        }
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

#if defined(SCIF_TIMING)
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
#endif

    /* fill in the fragment header (except for the tag) */
    hdr.size = length;
    hdr.tag  = tag;

#if defined(SCIF_USE_SEQ)
    hdr.seq  = endpoint->seq_next++;
#endif

    /* write the match header (with MPI comm/tag/etc. info) */
    memcpy (base + sizeof (hdr), header, header_size);

    if (payload_size) {
        uint32_t iov_count = 1;
        struct iovec iov[1];

        iov[0].iov_base = base + sizeof (hdr) + header_size;
        iov[0].iov_len  = payload_size;

        /* move the data */
        opal_convertor_pack (convertor, iov, &iov_count, &max_data);

        assert (max_data == payload_size);
    }

#if defined(SCIF_USE_SEQ)
    /* signal the remote side that this fragment is available */
    ((uint64_t *)base)[0] = *((uint64_t *) &hdr);
#else
    ((uint32_t *)base)[0] = *((uint32_t *) &hdr);
#endif

    opal_atomic_wmb ();

    mark_buffer (endpoint);

#if defined(SCIF_TIMING)
    SCIF_UPDATE_TIMER(mca_btl_scif_component.sendi_time, mca_btl_scif_component.sendi_time_max, ts);
#endif

    return OPAL_SUCCESS;
}

int mca_btl_scif_progress_send_wait_list (mca_btl_base_endpoint_t *endpoint)
{
    mca_btl_scif_base_frag_t *frag;
    int rc = OPAL_SUCCESS;

    while (NULL !=
           (frag = (mca_btl_scif_base_frag_t *) opal_list_remove_first (&endpoint->frag_wait_list))) {
        rc = mca_btl_scif_send_frag (endpoint, frag);
        if (OPAL_UNLIKELY(OPAL_SUCCESS > rc)) {
            if (OPAL_LIKELY(OPAL_ERR_OUT_OF_RESOURCE == rc)) {
                opal_list_prepend (&endpoint->frag_wait_list, (opal_list_item_t *) frag);
            } else {
                mca_btl_scif_frag_complete (frag, rc);
            }

            break;
        }
    }

    mark_buffer (endpoint);
    return OPAL_SUCCESS;
}
