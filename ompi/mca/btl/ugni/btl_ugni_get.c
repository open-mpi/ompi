/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni_rdma.h"
#include "btl_ugni_smsg.h"

/**
 * Initiate a get operation.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
int mca_btl_ugni_get (struct mca_btl_base_module_t *btl,
                      struct mca_btl_base_endpoint_t *endpoint,
                      struct mca_btl_base_descriptor_t *des) {
    mca_btl_ugni_base_frag_t *frag = (mca_btl_ugni_base_frag_t *) des;
    mca_btl_ugni_segment_t *src_seg = (mca_btl_ugni_segment_t *) des->des_src;
    mca_btl_ugni_segment_t *dst_seg = (mca_btl_ugni_segment_t *) des->des_dst;
    size_t size = src_seg->base.seg_len - src_seg->extra_byte_count;
    bool check;

    BTL_VERBOSE(("Using RDMA/FMA Get"));

    /* cause endpoint to bind if it isn't already (bind is sufficient for rdma) */
    (void) mca_btl_ugni_check_endpoint_state(endpoint);

    /* Check if the get is aligned/sized on a multiple of 4 */
    check = !!((des->des_src->seg_addr.lval | des->des_dst->seg_addr.lval | size) & 3);

    if (OPAL_UNLIKELY(check || size > mca_btl_ugni_component.ugni_get_limit)) {
        /* switch to put */
        return OMPI_ERR_NOT_AVAILABLE;
    }

    if (src_seg->extra_byte_count) {
        memmove ((char *) dst_seg->base.seg_addr.pval + size, src_seg->extra_bytes, src_seg->extra_byte_count);
        src_seg->base.seg_len = size;
        dst_seg->base.seg_len = size;
    }

    des->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    return mca_btl_ugni_post (frag, true, dst_seg, src_seg);
}

static void mca_btl_ugni_callback_rdma_complete (mca_btl_ugni_base_frag_t *frag, int rc)
{
    BTL_VERBOSE(("rdma operation for rem_ctx %p complete", frag->hdr.rdma.ctx));

    /* tell peer the get is complete */
    rc = ompi_mca_btl_ugni_smsg_send (frag, &frag->hdr.rdma, sizeof (frag->hdr.rdma),
                                      NULL, 0, MCA_BTL_UGNI_TAG_RDMA_COMPLETE);
    if (OPAL_UNLIKELY(0 > rc)) {
        /* call this callback again later */
        frag->cbfunc = mca_btl_ugni_callback_rdma_complete;
        opal_list_append (&frag->endpoint->btl->failed_frags, (opal_list_item_t *) frag);
    }
}

/* eager get */
static void mca_btl_ugni_callback_eager_get_retry (mca_btl_ugni_base_frag_t *frag, int rc)
{
    (void) mca_btl_ugni_start_eager_get(frag->endpoint, frag->hdr.eager_ex, frag);
}

static void mca_btl_ugni_callback_eager_get (mca_btl_ugni_base_frag_t *frag, int rc)
{
    uint32_t len = frag->hdr.eager.send.lag & 0x00ffffff;
    uint8_t tag = frag->hdr.eager.send.lag >> 24;
    size_t payload_len = frag->hdr.eager.src_seg.base.seg_len;
    size_t hdr_len = len - payload_len;
    mca_btl_active_message_callback_t *reg;
    mca_btl_base_segment_t segs[2];
    mca_btl_ugni_base_frag_t tmp;

    BTL_VERBOSE(("eager get for rem_ctx %p complete", frag->hdr.eager.ctx));

    tmp.base.des_dst = segs;
    if (hdr_len) {
        tmp.base.des_dst_cnt = 2;

        segs[0].seg_addr.pval = frag->hdr.eager_ex.pml_header;
        segs[0].seg_len       = hdr_len;
        segs[1].seg_addr.pval = frag->segments[0].base.seg_addr.pval;
        segs[1].seg_len       = payload_len;
    } else {
        tmp.base.des_dst_cnt = 1;

        segs[0].seg_addr.pval = frag->segments[0].base.seg_addr.pval;
        segs[0].seg_len       = payload_len;
    }

    reg = mca_btl_base_active_message_trigger + tag;
    reg->cbfunc(&frag->endpoint->btl->super, tag, &(tmp.base), reg->cbdata);

    frag->hdr.rdma.ctx = frag->hdr.eager.ctx;

    /* tell the remote peer the operation is complete */
    mca_btl_ugni_callback_rdma_complete (frag, rc);
}

int mca_btl_ugni_start_eager_get (mca_btl_base_endpoint_t *ep,
                                  mca_btl_ugni_eager_ex_frag_hdr_t hdr,
                                  mca_btl_ugni_base_frag_t *frag)
{
    int rc;

    if (OPAL_UNLIKELY(frag && frag->my_list == &ep->btl->rdma_int_frags)) {
        mca_btl_ugni_frag_return (frag);
        frag = NULL;
    }

    BTL_VERBOSE(("starting eager get for remote ctx: %p", hdr.eager.ctx));

    do {
        if (NULL == frag) {
            rc = MCA_BTL_UGNI_FRAG_ALLOC_EAGER_RECV(ep, frag);
            if (OPAL_UNLIKELY(NULL == frag)) {
                (void) MCA_BTL_UGNI_FRAG_ALLOC_RDMA_INT(ep, frag);
                assert (NULL != frag);
                frag->hdr.eager_ex = hdr;
                break;
            }
        }

        frag->hdr.eager_ex = hdr;
        frag->flags = 0;

        frag->base.des_flags  = MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;

        frag->segments[1] = hdr.eager.src_seg;

        /* increase size to a multiple of 4 bytes (required for get) */
        frag->segments[0].base.seg_len = frag->segments[1].base.seg_len =
            (hdr.eager.src_seg.base.seg_len + 3) & ~3;

        frag->base.des_src = &frag->segments[1].base;

        rc = mca_btl_ugni_post_wcb (frag, GNI_POST_RDMA_GET, frag->segments, frag->segments + 1,
                                    mca_btl_ugni_callback_eager_get);
        if (OPAL_UNLIKELY(OMPI_SUCCESS == rc)) {
            return OMPI_SUCCESS;
        }
    } while (0);

    frag->cbfunc = mca_btl_ugni_callback_eager_get_retry;
    opal_list_append (&ep->btl->failed_frags, (opal_list_item_t *) frag);

    return rc;
}
