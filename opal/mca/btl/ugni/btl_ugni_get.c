/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
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
    mca_btl_ugni_segment_t *src_seg = (mca_btl_ugni_segment_t *) des->des_remote;
    mca_btl_ugni_segment_t *dst_seg = (mca_btl_ugni_segment_t *) des->des_local;
    size_t size = src_seg->base.seg_len - src_seg->extra_byte_count;
    bool check;

    BTL_VERBOSE(("Using RDMA/FMA Get"));

    /* cause endpoint to bind if it isn't already (bind is sufficient for rdma) */
    (void) mca_btl_ugni_check_endpoint_state(endpoint);

    /* Check if the get is aligned/sized on a multiple of 4 */
    check = !!((des->des_remote->seg_addr.lval | des->des_local->seg_addr.lval | size) & 3);

    if (OPAL_UNLIKELY(check || size > mca_btl_ugni_component.ugni_get_limit)) {
        /* switch to put */
        return OPAL_ERR_NOT_AVAILABLE;
    }

    if (src_seg->extra_byte_count) {
        memmove ((char *) dst_seg->base.seg_addr.pval + size, src_seg->extra_bytes, src_seg->extra_byte_count);
        src_seg->base.seg_len = size;
        dst_seg->base.seg_len = size;
    }

    des->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    return mca_btl_ugni_post (frag, true, dst_seg, src_seg);
}

/* eager get */
static void mca_btl_ugni_callback_eager_get_progress_pending (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                                              struct mca_btl_base_descriptor_t *desc, int rc)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    mca_btl_ugni_base_frag_t *pending_frag, *frag = (mca_btl_ugni_base_frag_t *) desc;

    OPAL_THREAD_LOCK(&ugni_module->eager_get_pending_lock);
    pending_frag = (mca_btl_ugni_base_frag_t *) opal_list_remove_first (&ugni_module->eager_get_pending);
    OPAL_THREAD_UNLOCK(&ugni_module->eager_get_pending_lock);

    if (NULL != pending_frag) {
        /* copy the relevant data out of the pending fragment */
        frag->endpoint = pending_frag->endpoint;

        /* start the next eager get using this fragment */
        (void) mca_btl_ugni_start_eager_get (frag->endpoint, pending_frag->hdr.eager_ex, frag);

        /* return the temporary fragment */
        mca_btl_ugni_frag_return (pending_frag);
    } else {
        /* not needed anymore */
        mca_btl_ugni_frag_return (frag);
    }
}

static void mca_btl_ugni_callback_eager_get (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                             struct mca_btl_base_descriptor_t *desc, int rc)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    mca_btl_ugni_base_frag_t *frag = (mca_btl_ugni_base_frag_t *) desc;
    uint32_t len = frag->hdr.eager.send.lag & 0x00ffffff;
    uint8_t tag = frag->hdr.eager.send.lag >> 24;
    size_t payload_len = frag->hdr.eager.src_seg.base.seg_len;
    size_t hdr_len = len - payload_len;
    mca_btl_active_message_callback_t *reg;
    mca_btl_base_segment_t segs[2];
    mca_btl_ugni_base_frag_t tmp;

    BTL_VERBOSE(("eager get for rem_ctx %p complete", frag->hdr.eager.ctx));

    tmp.base.des_local = segs;
    if (hdr_len) {
        tmp.base.des_local_count = 2;

        segs[0].seg_addr.pval = frag->hdr.eager_ex.pml_header;
        segs[0].seg_len       = hdr_len;
        segs[1].seg_addr.pval = frag->segments[0].base.seg_addr.pval;
        segs[1].seg_len       = payload_len;
    } else {
        tmp.base.des_local_count = 1;

        segs[0].seg_addr.pval = frag->segments[0].base.seg_addr.pval;
        segs[0].seg_len       = payload_len;
    }

    reg = mca_btl_base_active_message_trigger + tag;
    reg->cbfunc(&frag->endpoint->btl->super, tag, &(tmp.base), reg->cbdata);

    frag->hdr.rdma.ctx = frag->hdr.eager.ctx;

    /* once complete use this fragment for a pending eager get if any exist */
    frag->base.des_cbfunc = mca_btl_ugni_callback_eager_get_progress_pending;

    /* tell the remote peer the operation is complete */
    rc = opal_mca_btl_ugni_smsg_send (frag, &frag->hdr.rdma, sizeof (frag->hdr.rdma),
                                      NULL, 0, MCA_BTL_UGNI_TAG_RDMA_COMPLETE);
    if (OPAL_UNLIKELY(0 > rc)) {
        /* queue fragment */
        if (false == endpoint->wait_listed) {
            OPAL_THREAD_LOCK(&ugni_module->ep_wait_list_lock);
            opal_list_append (&ugni_module->ep_wait_list, &endpoint->super);
            OPAL_THREAD_UNLOCK(&ugni_module->ep_wait_list_lock);
            endpoint->wait_listed = true;
        }

        OPAL_THREAD_LOCK(&endpoint->lock);
        opal_list_append (&endpoint->frag_wait_list, (opal_list_item_t *) frag);
        OPAL_THREAD_UNLOCK(&endpoint->lock);
    }
}

int mca_btl_ugni_start_eager_get (mca_btl_base_endpoint_t *ep,
                                  mca_btl_ugni_eager_ex_frag_hdr_t hdr,
                                  mca_btl_ugni_base_frag_t *frag)
{
    mca_btl_ugni_module_t *ugni_module = ep->btl;
    int rc;

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

        frag->base.des_flags  = 0;

        frag->segments[1] = hdr.eager.src_seg;

        /* increase size to a multiple of 4 bytes (required for get) */
        frag->segments[0].base.seg_len = frag->segments[1].base.seg_len =
            (hdr.eager.src_seg.base.seg_len + 3) & ~3;

        frag->base.des_local = &frag->segments[1].base;

        /* set up callback for get completion */
        frag->base.des_flags  = MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
        frag->base.des_cbfunc = mca_btl_ugni_callback_eager_get;

        rc = mca_btl_ugni_post (frag, GNI_POST_RDMA_GET, frag->segments, frag->segments + 1);
        if (OPAL_UNLIKELY(OPAL_SUCCESS == rc)) {
            return OPAL_SUCCESS;
        }
    } while (0);

    OPAL_THREAD_LOCK(&ugni_module->eager_get_pending_lock);
    opal_list_append (&ugni_module->eager_get_pending, (opal_list_item_t *) frag);
    OPAL_THREAD_UNLOCK(&ugni_module->eager_get_pending_lock);

    return rc;
}
