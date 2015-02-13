/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
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

int mca_btl_ugni_get (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                      mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                      int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    bool check;

    /* Check if the get is aligned/sized on a multiple of 4 */
    check = !!((remote_address | (uint64_t)(intptr_t) local_address | size) & (mca_btl_ugni_module.super.btl_get_alignment - 1));

    if (OPAL_UNLIKELY(check || size > mca_btl_ugni_module.super.btl_get_limit)) {
        BTL_VERBOSE(("RDMA/FMA Get not available due to size or alignment restrictions"));

        /* notify the caller that get is not available */
        return OPAL_ERR_NOT_AVAILABLE;
    }

    BTL_VERBOSE(("Using RDMA/FMA Get from local address %p to remote address %" PRIx64,
                 local_address, remote_address));

    /* cause endpoint to bind if it isn't already (bind is sufficient for rdma) */
    (void) mca_btl_ugni_check_endpoint_state_rdma (endpoint);

    return mca_btl_ugni_post (endpoint, true, size, local_address, remote_address, local_handle,
                              remote_handle, order, cbfunc, cbcontext, cbdata);
}

/* eager get */
static void mca_btl_ugni_callback_eager_get_progress_pending (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                                              struct mca_btl_base_descriptor_t *desc, int rc)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    mca_btl_ugni_base_frag_t *pending_frag, *frag = (mca_btl_ugni_base_frag_t *) desc;

    memset (&frag->hdr, 0, sizeof (frag->hdr));

    OPAL_THREAD_LOCK(&ugni_module->eager_get_pending_lock);
    pending_frag = (mca_btl_ugni_base_frag_t *) opal_list_remove_first (&ugni_module->eager_get_pending);
    OPAL_THREAD_UNLOCK(&ugni_module->eager_get_pending_lock);

    if (NULL != pending_frag) {
        /* copy the relevant data out of the pending fragment */
        frag->endpoint = pending_frag->endpoint;

        assert (frag != pending_frag);

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
                                             void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                             void *context, void *cbdata, int status)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    mca_btl_ugni_base_frag_t *frag = (mca_btl_ugni_base_frag_t *) context;
    uint32_t len = frag->hdr.eager.send.lag & 0x00ffffff;
    uint8_t tag = frag->hdr.eager.send.lag >> 24;
    size_t payload_len = frag->hdr.eager.size;
    size_t hdr_len = len - payload_len;
    mca_btl_active_message_callback_t *reg;
    mca_btl_base_segment_t segs[2];
    mca_btl_ugni_base_frag_t tmp;
    int rc;

    BTL_VERBOSE(("eager get for rem_ctx %p complete", frag->hdr.eager.ctx))

    tmp.base.des_segments = segs;
    if (hdr_len) {
        tmp.base.des_segment_count = 2;

        segs[0].seg_addr.pval = frag->hdr.eager_ex.pml_header;
        segs[0].seg_len       = hdr_len;
        segs[1].seg_addr.pval = local_address;
        segs[1].seg_len       = payload_len;
    } else {
        tmp.base.des_segment_count = 1;

        segs[0].seg_addr.pval = local_address;
        segs[0].seg_len       = payload_len;
    }

    reg = mca_btl_base_active_message_trigger + tag;
    reg->cbfunc(&frag->endpoint->btl->super, tag, &(tmp.base), reg->cbdata);

    /* fill in the response header */
    frag->hdr.rdma.ctx = frag->hdr.eager.ctx;
    frag->flags = MCA_BTL_UGNI_FRAG_RESPONSE;

    /* once complete use this fragment for a pending eager get if any exist */
    frag->base.des_cbfunc = mca_btl_ugni_callback_eager_get_progress_pending;

    /* tell the remote peer the operation is complete */
    rc = opal_mca_btl_ugni_smsg_send (frag, &frag->hdr.rdma, sizeof (frag->hdr.rdma),
                                      NULL, 0, MCA_BTL_UGNI_TAG_RDMA_COMPLETE);
    if (OPAL_UNLIKELY(0 > rc)) {
        /* queue fragment */
        OPAL_THREAD_LOCK(&endpoint->lock);
        if (false == endpoint->wait_listed) {
            OPAL_THREAD_LOCK(&ugni_module->ep_wait_list_lock);
            opal_list_append (&ugni_module->ep_wait_list, &endpoint->super);
            OPAL_THREAD_UNLOCK(&ugni_module->ep_wait_list_lock);
            endpoint->wait_listed = true;
        }

        opal_list_append (&endpoint->frag_wait_list, (opal_list_item_t *) frag);
        OPAL_THREAD_UNLOCK(&endpoint->lock);
    }
}

int mca_btl_ugni_start_eager_get (mca_btl_base_endpoint_t *endpoint,
                                  mca_btl_ugni_eager_ex_frag_hdr_t hdr,
                                  mca_btl_ugni_base_frag_t *frag)
{
    mca_btl_ugni_module_t *ugni_module = endpoint->btl;
    size_t size;
    int rc;

    BTL_VERBOSE(("starting eager get for remote ctx: %p", hdr.eager.ctx));

    do {
        if (NULL == frag) {
            /* try to allocate a registered buffer */
            rc = MCA_BTL_UGNI_FRAG_ALLOC_EAGER_RECV(endpoint, frag);
            if (OPAL_UNLIKELY(NULL == frag)) {
                /* no registered buffers available. try again later */
                (void) MCA_BTL_UGNI_FRAG_ALLOC_RDMA_INT(endpoint, frag);

                /* not much can be done if a small fragment can not be allocated. abort! */
                assert (NULL != frag);
                frag->hdr.eager_ex = hdr;
                break;
            }
        }

        frag->flags = 0;

        frag->hdr.eager_ex = hdr;

        /* increase size to a multiple of 4 bytes (required for get on Gemini) */
        size = (hdr.eager.size + 3) & ~3;

        /* set up callback for get completion */
        frag->base.des_flags  = MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

        /* start the get */
        rc = mca_btl_ugni_post (endpoint, true, size, frag->base.super.ptr, hdr.eager.address,
                                &frag->memory_handle, &hdr.eager.memory_handle,
                                MCA_BTL_NO_ORDER, mca_btl_ugni_callback_eager_get, frag, NULL);
        if (OPAL_UNLIKELY(OPAL_SUCCESS == rc)) {
            return OPAL_SUCCESS;
        }
    } while (0);

    OPAL_THREAD_LOCK(&ugni_module->eager_get_pending_lock);
    opal_list_append (&ugni_module->eager_get_pending, (opal_list_item_t *) frag);
    OPAL_THREAD_UNLOCK(&ugni_module->eager_get_pending_lock);

    return rc;
}
