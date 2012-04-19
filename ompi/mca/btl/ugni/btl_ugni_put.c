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
#include "ompi_config.h"
#include "opal/include/opal_stdint.h"

#include "btl_ugni_rdma.h"

/**
 * Initiate a put operation.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
int mca_btl_ugni_put (struct mca_btl_base_module_t *btl,
                      struct mca_btl_base_endpoint_t *endpoint,
                      struct mca_btl_base_descriptor_t *des) {
    mca_btl_ugni_base_frag_t *frag = (mca_btl_ugni_base_frag_t *) des;
    int rc;

    BTL_VERBOSE(("Using RDMA/FMA Put"));

    /* Check if endpoint is connected */
    rc = mca_btl_ugni_check_endpoint_state(endpoint);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        /* Ack! We should already be connected by this point (we got an smsg msg) */
        return rc;
    }

    if (NULL != frag->base.des_cbfunc) {
        des->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    }

    if (frag->base.des_src->seg_len <= mca_btl_ugni_component.ugni_fma_limit) {
        return mca_btl_ugni_post_fma (frag, GNI_POST_FMA_PUT, des->des_src, des->des_dst);
    }

    return mca_btl_ugni_post_bte (frag, GNI_POST_RDMA_PUT, des->des_src, des->des_dst);
}

/* reversed get */
static void mca_btl_ugni_callback_put_retry (mca_btl_ugni_base_frag_t *frag, int rc)
{
    (void) mca_btl_ugni_start_put(frag->endpoint, frag->hdr.rdma, frag);
}

int mca_btl_ugni_start_put (mca_btl_base_endpoint_t *ep,
                            mca_btl_ugni_rdma_frag_hdr_t hdr,
                            mca_btl_ugni_base_frag_t *frag)
{
    int rc;

    BTL_VERBOSE(("starting reverse get (put) for remote ctx: %p", hdr.ctx));

    if (NULL == frag) {
        rc = MCA_BTL_UGNI_FRAG_ALLOC_RDMA_INT(ep, frag);
        if (OPAL_UNLIKELY(NULL == frag)) {
            BTL_ERROR(("error allocating rdma frag for reverse get. rc = %d. fl_num_allocated = %d", rc,
                       ep->btl->rdma_int_frags.fl_num_allocated));
            return rc;
        }
    }

    frag->hdr.rdma = hdr;

    frag->base.des_cbfunc = NULL;
    frag->base.des_flags  = MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;

    frag->segments[0] = hdr.src_seg;
    frag->base.des_src = frag->segments;
    frag->base.des_src_cnt = 1;

    frag->segments[1] = hdr.dst_seg;
    frag->base.des_dst = frag->segments + 1;
    frag->base.des_dst_cnt = 1;

    rc = mca_btl_ugni_put (&ep->btl->super, ep, &frag->base);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        frag->cbfunc = mca_btl_ugni_callback_put_retry;
        opal_list_append (&ep->btl->failed_frags, (opal_list_item_t *) frag);
        return rc;
    }

    frag->cbfunc = mca_btl_ugni_callback_rdma_complete;

    return OMPI_SUCCESS;
}
