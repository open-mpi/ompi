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

static inline int mca_btl_ugni_init_reverse_get (struct mca_btl_base_module_t *btl,
                                                  mca_btl_ugni_base_frag_t *frag) {
    /* off alignment/off size. switch to put */
    frag->hdr.rdma.src_seg = frag->base.des_src[0];
    frag->hdr.rdma.dst_seg = frag->base.des_dst[0];
    frag->hdr.rdma.ctx     = (void *) &frag->post_desc;

    /* send the fragment header using smsg. ignore local completion */
    return ompi_mca_btl_ugni_smsg_send (frag, true, &frag->hdr.rdma,
                                        sizeof (frag->hdr.rdma), NULL, 0,
                                        MCA_BTL_UGNI_TAG_PUT_INIT);
}

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
    size_t size = des->des_src->seg_len;
    bool check;
    int rc;

    BTL_VERBOSE(("Using RDMA/FMA Get"));

    /* Check if endpoint is connected */
    rc = mca_btl_ugni_check_endpoint_state(endpoint);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc))
        /* Ack! we should already be connected by this point (we got a smsg msg) */
        return rc;

    /* Check if the get is aligned/sized on a multiple of 4 */
    check = !!((des->des_src->seg_addr.lval | des->des_dst->seg_addr.lval | size) & 3);

    if (OPAL_UNLIKELY(check || size > mca_btl_ugni_component.ugni_get_limit)) {
        /* switch to put */
        return mca_btl_ugni_init_reverse_get (btl, frag);
    }

    if (size <= mca_btl_ugni_component.ugni_fma_limit) {
        return mca_btl_ugni_post_fma (frag, GNI_POST_FMA_GET, des->des_dst, des->des_src);
    }

    return mca_btl_ugni_post_bte (frag, GNI_POST_RDMA_GET, des->des_dst, des->des_src);
}
