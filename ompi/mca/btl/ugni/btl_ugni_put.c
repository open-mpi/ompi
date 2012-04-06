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

    if (frag->base.des_src->seg_len <= mca_btl_ugni_component.ugni_fma_limit) {
        return mca_btl_ugni_post_fma (frag, GNI_POST_FMA_PUT, des->des_src, des->des_dst);
    }

    return mca_btl_ugni_post_bte (frag, GNI_POST_RDMA_PUT, des->des_src, des->des_dst);
}
