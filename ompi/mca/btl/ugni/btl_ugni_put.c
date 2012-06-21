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

    BTL_VERBOSE(("Using RDMA/FMA Put for frag %p", (void *) des));

    /* cause endpoint to bind if it isn't already (bind is sufficient for rdma) */
    (void) mca_btl_ugni_check_endpoint_state(endpoint);

    des->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    return mca_btl_ugni_post (frag, false, (mca_btl_ugni_segment_t *) des->des_src,
                              (mca_btl_ugni_segment_t *) des->des_dst);
}
