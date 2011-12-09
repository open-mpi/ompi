/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni_rdma.h"

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
    gni_mem_handle_t lcl_hdl, rem_hdl;
    void *lcl_buffer, *rem_buffer;
    size_t size;
    int rc;

    BTL_VERBOSE(("Using RDMA Get"));

    /* Check if endpoint is connected */
    rc = mca_btl_ugni_check_endpoint_state(endpoint);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc))
        /* we should already be connected by this point (we got a smsg send) */
        return rc;

    /* Get remote memory handle */
    rem_buffer = (void *)(des->des_src->seg_addr.pval);
    size  = des->des_src->seg_len;
    memcpy (&rem_hdl, (void *) des->des_src->seg_key.key64, sizeof (rem_hdl));

    /* Get local memory handle */
    lcl_buffer = (void *)(des->des_dst->seg_addr.pval);
    memcpy (&lcl_hdl, (void *) des->des_dst->seg_key.key64, sizeof (lcl_hdl));

    if (OPAL_UNLIKELY(((uintptr_t)rem_buffer & 0x3) || ((uintptr_t)lcl_buffer & 0x3) ||
                      size & 0x3 || size > mca_btl_ugni_component.btl_get_limit)) {
        /* switch to put */
        return mca_btl_ugni_start_reverse_get (btl, frag);
    }

    frag->tries = 0;

    if (size < mca_btl_ugni_component.btl_fma_limit) {
        rc = post_fma_descriptor (frag, GNI_POST_FMA_GET, endpoint, size,
                                  lcl_buffer, lcl_hdl, rem_buffer, rem_hdl);
    } else {
        rc = post_bte_descriptor (frag, GNI_POST_RDMA_GET, endpoint, size,
                                  lcl_buffer, lcl_hdl, rem_buffer, rem_hdl);
    }

    return rc;
}
