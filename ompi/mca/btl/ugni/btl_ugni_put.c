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
#include "ompi_config.h"
#include "opal/include/opal_stdint.h"

#include "btl_ugni_rdma.h"
#include "opal/util/opal_sos.h"

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
    gni_mem_handle_t lcl_hdl, rem_hdl;
    void *lcl_buffer, *rem_buffer;
    size_t size;
    int rc;

    BTL_VERBOSE(("Using RDMA Put"));

    /* Check if endpoint is connected */        
    rc = mca_btl_ugni_check_endpoint_state(endpoint);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        /* we should already be connected by this point (we got an rc send) */
        return rc;
    }

    /* Get local memory handle */
    lcl_buffer = (void*)(des->des_src->seg_addr.pval);
    size  = des->des_src->seg_len;
    memcpy (&lcl_hdl, (void *) des->des_src->seg_key.key64,
            sizeof(gni_mem_handle_t));

    /* Get remote memory handle */
    rem_buffer = (void*)(des->des_dst->seg_addr.pval);
    memcpy (&rem_hdl, (void *) des->des_dst->seg_key.key64,
            sizeof(gni_mem_handle_t));

    frag->tries = 0;

    if (size < mca_btl_ugni_component.btl_fma_limit) {
        rc = post_fma_descriptor (frag, GNI_POST_FMA_PUT, endpoint, size,
                                  lcl_buffer, lcl_hdl, rem_buffer, rem_hdl);
    } else {
        rc = post_bte_descriptor (frag, GNI_POST_RDMA_PUT, endpoint, size,
                                  lcl_buffer, lcl_hdl, rem_buffer, rem_hdl);
    }

    return rc;
}
