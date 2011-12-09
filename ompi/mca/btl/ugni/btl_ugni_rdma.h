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

#if !defined(MCA_BTL_UGNI_RDMA_H)
#define MCA_BTL_UGNI_RDMA_H

#include "btl_ugni.h"
#include "btl_ugni_frag.h"
#include "btl_ugni_endpoint.h"

static inline void
mca_btl_ugni_post_frag_complete (ompi_common_ugni_post_desc_t *desc, int rc) {
    mca_btl_ugni_base_frag_t *frag = MCA_BTL_UGNI_DESC_TO_FRAG(desc);

    /* always call put/get callback */
    frag->base.des_cbfunc(&frag->btl->super, frag->endpoint, &frag->base, rc);

    if (OPAL_LIKELY(frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
        MCA_BTL_UGNI_FRAG_RETURN(frag);
    }
}

static inline int init_gni_post_desc(mca_btl_ugni_base_frag_t *frag,
                                     mca_btl_base_endpoint_t *ep,
                                     gni_post_type_t op_type,
                                     uint64_t lcl_addr, 
                                     gni_mem_handle_t *lcl_mdh, 
                                     uint64_t rem_addr,
                                     gni_mem_handle_t *rem_mdh,
                                     uint64_t bufsize,
                                     gni_cq_handle_t cq_hndl) {
    frag->post_desc.base.type = op_type;
    frag->post_desc.base.cq_mode = GNI_CQMODE_GLOBAL_EVENT;
    frag->post_desc.base.dlvr_mode = GNI_DLVMODE_PERFORMANCE;
    frag->post_desc.base.local_addr = (uint64_t) lcl_addr;
    frag->post_desc.base.local_mem_hndl = *lcl_mdh;
    frag->post_desc.base.remote_addr = (uint64_t) rem_addr;
    frag->post_desc.base.remote_mem_hndl = *rem_mdh;
    frag->post_desc.base.length = bufsize;
    frag->post_desc.base.rdma_mode = 0;
    frag->post_desc.base.src_cq_hndl = cq_hndl;

    frag->post_desc.cbfunc = mca_btl_ugni_post_frag_complete;
    frag->post_desc.endpoint = ep->common;

    frag->post_desc.tries = 0;

    return 0;
}

static inline int post_fma_descriptor (mca_btl_ugni_base_frag_t *frag, gni_post_type_t op_type,
                                       struct mca_btl_base_endpoint_t *endpoint,
                                       size_t size, void *lcl_buffer, gni_mem_handle_t lcl_hdl,
                                       void *rem_buffer, gni_mem_handle_t rem_hdl)
{
    int rc;

    /* Post descriptor */
    init_gni_post_desc (frag, endpoint, op_type, (uint64_t)lcl_buffer,
                        &lcl_hdl, (uint64_t)rem_buffer, &rem_hdl,
                        size, 0);

    rc = GNI_PostFma (endpoint->common->ep_handle, &frag->post_desc.base);
    if (GNI_RC_SUCCESS != rc) {
        BTL_ERROR(("GNI_PostFma failed with rc = %d", rc));
        assert(rc < 4);
        rc = OMPI_ERR_OUT_OF_RESOURCE; /* ompi_common_rc_ugni_to_ompi (rc);*/
    }

    return rc;
}

static inline int post_bte_descriptor (mca_btl_ugni_base_frag_t *frag, gni_post_type_t op_type,
                                       struct mca_btl_base_endpoint_t *endpoint,
                                       size_t size, void *lcl_buffer, gni_mem_handle_t lcl_hdl,
                                       void *rem_buffer, gni_mem_handle_t rem_hdl) {
    int rc;

    /* Post descriptor */
    init_gni_post_desc (frag, endpoint, op_type, (uint64_t)lcl_buffer,
                        &lcl_hdl, (uint64_t)rem_buffer, &rem_hdl,
                        size, endpoint->btl->bte_local_cq);

    rc = GNI_PostRdma (endpoint->common->ep_handle, &frag->post_desc.base);
    if (GNI_RC_SUCCESS != rc) {
        assert(rc < 4);
        rc = ompi_common_rc_ugni_to_ompi (rc);
        BTL_ERROR(("GNI_PostRdma failed with rc = %d", rc));
    }

    return rc;
}

static inline int mca_btl_ugni_start_reverse_get (struct mca_btl_base_module_t *btl,
                                                  mca_btl_ugni_base_frag_t *frag) {
    /* off alignment/off size. switch to put */
    mca_btl_base_segment_t segments[2];
    uint32_t msg_id = ORTE_PROC_MY_NAME->vpid;
    void *post_desc_ptr = &(frag->post_desc);
    int rc;

    segments[0] = frag->base.des_src[0];
    segments[1] = frag->base.des_dst[0];

    rc = GNI_SmsgSendWTag (frag->endpoint->common->ep_handle, segments,
                           sizeof (segments), &post_desc_ptr, sizeof (void *),
                           msg_id, MCA_BTL_UGNI_TAG_PUT_INIT);
    if (OPAL_UNLIKELY(rc == GNI_RC_NOT_DONE)) {
        BTL_ERROR(("GNI_SmsgSendWTag failed with rc = %d", rc));
        /* send this smsg packet later */
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    /* todo -- on failure try again */
    assert (GNI_RC_SUCCESS == rc);

    return OMPI_SUCCESS;
}

#endif /* MCA_BTL_UGNI_RDMA_H */
