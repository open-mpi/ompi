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

#if !defined(MCA_BTL_UGNI_RDMA_H)
#define MCA_BTL_UGNI_RDMA_H

#include "btl_ugni.h"
#include "btl_ugni_frag.h"

int mca_btl_ugni_start_eager_get (mca_btl_base_endpoint_t *ep,
                                  mca_btl_ugni_eager_ex_frag_hdr_t hdr,
                                  mca_btl_ugni_base_frag_t *frag);

static inline int init_gni_post_desc(mca_btl_ugni_base_frag_t *frag,
                                     gni_post_type_t op_type,
                                     uint64_t lcl_addr, 
                                     gni_mem_handle_t *lcl_mdh, 
                                     uint64_t rem_addr,
                                     gni_mem_handle_t *rem_mdh,
                                     uint64_t bufsize,
                                     gni_cq_handle_t cq_hndl) {
    frag->post_desc.base.type            = op_type;
    frag->post_desc.base.cq_mode         = GNI_CQMODE_GLOBAL_EVENT;
    frag->post_desc.base.dlvr_mode       = GNI_DLVMODE_PERFORMANCE;
    frag->post_desc.base.local_addr      = (uint64_t) lcl_addr;
    frag->post_desc.base.local_mem_hndl  = *lcl_mdh;
    frag->post_desc.base.remote_addr     = (uint64_t) rem_addr;
    frag->post_desc.base.remote_mem_hndl = *rem_mdh;
    frag->post_desc.base.length          = bufsize;
    frag->post_desc.base.rdma_mode       = 0;
    frag->post_desc.base.src_cq_hndl     = cq_hndl;

    frag->cbfunc   = mca_btl_ugni_frag_complete;
    frag->post_desc.endpoint = frag->endpoint->common;
    frag->post_desc.tries    = 0;

    return 0;
}

static inline int mca_btl_ugni_post_fma (mca_btl_ugni_base_frag_t *frag, gni_post_type_t op_type,
                                         mca_btl_base_segment_t *lcl_seg, mca_btl_base_segment_t *rem_seg)
{
    int rc;

    /* Post descriptor */
    init_gni_post_desc (frag, op_type, lcl_seg->seg_addr.lval,
                        (gni_mem_handle_t *)&lcl_seg->seg_key.key64,
                        rem_seg->seg_addr.lval, (gni_mem_handle_t *)&rem_seg->seg_key.key64,
                        lcl_seg->seg_len, 0); /* CQ is ignored for FMA transactions */

    rc = GNI_PostFma (frag->endpoint->rdma_ep_handle, &frag->post_desc.base);
    if (GNI_RC_SUCCESS != rc) {
        assert(rc < 4);
        rc = OMPI_ERR_OUT_OF_RESOURCE;
    }

    return rc;
}

static inline int mca_btl_ugni_post_bte (mca_btl_ugni_base_frag_t *frag, gni_post_type_t op_type,
                                         mca_btl_base_segment_t *lcl_seg, mca_btl_base_segment_t *rem_seg)
{
    int rc;

    /* Post descriptor */
    init_gni_post_desc (frag, op_type, lcl_seg->seg_addr.lval,
                        (gni_mem_handle_t *)&lcl_seg->seg_key.key64,
                        rem_seg->seg_addr.lval, (gni_mem_handle_t *)&rem_seg->seg_key.key64,
                        lcl_seg->seg_len, frag->endpoint->btl->rdma_local_cq);

    rc = GNI_PostRdma (frag->endpoint->rdma_ep_handle, &frag->post_desc.base);
    if (GNI_RC_SUCCESS != rc) {
        rc = ompi_common_rc_ugni_to_ompi (rc);
        BTL_ERROR(("GNI_PostRdma failed with rc = %d", rc));
    }

    return rc;
}

#endif /* MCA_BTL_UGNI_RDMA_H */
