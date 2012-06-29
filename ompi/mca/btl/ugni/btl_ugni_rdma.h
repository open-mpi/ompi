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

static inline void init_gni_post_desc (mca_btl_ugni_base_frag_t *frag,
                                      gni_post_type_t op_type,
                                      uint64_t lcl_addr, 
                                      gni_mem_handle_t lcl_mdh, 
                                      uint64_t rem_addr,
                                      gni_mem_handle_t rem_mdh,
                                      uint64_t bufsize,
                                      gni_cq_handle_t cq_hndl) {
    frag->post_desc.base.type            = op_type;
    frag->post_desc.base.cq_mode         = GNI_CQMODE_GLOBAL_EVENT;
    frag->post_desc.base.dlvr_mode       = GNI_DLVMODE_PERFORMANCE;
    frag->post_desc.base.local_addr      = (uint64_t) lcl_addr;
    frag->post_desc.base.local_mem_hndl  = lcl_mdh;
    frag->post_desc.base.remote_addr     = (uint64_t) rem_addr;
    frag->post_desc.base.remote_mem_hndl = rem_mdh;
    frag->post_desc.base.length          = bufsize;
    frag->post_desc.base.rdma_mode       = 0;
    frag->post_desc.base.src_cq_hndl     = cq_hndl;
    frag->post_desc.tries                = 0;
}

static inline int mca_btl_ugni_post_fma (mca_btl_ugni_base_frag_t *frag, gni_post_type_t op_type,
                                         mca_btl_ugni_segment_t *lcl_seg, mca_btl_ugni_segment_t *rem_seg)
{
    gni_return_t rc;

    /* Post descriptor (CQ is ignored for FMA transactions) */
    init_gni_post_desc (frag, op_type, lcl_seg->base.seg_addr.lval, lcl_seg->memory_handle,
                        rem_seg->base.seg_addr.lval, rem_seg->memory_handle, lcl_seg->base.seg_len, 0);

    rc = GNI_PostFma (frag->endpoint->rdma_ep_handle, &frag->post_desc.base);
    if (GNI_RC_SUCCESS != rc) {
        BTL_VERBOSE(("GNI_PostFma failed with gni rc: %d", rc));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}

static inline int mca_btl_ugni_post_bte (mca_btl_ugni_base_frag_t *frag, gni_post_type_t op_type,
                                         mca_btl_ugni_segment_t *lcl_seg, mca_btl_ugni_segment_t *rem_seg)
{
    gni_return_t rc;

    /* Post descriptor */
    init_gni_post_desc (frag, op_type, lcl_seg->base.seg_addr.lval, lcl_seg->memory_handle,
                        rem_seg->base.seg_addr.lval, rem_seg->memory_handle, lcl_seg->base.seg_len,
                        frag->endpoint->btl->rdma_local_cq);

    rc = GNI_PostRdma (frag->endpoint->rdma_ep_handle, &frag->post_desc.base);
    if (GNI_RC_SUCCESS != rc) {
        BTL_VERBOSE(("GNI_PostRdma failed with gni rc: %d", rc));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}

static inline int mca_btl_ugni_post_wcb (mca_btl_ugni_base_frag_t *frag, bool get, mca_btl_ugni_segment_t *lcl_seg,
                                         mca_btl_ugni_segment_t *rem_seg, frag_cb_t cb) {
    frag->cbfunc = cb;

    if (frag->base.des_src->seg_len <= mca_btl_ugni_component.ugni_fma_limit) {
        return mca_btl_ugni_post_fma (frag, get ? GNI_POST_FMA_GET : GNI_POST_FMA_PUT, lcl_seg, rem_seg);
    }

    return mca_btl_ugni_post_bte (frag, get ? GNI_POST_RDMA_GET : GNI_POST_RDMA_PUT, lcl_seg, rem_seg);
}

static inline int mca_btl_ugni_post (mca_btl_ugni_base_frag_t *frag, bool get, mca_btl_ugni_segment_t *lcl_seg,
                                     mca_btl_ugni_segment_t *rem_seg) {
    return mca_btl_ugni_post_wcb (frag, get, lcl_seg, rem_seg, mca_btl_ugni_frag_complete);
}

static inline void mca_btl_ugni_repost (mca_btl_ugni_base_frag_t *frag, int rc) {
    gni_return_t grc;

    frag->cbfunc = mca_btl_ugni_frag_complete;

    if (GNI_POST_RDMA_PUT == frag->post_desc.base.type ||
        GNI_POST_RDMA_GET == frag->post_desc.base.type) {
        grc = GNI_PostRdma (frag->endpoint->rdma_ep_handle, &frag->post_desc.base);
    } else {
        grc = GNI_PostFma (frag->endpoint->rdma_ep_handle, &frag->post_desc.base);
    }

    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != grc)) {
        frag->cbfunc = mca_btl_ugni_repost;
        opal_list_append (&frag->endpoint->btl->failed_frags, (opal_list_item_t *) frag);
    }
}

#endif /* MCA_BTL_UGNI_RDMA_H */
