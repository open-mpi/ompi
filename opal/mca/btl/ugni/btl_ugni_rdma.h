/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
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
#if 0
    frag->post_desc.base.rdma_mode       = GNI_RDMAMODE_FENCE;
#endif
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

    OPAL_THREAD_LOCK(&frag->endpoint->common->dev->dev_lock);
    rc = GNI_PostFma (frag->endpoint->rdma_ep_handle, &frag->post_desc.base);
    OPAL_THREAD_UNLOCK(&frag->endpoint->common->dev->dev_lock);
    if (GNI_RC_SUCCESS != rc) {
        BTL_VERBOSE(("GNI_PostFma failed with gni rc: %d", rc));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    return OPAL_SUCCESS;
}

static void mca_btl_ugni_write_to_self_complete(struct mca_btl_ugni_base_frag_t *frag, int rc)
{
    frag->flags |= MCA_BTL_UGNI_FRAG_COMPLETE;

    BTL_VERBOSE(("cqwrite  frag complete"));
#if 0
    fprintf(stderr,"returning cq_frag %p\n",frag);
#endif
    mca_btl_ugni_frag_return (frag);
}

static inline int mca_btl_ugni_post_bte (mca_btl_ugni_base_frag_t *frag, gni_post_type_t op_type,
                                         mca_btl_ugni_segment_t *lcl_seg, mca_btl_ugni_segment_t *rem_seg)
{
    int rc;
    gni_return_t status;
    mca_btl_ugni_base_frag_t *cq_frag = NULL;
    extern void *howards_start_addr;

    /* Post descriptor */
#if 0
    if (howards_progress_var && (getenv("GENERATE_RDMA_IRQS") != NULL)) {
        fprintf(stderr,"Calling GNI_PostRdma with to trigger interrupt on rdma_local_irq_cq %p\n",frag->endpoint->btl->rdma_local_irq_cq);
        init_gni_post_desc (frag, op_type, lcl_seg->base.seg_addr.lval, lcl_seg->memory_handle,
                            rem_seg->base.seg_addr.lval, rem_seg->memory_handle, lcl_seg->base.seg_len,
                            frag->endpoint->btl->rdma_local_irq_cq);
    } else {
#endif
        init_gni_post_desc (frag, op_type, lcl_seg->base.seg_addr.lval, lcl_seg->memory_handle,
                            rem_seg->base.seg_addr.lval, rem_seg->memory_handle, lcl_seg->base.seg_len,
                            frag->endpoint->btl->rdma_local_cq);
#if 0
    }
#endif

    OPAL_THREAD_LOCK(&frag->endpoint->common->dev->dev_lock);
    status = GNI_PostRdma (frag->endpoint->rdma_ep_handle, &frag->post_desc.base);
    OPAL_THREAD_UNLOCK(&frag->endpoint->common->dev->dev_lock);
    if (GNI_RC_SUCCESS != status) {
        BTL_VERBOSE(("GNI_PostRdma failed with gni rc: %d", status));
        return opal_common_rc_ugni_to_opal(status);
    }

    if (howards_progress_var && (getenv("GENERATE_RDMA_IRQS") != NULL)) {

        rc = mca_btl_ugni_frag_alloc(frag->endpoint,
                                     &frag->endpoint->btl->rdma_frags,
                                     &cq_frag);
#if 0
        fprintf(stderr,"allocated cq_frag %p\n",cq_frag);
#endif
        if (rc == OPAL_SUCCESS) {
            cq_frag->registration = NULL;
            cq_frag->post_desc.base.type = GNI_POST_RDMA_PUT;
            cq_frag->post_desc.base.length  = 4;
            cq_frag->post_desc.base.remote_addr  = (uint64_t)howards_start_addr;
            cq_frag->post_desc.base.remote_mem_hndl = mca_btl_ugni_component.modules[0].device->smsg_irq_mhndl;
            cq_frag->post_desc.base.local_addr  = (uint64_t)howards_start_addr;
            cq_frag->post_desc.base.cq_mode = GNI_CQMODE_REMOTE_EVENT | GNI_CQMODE_GLOBAL_EVENT;
            cq_frag->post_desc.base.dlvr_mode = GNI_DLVMODE_IN_ORDER;
            cq_frag->post_desc.base.src_cq_hndl = mca_btl_ugni_component.modules[0].rdma_local_cq;
            cq_frag->post_desc.base.rdma_mode = 0;
            cq_frag->post_desc.base.local_mem_hndl = mca_btl_ugni_component.modules[0].device->smsg_irq_mhndl;
            cq_frag->post_desc.base.post_id = 0xFFFF;
            cq_frag->post_desc.tries = 0;
            cq_frag->cbfunc = mca_btl_ugni_write_to_self_complete;
            OPAL_THREAD_LOCK(&frag->endpoint->common->dev->dev_lock);
            status = GNI_PostRdma(mca_btl_ugni_component.modules[0].local_ep,&cq_frag->post_desc.base);
            OPAL_THREAD_UNLOCK(&frag->endpoint->common->dev->dev_lock);
            if (status == GNI_RC_ERROR_RESOURCE) {   /* errors for PostCqWrite treated as non-fatal */
                fprintf(stderr,"GNI_PostRdma returned %s\n",gni_err_str[status]);
                mca_btl_ugni_frag_return (cq_frag);
            } else {
                if (status != GNI_RC_SUCCESS) {
                     fprintf(stderr,"GNI_PostRdma returned %s\n",gni_err_str[status]);
                }
            }
        }
    }

    return OPAL_SUCCESS;
}

static inline int mca_btl_ugni_post (mca_btl_ugni_base_frag_t *frag, bool get, mca_btl_ugni_segment_t *lcl_seg,
                                     mca_btl_ugni_segment_t *rem_seg) {
    const gni_post_type_t fma_ops[2] = {GNI_POST_FMA_PUT, GNI_POST_FMA_GET};
    const gni_post_type_t rdma_ops[2] = {GNI_POST_RDMA_PUT, GNI_POST_RDMA_GET};

    if (frag->base.des_local->seg_len <= mca_btl_ugni_component.ugni_fma_limit) {
        return mca_btl_ugni_post_fma (frag, fma_ops[get], lcl_seg, rem_seg);
    }

    return mca_btl_ugni_post_bte (frag, rdma_ops[get], lcl_seg, rem_seg);
}

static inline void mca_btl_ugni_repost (mca_btl_ugni_base_frag_t *frag) {
    gni_return_t grc;

    OPAL_THREAD_LOCK(&frag->endpoint->common->dev->dev_lock);
    if (GNI_POST_RDMA_PUT == frag->post_desc.base.type ||
        GNI_POST_RDMA_GET == frag->post_desc.base.type) {
        grc = GNI_PostRdma (frag->endpoint->rdma_ep_handle, &frag->post_desc.base);
    } else {
        grc = GNI_PostFma (frag->endpoint->rdma_ep_handle, &frag->post_desc.base);
    }
    OPAL_THREAD_UNLOCK(&frag->endpoint->common->dev->dev_lock);

    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != grc)) {
        /* NTH: Should we even retry these? When this code was written there was no indication
         * whether an error in post is recoverable. Clobber this code and the associated data
         * structures if post errors are not recoverable. */
        OPAL_THREAD_LOCK(&frag->endpoint->btl->failed_frags_lock);
        opal_list_append (&frag->endpoint->btl->failed_frags, (opal_list_item_t *) frag);
        OPAL_THREAD_UNLOCK(&frag->endpoint->btl->failed_frags_lock);
    }
}

#endif /* MCA_BTL_UGNI_RDMA_H */
