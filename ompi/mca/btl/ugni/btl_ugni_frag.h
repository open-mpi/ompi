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

#if !defined(MCA_BTL_UGNI_FRAG_H)
#define MCA_BTL_UGNI_FRAG_H

#include "btl_ugni.h"
#include "btl_ugni_endpoint.h"

typedef struct mca_btl_ugni_send_frag_hdr_t {
    size_t len;
    mca_btl_base_tag_t tag;
} mca_btl_ugni_send_frag_hdr_t;

typedef struct mca_btl_ugni_rdma_frag_hdr_t {
    mca_btl_base_segment_t src_seg;
    mca_btl_base_segment_t dst_seg;
    void *ctx;
} mca_btl_ugni_rdma_frag_hdr_t;

typedef struct mca_btl_ugni_eager_frag_hdr_t {
    size_t len;
    mca_btl_base_tag_t tag;
    mca_btl_base_segment_t src_seg;
    void *ctx;
} mca_btl_ugni_eager_frag_hdr_t;

typedef union mca_btl_ugni_frag_hdr_t {
    mca_btl_ugni_send_frag_hdr_t  send;
    mca_btl_ugni_rdma_frag_hdr_t  rdma;
    mca_btl_ugni_eager_frag_hdr_t eager;
} mca_btl_ugni_frag_hdr_t;

typedef struct mca_btl_ugni_base_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_base_segment_t segments[2];
    mca_btl_ugni_frag_hdr_t hdr;
    ompi_common_ugni_post_desc_t post_desc;
    mca_btl_base_endpoint_t *endpoint;
    mca_btl_ugni_reg_t *registration;
    ompi_free_list_t *my_list;
    void *buffer;
    uint32_t msg_id;
} mca_btl_ugni_base_frag_t;

typedef struct mca_btl_ugni_base_frag_t mca_btl_ugni_smsg_frag_t;
typedef struct mca_btl_ugni_base_frag_t mca_btl_ugni_rdma_frag_t;
typedef struct mca_btl_ugni_base_frag_t mca_btl_ugni_eager_frag_t;

#define MCA_BTL_UGNI_DESC_TO_FRAG(desc) \
    ((mca_btl_ugni_base_frag_t *)((uintptr_t) (desc) - offsetof (mca_btl_ugni_base_frag_t, post_desc)))

OBJ_CLASS_DECLARATION(mca_btl_ugni_smsg_frag_t);
OBJ_CLASS_DECLARATION(mca_btl_ugni_rdma_frag_t);
OBJ_CLASS_DECLARATION(mca_btl_ugni_eager_frag_t);

static inline int mca_btl_ugni_frag_alloc (mca_btl_base_endpoint_t *ep,
                                           ompi_free_list_t *list,
                                           mca_btl_ugni_base_frag_t **frag)
{
    ompi_free_list_item_t *item = NULL;
    int rc;

    OMPI_FREE_LIST_GET(list, item, rc);
    *frag = (mca_btl_ugni_base_frag_t *) item;
    if (OPAL_LIKELY(NULL != item)) {
        (*frag)->my_list  = list;
        (*frag)->endpoint = ep;
    }

    return rc;
}

static inline void mca_btl_ugni_frag_return (mca_btl_ugni_base_frag_t *frag)
{
    if (frag->registration) {
        frag->endpoint->btl->super.btl_mpool->mpool_deregister(frag->endpoint->btl->super.btl_mpool,
                                                               &frag->registration->base);
        frag->registration = NULL;
    }

    OMPI_FREE_LIST_RETURN(frag->my_list, (ompi_free_list_item_t *) frag);
}

#define MCA_BTL_UGNI_FRAG_ALLOC_SMSG(ep, frag) \
    mca_btl_ugni_frag_alloc((ep), &mca_btl_ugni_component.ugni_frags_smsg, &(frag))
#define MCA_BTL_UGNI_FRAG_ALLOC_RDMA(ep, frag) \
    mca_btl_ugni_frag_alloc((ep), &mca_btl_ugni_component.ugni_frags_rdma, &(frag))
#define MCA_BTL_UGNI_FRAG_ALLOC_EAGER_SEND(ep, frag) \
    mca_btl_ugni_frag_alloc((ep), &(ep)->btl->eager_frags_send, &(frag))
#define MCA_BTL_UGNI_FRAG_ALLOC_EAGER_RECV(ep, frag) \
    mca_btl_ugni_frag_alloc((ep), &(ep)->btl->eager_frags_recv, &(frag))

#endif /* MCA_BTL_UGNI_FRAG_H */
