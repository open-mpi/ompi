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

struct mca_btl_ugni_frag_hdr_t {
    size_t len;
    mca_btl_base_tag_t tag;
};
typedef struct mca_btl_ugni_frag_hdr_t mca_btl_ugni_frag_hdr_t;

struct mca_btl_ugni_base_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_base_segment_t segments[2];
    mca_btl_ugni_frag_hdr_t *hdr;
    mca_btl_base_tag_t tag;
    ompi_common_ugni_post_desc_t post_desc;
    mca_btl_base_endpoint_t *endpoint;
    mca_btl_ugni_reg_t *registration;
    ompi_free_list_t *my_list;
    mca_btl_ugni_module_t *btl;
    int tries;
    uint32_t msg_id;
};

typedef struct mca_btl_ugni_base_frag_t mca_btl_ugni_base_frag_t;
typedef struct mca_btl_ugni_base_frag_t mca_btl_ugni_rdma_frag_t;

#define MCA_BTL_UGNI_DESC_TO_FRAG(desc) ((mca_btl_ugni_base_frag_t *)((uintptr_t) (desc) - offsetof (mca_btl_ugni_base_frag_t, post_desc)))

OBJ_CLASS_DECLARATION(mca_btl_ugni_base_frag_t);
OBJ_CLASS_DECLARATION(mca_btl_ugni_rdma_frag_t);

#define MCA_BTL_UGNI_FRAG_ALLOC_EAGER(module, frag, rc)                     \
    do {                                                                \
        ompi_free_list_item_t *item;                                    \
        OMPI_FREE_LIST_GET(&mca_btl_ugni_component.ugni_frags_eager, item, rc); \
        frag = (mca_btl_ugni_base_frag_t *) item;                       \
        frag->my_list = &mca_btl_ugni_component.ugni_frags_eager;       \
        frag->btl = (module);                                              \
    } while (0)

#define MCA_BTL_UGNI_FRAG_ALLOC_RDMA(module, frag, rc)                     \
    do {                                                                \
        ompi_free_list_item_t *item;                                    \
        OMPI_FREE_LIST_GET(&mca_btl_ugni_component.ugni_frags_rdma, item, rc); \
        frag = (mca_btl_ugni_base_frag_t *) item;                       \
        frag->my_list = &mca_btl_ugni_component.ugni_frags_rdma;        \
        frag->btl = (module);                                              \
    } while (0)

#define MCA_BTL_UGNI_FRAG_RETURN(frag)                                  \
    do {                                                                \
        if (OPAL_UNLIKELY(NULL != (frag)->registration)) {              \
            (frag)->btl->super.btl_mpool->mpool_deregister((frag)->btl->super.btl_mpool, \
                                               &(frag)->registration->base); \
            (frag)->registration = NULL;                                \
        }                                                               \
        OMPI_FREE_LIST_RETURN((frag)->my_list, (ompi_free_list_item_t *)(frag)); \
    } while (0);

#endif /* MCA_BTL_UGNI_FRAG_H */
