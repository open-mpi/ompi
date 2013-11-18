/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

typedef struct mca_btl_ugni_segment_t {
    mca_btl_base_segment_t base;
    gni_mem_handle_t       memory_handle;
    uint8_t                extra_bytes[3];
    uint8_t                extra_byte_count;
} mca_btl_ugni_segment_t;

typedef struct mca_btl_ugni_send_frag_hdr_t {
    uint32_t lag;
} mca_btl_ugni_send_frag_hdr_t;

typedef struct mca_btl_ugni_send_ex_frag_hdr_t {
    mca_btl_ugni_send_frag_hdr_t send;
    uint8_t                      pml_header[128];
} mca_btl_ugni_send_ex_frag_hdr_t;

typedef struct mca_btl_ugni_rdma_frag_hdr_t {
    void *ctx;
} mca_btl_ugni_rdma_frag_hdr_t;

typedef struct mca_btl_ugni_eager_frag_hdr_t {
    mca_btl_ugni_send_frag_hdr_t send;
    mca_btl_ugni_segment_t src_seg;
    void *ctx;
} mca_btl_ugni_eager_frag_hdr_t;

typedef struct mca_btl_ugni_eager_ex_frag_hdr_t {
    mca_btl_ugni_eager_frag_hdr_t eager;
    uint8_t                       pml_header[128];
} mca_btl_ugni_eager_ex_frag_hdr_t;

typedef union mca_btl_ugni_frag_hdr_t {
    mca_btl_ugni_send_frag_hdr_t     send;
    mca_btl_ugni_send_ex_frag_hdr_t  send_ex;
    mca_btl_ugni_rdma_frag_hdr_t     rdma;
    mca_btl_ugni_eager_frag_hdr_t    eager;
    mca_btl_ugni_eager_ex_frag_hdr_t eager_ex;
} mca_btl_ugni_frag_hdr_t;

enum {
    MCA_BTL_UGNI_FRAG_BUFFERED      = 1, /* frag data is buffered */
    MCA_BTL_UGNI_FRAG_COMPLETE      = 2, /* smsg complete for frag */
    MCA_BTL_UGNI_FRAG_EAGER         = 4, /* eager get frag */
    MCA_BTL_UGNI_FRAG_IGNORE        = 8, /* ignore local smsg completion */
    MCA_BTL_UGNI_FRAG_SMSG_COMPLETE = 16 /* SMSG has completed for this message */
};

struct mca_btl_ugni_base_frag_t;

typedef void (*frag_cb_t) (struct mca_btl_ugni_base_frag_t *, int);

typedef struct mca_btl_ugni_base_frag_t {
    mca_btl_base_descriptor_t    base;
    uint32_t                     msg_id;
    uint16_t                     hdr_size;
    uint16_t                     flags;
    mca_btl_ugni_frag_hdr_t      hdr;
    mca_btl_ugni_segment_t       segments[2];
    ompi_common_ugni_post_desc_t post_desc;
    mca_btl_base_endpoint_t     *endpoint;
    mca_btl_ugni_reg_t          *registration;
    ompi_free_list_t            *my_list;
    frag_cb_t                    cbfunc;
} mca_btl_ugni_base_frag_t;

typedef struct mca_btl_ugni_base_frag_t mca_btl_ugni_smsg_frag_t;
typedef struct mca_btl_ugni_base_frag_t mca_btl_ugni_rdma_frag_t;
typedef struct mca_btl_ugni_base_frag_t mca_btl_ugni_eager_frag_t;

#define MCA_BTL_UGNI_DESC_TO_FRAG(desc) \
    ((mca_btl_ugni_base_frag_t *)((uintptr_t) (desc) - offsetof (mca_btl_ugni_base_frag_t, post_desc)))

OBJ_CLASS_DECLARATION(mca_btl_ugni_smsg_frag_t);
OBJ_CLASS_DECLARATION(mca_btl_ugni_rdma_frag_t);
OBJ_CLASS_DECLARATION(mca_btl_ugni_eager_frag_t);

void mca_btl_ugni_frag_init (mca_btl_ugni_base_frag_t *frag, mca_btl_ugni_module_t *ugni_module);

static inline int mca_btl_ugni_frag_alloc (mca_btl_base_endpoint_t *ep,
                                           ompi_free_list_t *list,
                                           mca_btl_ugni_base_frag_t **frag)
{
    ompi_free_list_item_t *item = NULL;

    OMPI_FREE_LIST_GET_MT(list, item);
    *frag = (mca_btl_ugni_base_frag_t *) item;
    if (OPAL_LIKELY(NULL != item)) {
        (*frag)->my_list  = list;
        (*frag)->endpoint = ep;
        return OMPI_SUCCESS;
    }

    return OMPI_ERR_OUT_OF_RESOURCE;
}

static inline int mca_btl_ugni_frag_return (mca_btl_ugni_base_frag_t *frag)
{
    if (frag->registration) {
        frag->endpoint->btl->super.btl_mpool->mpool_deregister(frag->endpoint->btl->super.btl_mpool,
                                                               (mca_mpool_base_registration_t *) frag->registration);
        frag->registration = NULL;
    }

    frag->flags = 0;

    OMPI_FREE_LIST_RETURN_MT(frag->my_list, (ompi_free_list_item_t *) frag);

    return OMPI_SUCCESS;
}

static inline void mca_btl_ugni_frag_complete (mca_btl_ugni_base_frag_t *frag, int rc) {
    frag->flags |= MCA_BTL_UGNI_FRAG_COMPLETE;

    BTL_VERBOSE(("frag complete. flags = %d", frag->base.des_flags));

    /* call callback if specified */
    if (frag->base.des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
        frag->base.des_cbfunc(&frag->endpoint->btl->super, frag->endpoint, &frag->base, rc);
    }

    if (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP) {
        mca_btl_ugni_frag_return (frag);
    }
}

#define MCA_BTL_UGNI_FRAG_ALLOC_SMSG(ep, frag) \
    mca_btl_ugni_frag_alloc((ep), &(ep)->btl->smsg_frags, &(frag))
#define MCA_BTL_UGNI_FRAG_ALLOC_RDMA(ep, frag) \
    mca_btl_ugni_frag_alloc((ep), &(ep)->btl->rdma_frags, &(frag))
#define MCA_BTL_UGNI_FRAG_ALLOC_RDMA_INT(ep, frag) \
    mca_btl_ugni_frag_alloc((ep), &(ep)->btl->rdma_int_frags, &(frag))
#define MCA_BTL_UGNI_FRAG_ALLOC_EAGER_SEND(ep, frag) \
    mca_btl_ugni_frag_alloc((ep), &(ep)->btl->eager_frags_send, &(frag))
#define MCA_BTL_UGNI_FRAG_ALLOC_EAGER_RECV(ep, frag) \
    mca_btl_ugni_frag_alloc((ep), &(ep)->btl->eager_frags_recv, &(frag))

#endif /* MCA_BTL_UGNI_FRAG_H */
