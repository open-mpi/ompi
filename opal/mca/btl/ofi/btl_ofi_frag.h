/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MCA_BTL_OFI_FRAG_H)
#    define MCA_BTL_OFI_FRAG_H

#    include "btl_ofi.h"
#    include "btl_ofi_endpoint.h"

#    define MCA_BTL_OFI_HDR_SIZE  sizeof(mca_btl_ofi_header_t)
#    define MCA_BTL_OFI_FRAG_SIZE 4096
#    define MCA_BTL_OFI_RECV_SIZE MCA_BTL_OFI_FRAG_SIZE + MCA_BTL_OFI_HDR_SIZE

#    define MCA_BTL_OFI_NUM_SEND_INC(module)                                       \
        OPAL_ATOMIC_ADD_FETCH64(&(module)->outstanding_send, 1);                   \
        if (module->outstanding_send > mca_btl_ofi_component.progress_threshold) { \
            mca_btl_ofi_component.super.btl_progress();                            \
        }

#    define MCA_BTL_OFI_NUM_SEND_DEC(module) \
        OPAL_ATOMIC_ADD_FETCH64(&(module)->outstanding_send, -1);

mca_btl_base_descriptor_t *mca_btl_ofi_alloc(mca_btl_base_module_t *btl,
                                             mca_btl_base_endpoint_t *endpoint, uint8_t order,
                                             size_t size, uint32_t flags);

int mca_btl_ofi_free(mca_btl_base_module_t *btl, mca_btl_base_descriptor_t *des);

int mca_btl_ofi_send(mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint,
                     mca_btl_base_descriptor_t *descriptor, mca_btl_base_tag_t tag);

int mca_btl_ofi_recv_frag(mca_btl_ofi_module_t *ofi_btl, mca_btl_base_endpoint_t *endpoint,
                          mca_btl_ofi_context_t *context, mca_btl_ofi_base_frag_t *frag);

struct mca_btl_base_descriptor_t *mca_btl_ofi_prepare_src(mca_btl_base_module_t *btl,
                                                          mca_btl_base_endpoint_t *endpoint,
                                                          opal_convertor_t *convertor,
                                                          uint8_t order, size_t reserve,
                                                          size_t *size, uint32_t flags);

mca_btl_ofi_frag_completion_t *mca_btl_ofi_frag_completion_alloc(mca_btl_base_module_t *btl,
                                                                 mca_btl_ofi_context_t *context,
                                                                 mca_btl_ofi_base_frag_t *frag,
                                                                 int type);

static inline mca_btl_ofi_base_frag_t *mca_btl_ofi_frag_alloc(mca_btl_ofi_module_t *ofi_btl,
                                                              opal_free_list_t *fl,
                                                              mca_btl_base_endpoint_t *endpoint)
{
    mca_btl_ofi_base_frag_t *frag = (mca_btl_ofi_base_frag_t *) opal_free_list_get(fl);

    if (OPAL_LIKELY(NULL != frag)) {
        frag->free_list = fl;
        frag->endpoint = endpoint;
        frag->btl = ofi_btl;
    }

    return frag;
}

static inline void mca_btl_ofi_frag_return(mca_btl_ofi_base_frag_t *frag)
{
    opal_free_list_return(frag->free_list, &frag->base.super);
}

static inline void mca_btl_ofi_frag_complete(mca_btl_ofi_base_frag_t *frag, int rc)
{
    mca_btl_ofi_module_t *ofi_btl = frag->btl;

    /* call the local callback if specified */
    if (frag->base.des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
        frag->base.des_cbfunc(&ofi_btl->super, frag->endpoint, &frag->base, rc);
    }

    /* If the BTL has ownership, return it to the free list, */
    if (OPAL_LIKELY(frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
        mca_btl_ofi_frag_return(frag);
    }
}

#endif /* !defined(MCA_BTL_OFI_FRAG_H) */
