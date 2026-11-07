/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#if !defined(MCA_BTL_OFI_FRAG_H)
#    define MCA_BTL_OFI_FRAG_H

#    include "btl_ofi.h"
#    include "btl_ofi_endpoint.h"

#    define MCA_BTL_OFI_HDR_SIZE  sizeof(mca_btl_ofi_header_t)
#    define MCA_BTL_OFI_FRAG_SIZE (mca_btl_ofi_component.frag_size)
#    define MCA_BTL_OFI_RECV_SIZE MCA_BTL_OFI_FRAG_SIZE + MCA_BTL_OFI_HDR_SIZE
/* eager limit of device buffers when the user sets none (see mca_btl_ofi_init_device) */
#    define MCA_BTL_OFI_ACCELERATOR_EAGER_LIMIT 4096

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
                          mca_btl_ofi_context_t *context, mca_btl_ofi_base_frag_t *frag,
                          int rc);

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

/* The payload of a send has already been copied into the fragment, so once the provider has
 * accepted the send the PML's buffer is free and its request can complete. When the BTL owns the
 * fragment, tell the PML now instead of when the provider completes the send: on a reliable
 * provider that completion waits for the peer's acknowledgement, a round trip that a rank sending
 * and then receiving would otherwise pay on every exchange. The fragment itself is returned to
 * its free list when the provider completes. Errors on the peer still reach the PML through the
 * module's error callback. */
static inline void mca_btl_ofi_frag_send_accepted(mca_btl_ofi_base_frag_t *frag)
{
    mca_btl_base_completion_fn_t cbfunc = frag->base.des_cbfunc;

    if ((frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP) && NULL != cbfunc) {
        frag->base.des_cbfunc = NULL;
        cbfunc(&frag->btl->super, frag->endpoint, &frag->base, OPAL_SUCCESS);
    }
}

static inline void mca_btl_ofi_frag_complete(mca_btl_ofi_base_frag_t *frag, int rc)
{
    mca_btl_ofi_module_t *ofi_btl = frag->btl;

    /* call the local callback if specified */
    if ((frag->base.des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) && NULL != frag->base.des_cbfunc) {
        frag->base.des_cbfunc(&ofi_btl->super, frag->endpoint, &frag->base, rc);
    }

    /* If the BTL has ownership, return it to the free list, */
    if (OPAL_LIKELY(frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
        mca_btl_ofi_frag_return(frag);
    }
}

#endif /* !defined(MCA_BTL_OFI_FRAG_H) */
