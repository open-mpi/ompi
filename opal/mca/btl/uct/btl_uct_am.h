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

#if !defined(MCA_BTL_UCT_AM_H)
#define MCA_BTL_UCT_AM_H

#include "btl_uct_frag.h"

struct mca_btl_base_descriptor_t *mca_btl_uct_prepare_src (mca_btl_base_module_t *btl,
                                                           mca_btl_base_endpoint_t *endpoint,
                                                           opal_convertor_t *convertor,
                                                           uint8_t order, size_t reserve,
                                                           size_t *size, uint32_t flags);

int mca_btl_uct_sendi (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, opal_convertor_t *convertor,
                       void *header, size_t header_size, size_t payload_size, uint8_t order, uint32_t flags,
                       mca_btl_base_tag_t tag, mca_btl_base_descriptor_t **descriptor);

int mca_btl_uct_send (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, mca_btl_base_descriptor_t *descriptor,
                      mca_btl_base_tag_t tag);

int mca_btl_uct_send_frag (mca_btl_uct_module_t *uct_btl, mca_btl_uct_base_frag_t *frag, bool append);

mca_btl_base_descriptor_t *mca_btl_uct_alloc (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint,
                                              uint8_t order, size_t size, uint32_t flags);

int mca_btl_uct_free (mca_btl_base_module_t *btl, mca_btl_base_descriptor_t *des);


#endif /* !defined(MCA_BTL_UCT_AM_H) */
