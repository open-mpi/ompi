/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_BTL_PORTALS4_FRAG_H
#define OPAL_BTL_PORTALS4_FRAG_H

#include "opal/mca/btl/btl.h"

BEGIN_C_DECLS

struct mca_btl_portals4_segment_t {
  mca_btl_base_segment_t base; 
  ptl_match_bits_t key;
};
typedef struct mca_btl_portals4_segment_t mca_btl_portals4_segment_t;

/**
 * Portals send fragment derived type
 */
struct mca_btl_portals4_frag_t {
    mca_btl_base_descriptor_t base; 
    mca_btl_portals4_segment_t segments[1]; 
    /* needed for retransmit case */
   struct mca_btl_base_endpoint_t *endpoint; 
    /* needed for retransmit case */
    mca_btl_base_header_t hdr;
    /* handle to use for communication */
    ptl_handle_me_t me_h;
    /* handle to use for communication */
    ptl_handle_md_t md_h;
    /* size of the allocated memory region -- not the amount of data
       we need to send */
    size_t size; 
    /* match bits for retransmit case */
    ptl_match_bits_t match_bits;
    /* length for retransmit case */
    ptl_size_t length;
    /* length for retransmit case */
    ptl_process_t peer_proc;

    /* the callback and context to complete an RDMA operation */
    struct {
        mca_btl_base_rdma_completion_fn_t func;
        void *context;
        void *data;
        mca_btl_base_registration_handle_t *local_handle;
    } rdma_cb;

    enum { BTL_PORTALS4_FRAG_TYPE_EAGER, 
           BTL_PORTALS4_FRAG_TYPE_MAX,
           BTL_PORTALS4_FRAG_TYPE_USER } type;
    unsigned char  data[16];
};
typedef struct mca_btl_portals4_frag_t mca_btl_portals4_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_portals4_frag_t);

typedef struct mca_btl_portals4_frag_t mca_btl_portals4_frag_eager_t; 
OBJ_CLASS_DECLARATION(mca_btl_portals4_frag_eager_t); 

typedef struct mca_btl_portals4_frag_t mca_btl_portals4_frag_max_t; 
OBJ_CLASS_DECLARATION(mca_btl_portals4_frag_max_t); 

typedef struct mca_btl_portals4_frag_t mca_btl_portals4_frag_user_t; 
OBJ_CLASS_DECLARATION(mca_btl_portals4_frag_user_t); 

/*
 * Macros to allocate/return descriptors from module specific
 * free list(s).
 */
#define OPAL_BTL_PORTALS4_FRAG_ALLOC_EAGER(btl_macro, frag)                                \
{                                                                                          \
    frag = (mca_btl_portals4_frag_t *)                                                     \
        opal_free_list_get (&((mca_btl_portals4_module_t*)btl_macro)->portals_frag_eager); \
    if (NULL == frag) {                                                                    \
        OPAL_BTL_PORTALS4_FRAG_ALLOC_MAX(btl_macro, frag);                                 \
    }                                                                                      \
}


#define OPAL_BTL_PORTALS4_FRAG_RETURN_EAGER(btl_macro, frag)                            \
{                                                                                       \
    assert(BTL_PORTALS4_FRAG_TYPE_EAGER == frag->type);                                 \
    opal_free_list_return (&((mca_btl_portals4_module_t*)btl_macro)->portals_frag_eager, \
        (opal_free_list_item_t*)(frag));                                                \
}


#define OPAL_BTL_PORTALS4_FRAG_ALLOC_MAX(btl_macro, frag)                                           \
{                                                                                                   \
    frag = (mca_btl_portals4_frag_t*)                                                               \
        opal_free_list_get (&((mca_btl_portals4_module_t*)btl_macro)->portals_frag_max);            \
}


#define OPAL_BTL_PORTALS4_FRAG_RETURN_MAX(btl_macro, frag)                            \
{                                                                                     \
    assert(BTL_PORTALS4_FRAG_TYPE_MAX == frag->type);                                 \
    opal_free_list_return (&((mca_btl_portals4_module_t*)btl_macro)->portals_frag_max, \
        (opal_free_list_item_t*)(frag));                                              \
}


#define OPAL_BTL_PORTALS4_FRAG_ALLOC_USER(btl_macro, frag)                                      \
{                                                                                               \
    frag = (mca_btl_portals4_frag_t*)                                                           \
        opal_free_list_get (&((mca_btl_portals4_module_t*)btl_macro)->portals_frag_user);       \
    frag->base.des_cbfunc = NULL;                                                               \
}


#define OPAL_BTL_PORTALS4_FRAG_RETURN_USER(btl_macro, frag)                            \
{                                                                                      \
    assert(BTL_PORTALS4_FRAG_TYPE_USER == frag->type);                                 \
    opal_free_list_return (&((mca_btl_portals4_module_t*)btl_macro)->portals_frag_user, \
        (opal_free_list_item_t*)(frag));                                               \
}


END_C_DECLS
#endif
