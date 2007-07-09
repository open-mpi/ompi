/*
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_TEMPLATE_FRAG_H
#define MCA_BTL_TEMPLATE_FRAG_H


#define MCA_BTL_TEMPLATE_FRAG_ALIGN (8)
#include "ompi_config.h"
#include "btl_elan.h" 

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define MCA_BTL_ELAN_HDR_TYPE_SEND     1
#define MCA_BTL_ELAN_HDR_TYPE_PUT      2
#define MCA_BTL_ELAN_HDR_TYPE_GET      3
#define MCA_BTL_ELAN_HDR_TYPE_MATCH    4
#define MCA_BTL_ELAN_HDR_TYPE_FRAG     5
#define MCA_BTL_ELAN_HDR_TYPE_ACK      6
#define MCA_BTL_ELAN_HDR_TYPE_NACK     7
#define MCA_BTL_ELAN_HDR_TYPE_FIN      8
#define MCA_BTL_ELAN_HDR_TYPE_FIN_ACK  9
#define MCA_BTL_ELAN_HDR_TYPE_RECV     10


/**
 * TEMPLATE send fraelanent derived type.
 */
struct mca_btl_elan_frag_t {
    mca_btl_base_descriptor_t base; 
    mca_btl_base_segment_t segment; 
    struct mca_btl_base_endpoint_t *endpoint; 
    struct mca_btl_elan_module_t* btl;
    int type;
    ompi_free_list_t* my_list;
    mca_btl_base_tag_t tag;
    size_t size; 
#if defined MCA_BTL_HAS_MPOOL
    struct mca_mpool_base_registration_t* registration;
#endif
}; 
typedef struct mca_btl_elan_frag_t mca_btl_elan_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_elan_frag_t); 

typedef struct mca_btl_elan_frag_t mca_btl_elan_frag_eager_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_elan_frag_eager_t); 

typedef struct mca_btl_elan_frag_t mca_btl_elan_frag_max_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_elan_frag_max_t); 

typedef struct mca_btl_elan_frag_t mca_btl_elan_frag_user_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_elan_frag_user_t); 


/*
 * Macros to allocate/return descriptors from module specific
 * free list(s).
 */

#define MCA_BTL_TEMPLATE_FRAG_ALLOC_EAGER(frag, rc)           \
{                                                                  \
    ompi_free_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&mca_btl_elan_component.elan_frag_eager, item, rc); \
    frag = (mca_btl_elan_frag_t*) item;                        \
    frag->segment.seg_addr.pval = (void*)(frag+1);           \
    frag->my_list = &mca_btl_elan_component.elan_frag_eager;            \
}


#define MCA_BTL_TEMPLATE_FRAG_ALLOC_MAX(frag, rc)             \
{                                                                  \
    ompi_free_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&mca_btl_elan_component.elan_frag_max, item, rc); \
    frag = (mca_btl_elan_frag_t*) item;                        \
    frag->segment.seg_addr.pval = (void*)(frag+1);           \
    frag->my_list = &mca_btl_elan_component.elan_frag_max;      \
}


#define MCA_BTL_TEMPLATE_FRAG_ALLOC_USER(frag, rc)            \
{                                                                  \
    ompi_free_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&mca_btl_elan_component.elan_frag_user, item, rc); \
    frag = (mca_btl_elan_frag_t*) item;                        \
    frag->my_list = &mca_btl_elan_component.elan_frag_user;    \
}

#define MCA_BTL_TEMPLATE_FRAG_RETURN(frag)               \
{                                                                  \
    OMPI_FREE_LIST_RETURN(frag->my_list,                 \
        (ompi_free_list_item_t*)(frag));                    \
}


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
