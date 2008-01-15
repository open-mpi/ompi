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

#ifndef MCA_BTL_ELAN_FRAG_H
#define MCA_BTL_ELAN_FRAG_H

#include "ompi_config.h"
#include "btl_elan.h" 

BEGIN_C_DECLS

#define MCA_BTL_ELAN_HDR_TYPE_SEND     1
#define MCA_BTL_ELAN_HDR_TYPE_PUT      2
#define MCA_BTL_ELAN_HDR_TYPE_GET      3
#define MCA_BTL_ELAN_HDR_TYPE_RECV     4


/**
 * Elan send fragment derived type.
 */
struct mca_btl_elan_frag_t {
    mca_btl_base_descriptor_t base; 
    mca_btl_base_segment_t segment; 
    struct mca_btl_base_endpoint_t *endpoint; 
    struct mca_btl_elan_module_t* btl;
    int type;
    ompi_free_list_t* my_list;
    mca_btl_base_tag_t tag;
    struct ELAN_EVENT* elan_event;
    size_t size;
    struct mca_mpool_base_registration_t* registration;
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

#define MCA_BTL_ELAN_FRAG_ALLOC_EAGER(frag, rc)                         \
    {                                                                   \
        ompi_free_list_item_t *item;                                    \
        OMPI_FREE_LIST_WAIT(&mca_btl_elan_component.elan_frag_eager, item, rc); \
        frag = (mca_btl_elan_frag_t*) item;                             \
        frag->segment.seg_addr.pval = (void*)(frag+1);                  \
        frag->my_list = &mca_btl_elan_component.elan_frag_eager;        \
    }


#define MCA_BTL_ELAN_FRAG_ALLOC_MAX(frag, rc)                           \
    {                                                                   \
        ompi_free_list_item_t *item;                                    \
        OMPI_FREE_LIST_WAIT(&mca_btl_elan_component.elan_frag_max, item, rc); \
        frag = (mca_btl_elan_frag_t*) item;                             \
        frag->segment.seg_addr.pval = (void*)(frag+1);                  \
        frag->my_list = &mca_btl_elan_component.elan_frag_max;          \
    }


#define MCA_BTL_ELAN_FRAG_ALLOC_USER(frag, rc)                     \
    {                                                                   \
        ompi_free_list_item_t *item;                                    \
        OMPI_FREE_LIST_WAIT(&mca_btl_elan_component.elan_frag_user, item, rc); \
        frag = (mca_btl_elan_frag_t*) item;                             \
        frag->my_list = &mca_btl_elan_component.elan_frag_user;         \
    }

#define MCA_BTL_ELAN_FRAG_RETURN(frag)                             \
    {                                                              \
        OMPI_FREE_LIST_RETURN(frag->my_list,                       \
                              (ompi_free_list_item_t*)(frag));     \
    }


END_C_DECLS

#endif  /* MCA_BTL_ELAN_FRAG_H */
