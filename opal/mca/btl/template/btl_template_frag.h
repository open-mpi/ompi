/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_TEMPLATE_FRAG_H
#define MCA_BTL_TEMPLATE_FRAG_H


#define MCA_BTL_TEMPLATE_FRAG_ALIGN (8)
#include "opal_config.h"
#include "btl_template.h" 

BEGIN_C_DECLS

/**
 * TEMPLATE send fratemplateent derived type.
 */
struct mca_btl_template_frag_t {
    mca_btl_base_descriptor_t base; 
    mca_btl_base_segment_t segment; 
    struct mca_btl_base_endpoint_t *endpoint; 
    mca_btl_base_header_t *hdr;
    size_t size; 
#if MCA_BTL_HAS_MPOOL
    struct mca_mpool_base_registration_t* registration;
#endif
}; 
typedef struct mca_btl_template_frag_t mca_btl_template_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_template_frag_t); 

typedef struct mca_btl_template_frag_t mca_btl_template_frag_eager_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_template_frag_eager_t); 

typedef struct mca_btl_template_frag_t mca_btl_template_frag_max_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_template_frag_max_t); 

typedef struct mca_btl_template_frag_t mca_btl_template_frag_user_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_template_frag_user_t); 


/*
 * Macros to allocate/return descriptors from module specific
 * free list(s).
 */

#define MCA_BTL_TEMPLATE_FRAG_ALLOC_EAGER(btl, frag)               \
{                                                                  \
                                                                   \
    ompi_free_list_item_t *item;                                        \
    OMPI_FREE_LIST_GET_MT(&((mca_btl_template_module_t*)btl)->template_frag_eager, item); \
    frag = (mca_btl_template_frag_t*) item;                        \
}

#define MCA_BTL_TEMPLATE_FRAG_RETURN_EAGER(btl, frag)              \
{                                                                  \
    OMPI_FREE_LIST_RETURN_MT(&((mca_btl_template_module_t*)btl)->template_frag_eager, \
        (ompi_free_list_item_t*)(frag));                                \
}

#define MCA_BTL_TEMPLATE_FRAG_ALLOC_MAX(btl, frag)                 \
{                                                                  \
                                                                   \
    ompi_free_list_item_t *item;                                        \
    OMPI_FREE_LIST_GET_MT(&((mca_btl_template_module_t*)btl)->template_frag_max, item); \
    frag = (mca_btl_template_frag_t*) item;                        \
}

#define MCA_BTL_TEMPLATE_FRAG_RETURN_MAX(btl, frag)                \
{                                                                  \
    OMPI_FREE_LIST_RETURN_MT(&((mca_btl_template_module_t*)btl)->template_frag_max, \
        (ompi_free_list_item_t*)(frag));                                \
}


#define MCA_BTL_TEMPLATE_FRAG_ALLOC_USER(btl, frag)                \
{                                                                  \
    ompi_free_list_item_t *item;                                        \
    OMPI_FREE_LIST_GET_MT(&((mca_btl_template_module_t*)btl)->template_frag_user, item); \
    frag = (mca_btl_template_frag_t*) item;                        \
}

#define MCA_BTL_TEMPLATE_FRAG_RETURN_USER(btl, frag)               \
{                                                                  \
    OMPI_FREE_LIST_RETURN_MT(&((mca_btl_template_module_t*)btl)->template_frag_user, \
        (ompi_free_list_item_t*)(frag)); \
}



END_C_DECLS
#endif
