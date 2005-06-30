/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifndef MCA_BTL_GM_FRAG_H
#define MCA_BTL_GM_FRAG_H


#define MCA_BTL_GM_FRAG_ALIGN (8)
#include "ompi_config.h"
#include "btl_gm.h" 

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_btl_gm_frag_t);



/**
 * GM send fragment derived type.
 */
struct mca_btl_gm_frag_t {
    mca_btl_base_descriptor_t base; 
    mca_btl_base_segment_t segment; 
    struct mca_btl_gm_module_t* btl;
    struct mca_btl_base_endpoint_t *endpoint; 
    struct mca_mpool_base_registration_t* registration;
    mca_btl_base_header_t *hdr;
    size_t size; 
}; 
typedef struct mca_btl_gm_frag_t mca_btl_gm_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_gm_frag_t); 


typedef struct mca_btl_gm_frag_t mca_btl_gm_frag_eager_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_gm_frag_eager_t); 

typedef struct mca_btl_gm_frag_t mca_btl_gm_frag_max_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_gm_frag_max_t); 

typedef struct mca_btl_gm_frag_t mca_btl_gm_frag_user_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_gm_frag_user_t); 


/*
 * Macros to allocate/return descriptors from module specific
 * free list(s).
 */

#define MCA_BTL_GM_FRAG_ALLOC_EAGER(btl, frag, rc)                 \
{                                                                  \
                                                                   \
    ompi_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_gm_module_t*)btl)->gm_frag_eager, item, rc); \
    frag = (mca_btl_gm_frag_t*) item;                              \
}

#define MCA_BTL_GM_FRAG_RETURN_EAGER(btl, frag)                    \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_gm_module_t*)btl)->gm_frag_eager, \
        (ompi_list_item_t*)(frag));                                \
}

#define MCA_BTL_GM_FRAG_ALLOC_MAX(btl, frag, rc)                   \
{                                                                  \
                                                                   \
    ompi_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_gm_module_t*)btl)->gm_frag_max, item, rc); \
    frag = (mca_btl_gm_frag_t*) item;                              \
}

#define MCA_BTL_GM_FRAG_RETURN_MAX(btl, frag)                      \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_gm_module_t*)btl)->gm_frag_max, \
        (ompi_list_item_t*)(frag));                                \
}


#define MCA_BTL_GM_FRAG_ALLOC_USER(btl, frag, rc)                  \
{                                                                  \
    ompi_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_gm_module_t*)btl)->gm_frag_user, item, rc); \
    frag = (mca_btl_gm_frag_t*) item;                              \
}

#define MCA_BTL_GM_FRAG_RETURN_USER(btl, frag)                     \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_gm_module_t*)btl)->gm_frag_user, \
        (ompi_list_item_t*)(frag)); \
}



#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
