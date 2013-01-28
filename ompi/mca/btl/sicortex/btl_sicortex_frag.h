/*
 * Copyright (c) 2008-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_SICORTEX_FRAG_H
#define MCA_BTL_SICORTEX_FRAG_H

#include "ompi_config.h"
#include "ompi/class/ompi_free_list.h"
#include "btl_sicortex.h" 

BEGIN_C_DECLS

/**
 * SICORTEX send frasicortexent derived type.
 */
struct mca_btl_sicortex_frag_t {
    mca_btl_base_descriptor_t           base; 
    mca_btl_base_segment_t              segment;
    ompi_free_list_t*                   my_list;
    struct mca_btl_base_endpoint_t*     endpoint; 
    mca_btl_base_tag_t                  tag;
    int                                 count;
    size_t 	                        size;
#if MCA_BTL_HAS_MPOOL
    struct mca_mpool_base_registration_t* registration;
#endif
}; 
typedef struct mca_btl_sicortex_frag_t mca_btl_sicortex_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_sicortex_frag_t); 

typedef struct mca_btl_sicortex_frag_t mca_btl_sicortex_frag_eager_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_sicortex_frag_eager_t); 

typedef struct mca_btl_sicortex_frag_t mca_btl_sicortex_frag_max_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_sicortex_frag_max_t); 

typedef struct mca_btl_sicortex_frag_t mca_btl_sicortex_frag_user_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_sicortex_frag_user_t); 


/*
 * Macros to allocate/return descriptors from module specific
 * free list(s).
 */

#define MCA_BTL_SICORTEX_FRAG_ALLOC(list, frag, rc)                     \
    do {                                                                \
        ompi_free_list_item_t *_item;                                   \
        OMPI_FREE_LIST_GET((list), _item, rc);                          \
        frag = (mca_btl_sicortex_frag_t*)_item;                         \
    } while (0)

#define MCA_BTL_SICORTEX_FRAG_RETURN(btl, frag)                    \
    do {                                                           \
        OMPI_FREE_LIST_RETURN(frag->my_list,                       \
                              (ompi_free_list_item_t*)(frag));     \
    } while (0)

#define MCA_BTL_SICORTEX_FRAG_ALLOC_EAGER(btl, frag, rc)                \
    MCA_BTL_SICORTEX_FRAG_ALLOC(&mca_btl_sicortex_component.sicortex_frag_eager, frag, rc )

#define MCA_BTL_SICORTEX_FRAG_ALLOC_MAX(btl, frag, rc)                  \
    MCA_BTL_SICORTEX_FRAG_ALLOC(&mca_btl_sicortex_component.sicortex_frag_max, frag, rc )

#define MCA_BTL_SICORTEX_FRAG_ALLOC_USER(btl, frag, rc)                 \
    MCA_BTL_SICORTEX_FRAG_ALLOC(&mca_btl_sicortex_component.sicortex_frag_user, frag, rc )

END_C_DECLS

#endif
