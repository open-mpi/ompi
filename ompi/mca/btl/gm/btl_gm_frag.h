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
    enum gm_priority priority;
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
    opal_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_gm_module_t*)btl)->gm_frag_eager, item, rc); \
    frag = (mca_btl_gm_frag_t*) item;                              \
}

#define MCA_BTL_GM_FRAG_RETURN_EAGER(btl, frag)                    \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_gm_module_t*)btl)->gm_frag_eager, \
        (opal_list_item_t*)(frag));                                \
}

#define MCA_BTL_GM_FRAG_ALLOC_MAX(btl, frag, rc)                   \
{                                                                  \
                                                                   \
    opal_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_gm_module_t*)btl)->gm_frag_max, item, rc); \
    frag = (mca_btl_gm_frag_t*) item;                              \
}

#define MCA_BTL_GM_FRAG_RETURN_MAX(btl, frag)                      \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_gm_module_t*)btl)->gm_frag_max, \
        (opal_list_item_t*)(frag));                                \
}


#define MCA_BTL_GM_FRAG_ALLOC_USER(btl, frag, rc)                  \
{                                                                  \
    opal_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_gm_module_t*)btl)->gm_frag_user, item, rc); \
    frag = (mca_btl_gm_frag_t*) item;                              \
}

#define MCA_BTL_GM_FRAG_RETURN_USER(btl, frag)                     \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_gm_module_t*)btl)->gm_frag_user, \
        (opal_list_item_t*)(frag)); \
}

                                                                                                       
#define MCA_BTL_GM_FRAG_POST(btl,frag) \
do { \
    if(opal_list_get_size(&btl->gm_repost) < btl->gm_num_repost) { \
        OPAL_THREAD_LOCK(&btl->gm_lock);  \
        opal_list_append(&btl->gm_repost, (opal_list_item_t*)frag); \
        OPAL_THREAD_UNLOCK(&btl->gm_lock);  \
    } else { \
        OPAL_THREAD_LOCK(&btl->gm_lock);  \
        do { \
            gm_provide_receive_buffer(btl->port, frag->hdr, frag->size, priority); \
        } while (NULL != (frag = (mca_btl_gm_frag_t*)opal_list_remove_first(&btl->gm_repost)));  \
        OPAL_THREAD_UNLOCK(&btl->gm_lock);  \
    } \
} while(0) 



#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
