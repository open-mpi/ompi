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
#include "ompi/mca/btl/base/btl_base_error.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

typedef enum {
    MCA_BTL_GM_EAGER,
    MCA_BTL_GM_SEND,
    MCA_BTL_GM_PUT,
    MCA_BTL_GM_GET
} mca_btl_gm_frag_type_t;


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
    mca_btl_gm_frag_type_t type;
    
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
    ompi_free_list_item_t *item;                                   \
    OMPI_FREE_LIST_GET(&((mca_btl_gm_module_t*)btl)->gm_frag_eager, item, rc); \
    frag = (mca_btl_gm_frag_t*) item;                              \
}

#define MCA_BTL_GM_FRAG_ALLOC_MAX(btl, frag, rc)                   \
{                                                                  \
                                                                   \
    ompi_free_list_item_t *item;                                        \
    OMPI_FREE_LIST_GET(&((mca_btl_gm_module_t*)btl)->gm_frag_max, item, rc); \
    frag = (mca_btl_gm_frag_t*) item;                                   \
}

#define MCA_BTL_GM_FRAG_ALLOC_USER(btl, frag, rc)                  \
{                                                                  \
    ompi_free_list_item_t *item;                                   \
    OMPI_FREE_LIST_GET(&((mca_btl_gm_module_t*)btl)->gm_frag_user, item, rc); \
    frag = (mca_btl_gm_frag_t*) item;                              \
}

#define MCA_BTL_GM_FRAG_RETURN(btl, frag)                          \
do {                                                               \
        ompi_free_list_t* mylist;                                  \
        mca_btl_gm_module_t* btl_gm = (mca_btl_gm_module_t*) btl;  \
        mca_btl_gm_frag_t* frag_gm = (mca_btl_gm_frag_t*) frag;    \
        switch(frag_gm->type) {                                    \
        case MCA_BTL_GM_EAGER:                                     \
            mylist = &btl_gm->gm_frag_eager;                       \
            break;                                                 \
        case MCA_BTL_GM_SEND:                                      \
            mylist = &btl_gm->gm_frag_max;                         \
            break;                                                 \
        case MCA_BTL_GM_PUT:                                       \
        case MCA_BTL_GM_GET:                                       \
            mylist = &btl_gm->gm_frag_user;                        \
            break;                                                 \
        default:                                                   \
            BTL_ERROR(("Unknown frag type\n"));                    \
            break;                                                 \
        }                                                          \
        OMPI_FREE_LIST_RETURN(mylist,                              \
                              (ompi_free_list_item_t*)(frag));     \
} while (0);
    

/* called with mca_btl_gm_component.gm_lock held */
                                                                                                       
#define MCA_BTL_GM_FRAG_POST(btl,frag) \
do { \
    if(opal_list_get_size(&btl->gm_repost) < (size_t)btl->gm_num_repost) { \
        opal_list_append(&btl->gm_repost, (opal_list_item_t*)frag); \
    } else { \
        do { \
            gm_provide_receive_buffer(btl->port, frag->hdr, frag->size, frag->priority); \
        } while (NULL != (frag = (mca_btl_gm_frag_t*)opal_list_remove_first(&btl->gm_repost)));  \
    } \
} while(0) 



#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
