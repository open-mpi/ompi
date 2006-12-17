/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_UDAPL_FRAG_H
#define MCA_BTL_UDAPL_FRAG_H


#define MCA_BTL_UDAPL_FRAG_ALIGN (8)
#include "ompi_config.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

typedef enum {
    MCA_BTL_UDAPL_SEND,
    MCA_BTL_UDAPL_RECV,
    MCA_BTL_UDAPL_PUT,
    MCA_BTL_UDAPL_GET,
    MCA_BTL_UDAPL_CONN_RECV,
    MCA_BTL_UDAPL_CONN_SEND
} mca_btl_udapl_frag_type_t;

/**
 * uDAPL btl footer.
 * This is put after the payload packet so the PML header can be aligned.
 */
struct mca_btl_udapl_footer_t {
    mca_btl_base_tag_t tag;
};
typedef struct mca_btl_udapl_footer_t mca_btl_udapl_footer_t;

/**
 * uDAPL fragment derived type.
 */
struct mca_btl_udapl_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_base_segment_t segment;

    struct mca_btl_udapl_module_t* btl;
    struct mca_btl_base_endpoint_t *endpoint; 
    struct mca_btl_udapl_reg_t* registration;
    DAT_LMR_TRIPLET triplet;
    
    mca_btl_udapl_footer_t *ftr;
    size_t size; 
    mca_btl_udapl_frag_type_t type;
}; 
typedef struct mca_btl_udapl_frag_t mca_btl_udapl_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_udapl_frag_t); 


typedef struct mca_btl_udapl_frag_t mca_btl_udapl_frag_eager_t; 
OBJ_CLASS_DECLARATION(mca_btl_udapl_frag_eager_t); 

typedef struct mca_btl_udapl_frag_t mca_btl_udapl_frag_max_t; 
OBJ_CLASS_DECLARATION(mca_btl_udapl_frag_max_t); 

typedef struct mca_btl_udapl_frag_t mca_btl_udapl_frag_user_t; 
OBJ_CLASS_DECLARATION(mca_btl_udapl_frag_user_t); 


/*
 * Macros to allocate/return descriptors from module specific
 * free list(s).
 */

#define MCA_BTL_UDAPL_FRAG_ALLOC_EAGER(btl, frag, rc)              \
{                                                                  \
                                                                   \
    ompi_free_list_item_t *item;                                   \
    OMPI_FREE_LIST_WAIT(&((mca_btl_udapl_module_t*)btl)->udapl_frag_eager, item, rc); \
    frag = (mca_btl_udapl_frag_t*) item;                           \
}

#define MCA_BTL_UDAPL_FRAG_RETURN_EAGER(btl, frag)                 \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_udapl_module_t*)btl)->udapl_frag_eager, \
        (ompi_free_list_item_t*)(frag));                           \
}

#define MCA_BTL_UDAPL_FRAG_ALLOC_MAX(btl, frag, rc)                \
{                                                                  \
                                                                   \
    ompi_free_list_item_t *item;                                   \
    OMPI_FREE_LIST_WAIT(&((mca_btl_udapl_module_t*)btl)->udapl_frag_max, item, rc); \
    frag = (mca_btl_udapl_frag_t*) item;                           \
}

#define MCA_BTL_UDAPL_FRAG_RETURN_MAX(btl, frag)                   \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_udapl_module_t*)btl)->udapl_frag_max, \
        (ompi_free_list_item_t*)(frag));                           \
}


#define MCA_BTL_UDAPL_FRAG_ALLOC_USER(btl, frag, rc)               \
{                                                                  \
    ompi_free_list_item_t *item;                                   \
    OMPI_FREE_LIST_WAIT(&((mca_btl_udapl_module_t*)btl)->udapl_frag_user, item, rc); \
    frag = (mca_btl_udapl_frag_t*) item;                           \
}

#define MCA_BTL_UDAPL_FRAG_RETURN_USER(btl, frag)                  \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_udapl_module_t*)btl)->udapl_frag_user, \
        (ompi_free_list_item_t*)(frag)); \
}


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
