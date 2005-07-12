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

#ifndef MCA_BTL_IB_FRAG_H
#define MCA_BTL_IB_FRAG_H


#define MCA_BTL_IB_FRAG_ALIGN (8)
#include "ompi_config.h"
#include "btl_openib.h" 

#include <infiniband/verbs.h> 
#include "mca/mpool/openib/mpool_openib.h" 

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_btl_openib_frag_t);

typedef mca_btl_base_header_t mca_btl_openib_header_t; 


typedef enum { 
    MCA_BTL_IB_FRAG_SEND, 
    MCA_BTL_IB_FRAG_PUT, 
    MCA_BTL_IB_FRAG_GET, 
    MCA_BTL_IB_FRAG_ACK 
} mca_btl_openib_frag_type_t; 


/**
 * IB send fragment derived type.
 */
struct mca_btl_openib_frag_t {
    mca_btl_base_descriptor_t base; 
    mca_btl_base_segment_t segment; 
    struct mca_btl_base_endpoint_t *endpoint; 
    mca_btl_openib_frag_type_t type; 
    size_t size; 
    int rc; 
    
    union{ 
        struct ibv_recv_wr rr_desc; 
        struct ibv_send_wr sr_desc; 
    }; 
    struct ibv_sge sg_entry;  
    struct ibv_mr *mr; 
    mca_btl_openib_header_t *hdr;
    mca_mpool_openib_registration_t * openib_reg; 
}; 
typedef struct mca_btl_openib_frag_t mca_btl_openib_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_openib_frag_t); 


typedef struct mca_btl_openib_frag_t mca_btl_openib_send_frag_eager_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_openib_send_frag_eager_t); 

typedef struct mca_btl_openib_frag_t mca_btl_openib_send_frag_max_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_openib_send_frag_max_t); 

typedef struct mca_btl_openib_frag_t mca_btl_openib_send_frag_frag_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_openib_send_frag_frag_t); 

typedef struct mca_btl_openib_frag_t mca_btl_openib_recv_frag_eager_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_openib_recv_frag_eager_t); 

typedef struct mca_btl_openib_frag_t mca_btl_openib_recv_frag_max_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_openib_recv_frag_max_t); 


    

/*
 * Allocate an IB send descriptor
 *
 */

#define MCA_BTL_IB_FRAG_ALLOC_EAGER(btl, frag, rc)                               \
{                                                                      \
                                                                       \
    opal_list_item_t *item;                                            \
    OMPI_FREE_LIST_WAIT(&((mca_btl_openib_module_t*)btl)->send_free_eager, item, rc);       \
    frag = (mca_btl_openib_frag_t*) item;                                  \
}

#define MCA_BTL_IB_FRAG_RETURN_EAGER(btl, frag)                                  \
{                                                                      \
    OMPI_FREE_LIST_RETURN(&((mca_btl_openib_module_t*)btl)->send_free_eager, (opal_list_item_t*)(frag)); \
}


#define MCA_BTL_IB_FRAG_ALLOC_MAX(btl, frag, rc)                               \
{                                                                      \
                                                                       \
    opal_list_item_t *item;                                            \
    OMPI_FREE_LIST_WAIT(&((mca_btl_openib_module_t*)btl)->send_free_max, item, rc);       \
    frag = (mca_btl_openib_frag_t*) item;                                  \
}

#define MCA_BTL_IB_FRAG_RETURN_MAX(btl, frag)                                  \
{                                                                      \
    OMPI_FREE_LIST_RETURN(&((mca_btl_openib_module_t*)btl)->send_free_max, (opal_list_item_t*)(frag)); \
}


#define MCA_BTL_IB_FRAG_ALLOC_FRAG(btl, frag, rc)                               \
{                                                                      \
                                                                       \
    opal_list_item_t *item;                                            \
    OMPI_FREE_LIST_WAIT(&((mca_btl_openib_module_t*)btl)->send_free_frag, item, rc);       \
    frag = (mca_btl_openib_frag_t*) item;                                  \
}

#define MCA_BTL_IB_FRAG_RETURN_FRAG(btl, frag)                                  \
{                                                                      \
    OMPI_FREE_LIST_RETURN(&((mca_btl_openib_module_t*)btl)->send_free_frag, (opal_list_item_t*)(frag)); \
}





struct mca_btl_openib_module_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
