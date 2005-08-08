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

#ifndef OMPI_BTL_PORTALS_FRAG_H
#define OMPI_BTL_PORTALS_FRAG_H

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_btl_portals_frag_t);


/**
 * Portals send fragment derived type
 */
struct mca_btl_portals_frag_t {
    mca_btl_base_descriptor_t base; 
    mca_btl_base_segment_t segment; 
    /* needed for retransmit case */
    struct mca_btl_base_endpoint_t *endpoint; 
    mca_btl_base_header_t hdr;
    enum { mca_btl_portals_frag_type_send, 
           mca_btl_portals_frag_type_recv, 
           mca_btl_portals_frag_type_rdma} type;
    size_t size; 

};
typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_t); 


typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_eager_t; 
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_eager_t); 

typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_max_t; 
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_max_t); 

typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_user_t; 
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_user_t); 

typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_recv_t;
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_recv_t); 

/*
 * Macros to allocate/return descriptors from module specific
 * free list(s).
 */
#define OMPI_BTL_PORTALS_FRAG_ALLOC_EAGER(btl_macro, frag, rc)           \
{                                                                  \
                                                                   \
    opal_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_eager, item, rc); \
    frag = (mca_btl_portals_frag_t*) item;                        \
}

#define OMPI_BTL_PORTALS_FRAG_RETURN_EAGER(btl_macro, frag)              \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_eager, \
        (opal_list_item_t*)(frag));                                \
}


#define OMPI_BTL_PORTALS_FRAG_ALLOC_MAX(btl_macro, frag, rc)             \
{                                                                  \
                                                                   \
    opal_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_max, item, rc); \
    frag = (mca_btl_portals_frag_t*) item;                        \
}

#define OMPI_BTL_PORTALS_FRAG_RETURN_MAX(btl_macro, frag)                \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_max, \
        (opal_list_item_t*)(frag));                                \
}


#define OMPI_BTL_PORTALS_FRAG_ALLOC_USER(btl_macro, frag, rc)            \
{                                                                  \
    opal_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_user, item, rc); \
    frag = (mca_btl_portals_frag_t*) item;                        \
}

#define OMPI_BTL_PORTALS_FRAG_RETURN_USER(btl_macro, frag)               \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_user, \
        (opal_list_item_t*)(frag)); \
}


#define OMPI_BTL_PORTALS_FRAG_ALLOC_RECV(btl_macro, frag, rc)            \
{                                                                  \
    opal_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_recv, item, rc); \
    frag = (mca_btl_portals_frag_t*) item;                        \
}

#define OMPI_BTL_PORTALS_FRAG_RETURN_RECV(btl_macro, frag)               \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_recv, \
        (opal_list_item_t*)(frag)); \
}



#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
