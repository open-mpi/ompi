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

#ifndef MCA_BTL_PORTALS_FRAG_H
#define MCA_BTL_PORTALS_FRAG_H

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_btl_portals_frag_t);

typedef enum {
    MCA_BTL_PORTALS_FRAG_SEND,
    MCA_BTL_PORTALS_FRAG_RECV
} mca_btl_portals_frag_type_t;

struct mca_btl_portals_send_frag_t {
    struct mca_btl_portals_module_t *btl;
    struct mca_btl_base_endpoint_t *endpoint; 
    mca_btl_base_header_t hdr;
};
typedef struct mca_btl_portals_send_frag_t mca_btl_portals_send_frag_t;

struct mca_btl_portals_recv_frag_t {
    struct mca_btl_portals_recv_chunk_t *chunk;
};
typedef struct mca_btl_portals_recv_frag_t mca_btl_portals_recv_frag_t;


/**
 * PORTALS send fraportalsent derived type.
 */
struct mca_btl_portals_frag_t {
    mca_btl_base_descriptor_t base; 
    mca_btl_base_segment_t segment; 
    mca_btl_portals_frag_type_t type;
    size_t size; 

    union {
        mca_btl_portals_send_frag_t send_frag;
        mca_btl_portals_recv_frag_t recv_frag;
    } u;
}; 
typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_t); 


typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_eager_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_eager_t); 

typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_max_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_max_t); 

typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_user_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_user_t); 


/*
 * Macros to allocate/return descriptors from module specific
 * free list(s).
 */

#define MCA_BTL_PORTALS_FRAG_ALLOC_EAGER(btl, frag, rc)           \
{                                                                  \
                                                                   \
    opal_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_portals_module_t*)btl)->portals_frag_eager, item, rc); \
    frag = (mca_btl_portals_frag_t*) item;                        \
}

#define MCA_BTL_PORTALS_FRAG_RETURN_EAGER(btl, frag)              \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_portals_module_t*)btl)->portals_frag_eager, \
        (opal_list_item_t*)(frag));                                \
}

#define MCA_BTL_PORTALS_FRAG_ALLOC_MAX(btl, frag, rc)             \
{                                                                  \
                                                                   \
    opal_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_portals_module_t*)btl)->portals_frag_max, item, rc); \
    frag = (mca_btl_portals_frag_t*) item;                        \
}

#define MCA_BTL_PORTALS_FRAG_RETURN_MAX(btl, frag)                \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_portals_module_t*)btl)->portals_frag_max, \
        (opal_list_item_t*)(frag));                                \
}


#define MCA_BTL_PORTALS_FRAG_ALLOC_USER(btl, frag, rc)            \
{                                                                  \
    opal_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_portals_module_t*)btl)->portals_frag_user, item, rc); \
    frag = (mca_btl_portals_frag_t*) item;                        \
}

#define MCA_BTL_PORTALS_FRAG_RETURN_USER(btl, frag)               \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_portals_module_t*)btl)->portals_frag_user, \
        (opal_list_item_t*)(frag)); \
}



#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
