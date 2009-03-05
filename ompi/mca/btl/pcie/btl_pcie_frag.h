/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All righs reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_PCIE_FRAG_H
#define MCA_BTL_PCIE_FRAG_H

#include "ompi_config.h"

#include "ompi/mca/btl/btl.h"

BEGIN_C_DECLS

#define MCA_BTL_PCIE_FRAG_ALIGN (16)

/* Header that sits at top of any send message */
struct mca_btl_pcie_header_t { 
    mca_btl_base_tag_t tag;
    uint8_t pad[3];
    uint32_t length; 
    ompi_ptr_t send_frag;
};  
typedef struct mca_btl_pcie_header_t mca_btl_pcie_header_t;

#define OMPI_BTL_PCIE_HEADER_HTON(header)      \
do {                                           \
  (header).length = htonl((header).length);    \
 } while (0)


#define OMPI_BTL_PCIE_HEADER_NTOH(header)      \
do {                                           \
  (header).length = ntohl((header).length);    \
 } while (0)

struct mca_btl_pcie_frag_t;

/** Type description for fragments / buffers */
enum mca_btl_pcie_frag_type_t { 
    MCA_BTL_PCIE_TYPE_UNKNOWN,
    MCA_BTL_PCIE_TYPE_EAGER,
    MCA_BTL_PCIE_TYPE_MAX,
    MCA_BTL_PCIE_TYPE_RDMA,
    MCA_BTL_PCIE_TYPE_RECV
};
typedef enum mca_btl_pcie_frag_type_t mca_btl_pcie_frag_type_t;

/** SMA transfer fragment */
struct mca_btl_pcie_sma_buf_t {
    ompi_free_list_item_t super;
    /** Pointer to the SMA space available for this copy.  An
        ompi_ptr_t because in v1.2, this sits in the sma region,
        and we need to not have different sizes on each endpoint. */
    ompi_ptr_t pcie_data;
    /** type of buffer */
    mca_btl_pcie_frag_type_t type;
};
typedef struct mca_btl_pcie_sma_buf_t mca_btl_pcie_sma_buf_t;

typedef mca_btl_pcie_sma_buf_t mca_btl_pcie_sma_buf_eager_t;
OBJ_CLASS_DECLARATION(mca_btl_pcie_sma_buf_eager_t);

typedef mca_btl_pcie_sma_buf_t mca_btl_pcie_sma_buf_max_t;
OBJ_CLASS_DECLARATION(mca_btl_pcie_sma_buf_max_t);

#define MCA_BTL_PCIE_SMA_BUF_ALLOC_EAGER(btl, buf, rc)                \
{                                                                     \
    ompi_free_list_item_t *item;                                      \
    OMPI_FREE_LIST_GET(&((mca_btl_pcie_module_t*)btl)->pcie_sma_buf_eager, item, rc); \
    buf = (mca_btl_pcie_sma_buf_t*) item;                             \
}

#define MCA_BTL_PCIE_SMA_BUF_ALLOC_MAX(btl, buf, rc)                  \
{                                                                     \
    ompi_free_list_item_t *item;                                      \
    OMPI_FREE_LIST_GET(&((mca_btl_pcie_module_t*)btl)->pcie_sma_buf_max, item, rc); \
    buf = (mca_btl_pcie_sma_buf_t*) item;                             \
}

#define MCA_BTL_PCIE_SMA_BUF_RETURN(btl, buf, ret)                    \
{                                                                     \
    ret = OMPI_SUCCESS;                                               \
    switch ((buf)->type) {                                            \
    case MCA_BTL_PCIE_TYPE_EAGER:                                     \
        OMPI_FREE_LIST_RETURN(&((mca_btl_pcie_module_t*)btl)->pcie_sma_buf_eager, \
                              (ompi_free_list_item_t*)(buf));         \
        break;                                                        \
    case MCA_BTL_PCIE_TYPE_MAX:                                       \
        OMPI_FREE_LIST_RETURN(&((mca_btl_pcie_module_t*)btl)->pcie_sma_buf_max, \
                              (ompi_free_list_item_t*)(buf));         \
        break;                                                        \
    default:                                                          \
        BTL_ERROR(("Invalid return type (%d) for frag 0x%lx in SMA_BUF_RETURN", \
                   buf->type, buf));                                  \
        ret = OMPI_ERR_BAD_PARAM;                                     \
    }                                                                 \
}


/** Fragment description -- used for send/rdma fragments */
struct mca_btl_pcie_frag_t {
    mca_btl_base_descriptor_t base; 
    mca_btl_base_segment_t segment; 
    struct mca_btl_base_endpoint_t *endpoint; 
    mca_btl_pcie_header_t *hdr;
    size_t size; 
    struct mca_btl_pcie_reg_t *registration;
    mca_btl_pcie_frag_type_t type;
    mca_btl_pcie_sma_buf_t *sma_buf;
}; 
typedef struct mca_btl_pcie_frag_t mca_btl_pcie_frag_t; 

typedef struct mca_btl_pcie_frag_t mca_btl_pcie_frag_eager_t; 
OBJ_CLASS_DECLARATION(mca_btl_pcie_frag_eager_t); 

typedef struct mca_btl_pcie_frag_t mca_btl_pcie_frag_max_t; 
OBJ_CLASS_DECLARATION(mca_btl_pcie_frag_max_t); 

typedef struct mca_btl_pcie_frag_t mca_btl_pcie_frag_recv_t;
OBJ_CLASS_DECLARATION(mca_btl_pcie_frag_recv_t); 

typedef struct mca_btl_pcie_frag_t mca_btl_pcie_frag_dma_t;
OBJ_CLASS_DECLARATION(mca_btl_pcie_frag_dma_t); 


#define MCA_BTL_PCIE_FRAG_ALLOC_EAGER(btl, frag, rc)                  \
{                                                                     \
    ompi_free_list_item_t *item;                                      \
    OMPI_FREE_LIST_GET(&((mca_btl_pcie_module_t*)btl)->pcie_frag_eager, item, rc); \
    frag = (mca_btl_pcie_frag_t*) item;                               \
}

#define MCA_BTL_PCIE_FRAG_ALLOC_MAX(btl, frag, rc)                    \
{                                                                     \
    ompi_free_list_item_t *item;                                      \
    OMPI_FREE_LIST_GET(&((mca_btl_pcie_module_t*)btl)->pcie_frag_max, item, rc); \
    frag = (mca_btl_pcie_frag_t*) item;                               \
}

#define MCA_BTL_PCIE_FRAG_ALLOC_DMA(btl, frag, rc)                    \
{                                                                     \
                                                                      \
    ompi_free_list_item_t *item;                                      \
    OMPI_FREE_LIST_GET(&((mca_btl_pcie_module_t*)btl)->pcie_frag_dma, item, rc); \
    frag = (mca_btl_pcie_frag_t*) item;                               \
}

#define MCA_BTL_PCIE_FRAG_RETURN(btl, frag, ret)                      \
{                                                                     \
    ret = OMPI_SUCCESS;                                               \
    switch ((frag)->type) {                                           \
    case MCA_BTL_PCIE_TYPE_EAGER:                                     \
        OMPI_FREE_LIST_RETURN(&((mca_btl_pcie_module_t*)btl)->pcie_frag_eager, \
                              (ompi_free_list_item_t*)(frag));        \
        break;                                                        \
    case MCA_BTL_PCIE_TYPE_MAX:                                       \
        OMPI_FREE_LIST_RETURN(&((mca_btl_pcie_module_t*)btl)->pcie_frag_max, \
                              (ompi_free_list_item_t*)(frag));        \
        break;                                                        \
    case MCA_BTL_PCIE_TYPE_RDMA:                                      \
        OMPI_FREE_LIST_RETURN(&((mca_btl_pcie_module_t*)btl)->pcie_frag_dma, \
                              (ompi_free_list_item_t*)(frag));        \
        break;                                                        \
    default:                                                          \
        BTL_ERROR(("Invalid return type (%d) for frag 0x%lx in FRAG_RETURN", \
                   frag->type, frag));                                \
        ret = OMPI_ERR_BAD_PARAM;                                     \
    }                                                                 \
}

END_C_DECLS

#endif /* #ifndef MCA_BTL_PCIE_FRAG_H */
