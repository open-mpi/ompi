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
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_IB_FRAG_H
#define MCA_BTL_IB_FRAG_H

#include "ompi_config.h"

#include <infiniband/verbs.h> 
#include "ompi/mca/btl/btl.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_btl_openib_reg_t;

struct mca_btl_openib_header_t {
    mca_btl_base_tag_t tag;
    uint16_t credits;
};
typedef struct mca_btl_openib_header_t mca_btl_openib_header_t;
#define BTL_OPENIB_RDMA_CREDITS_FLAG (1<<15)
#define BTL_OPENIB_IS_RDMA_CREDITS(I) ((I)&BTL_OPENIB_RDMA_CREDITS_FLAG)
#define BTL_OPENIB_CREDITS(I) ((I)&~BTL_OPENIB_RDMA_CREDITS_FLAG)

#define BTL_OPENIB_HEADER_HTON(h) \
do {                              \
    h.credits = htons(h.credits); \
} while (0)

#define BTL_OPENIB_HEADER_NTOH(h) \
do {                              \
    h.credits = ntohs(h.credits); \
} while (0)


struct mca_btl_openib_footer_t {
#if OMPI_ENABLE_DEBUG
    uint32_t seq;
#endif
    union {
        uint32_t size;
        uint8_t buf[4];
    } u;
};
typedef struct mca_btl_openib_footer_t mca_btl_openib_footer_t;

#ifdef WORDS_BIGENDIAN
#define MCA_BTL_OPENIB_FTR_SIZE_REVERSE(ftr)
#else 
#define MCA_BTL_OPENIB_FTR_SIZE_REVERSE(ftr)    \
    do {                                        \
        uint8_t tmp = (ftr).u.buf[0];           \
        (ftr).u.buf[0]=(ftr).u.buf[2];          \
        (ftr).u.buf[2]=tmp;                     \
    } while (0)
#endif

#if OMPI_ENABLE_DEBUG
#define BTL_OPENIB_FOOTER_HTON(h)               \
    do {                                        \
        h.seq = htonl(h.seq);                   \
        MCA_BTL_OPENIB_FTR_SIZE_REVERSE(h);     \
    } while (0)
    
#define BTL_OPENIB_FOOTER_NTOH(h)               \
    do {                                        \
        h.seq = ntohl(h.seq);                   \
        MCA_BTL_OPENIB_FTR_SIZE_REVERSE(h);     \
    } while (0)
#else
#define BTL_OPENIB_FOOTER_HTON(h)               \
    do {                                        \
        MCA_BTL_OPENIB_FTR_SIZE_REVERSE(h);     \
    } while (0)
    
#define BTL_OPENIB_FOOTER_NTOH(h)               \
    do {                                        \
        MCA_BTL_OPENIB_FTR_SIZE_REVERSE(h);     \
    } while (0)
#endif


#define MCA_BTL_OPENIB_CONTROL_CREDITS 0
#define  MCA_BTL_OPENIB_CONTROL_RDMA   1

struct mca_btl_openib_control_header_t {
    uint8_t type;
};
typedef struct mca_btl_openib_control_header_t mca_btl_openib_control_header_t;

struct mca_btl_openib_eager_rdma_header_t {
    mca_btl_openib_control_header_t control;
    uint8_t padding[3]; 
    uint32_t rkey;
    ompi_ptr_t rdma_start;
    uint64_t frag_t_len;
};
typedef struct mca_btl_openib_eager_rdma_header_t mca_btl_openib_eager_rdma_header_t;

#define BTL_OPENIB_EAGER_RDMA_CONTROL_HEADER_HTON(h)       \
    do {                                                   \
        h.rkey = htonl(h.rkey);                                  \
        h.rdma_start.lval = hton64(h.rdma_start.lval);           \
        h.frag_t_len = hton64(h.frag_t_len);                     \
    } while (0)
    
#define BTL_OPENIB_EAGER_RDMA_CONTROL_HEADER_NTOH(h)     \
    do {                                                 \
        h.rkey = ntohl(h.rkey);                          \
        h.rdma_start.lval = ntoh64(h.rdma_start.lval);   \
        h.frag_t_len = ntoh64(h.frag_t_len);             \
    } while (0)
    
    
struct mca_btl_openib_rdma_credits_header_t {
    mca_btl_openib_control_header_t control;
    uint8_t padding[1];
    uint16_t rdma_credits;
};
typedef struct mca_btl_openib_rdma_credits_header_t mca_btl_openib_rdma_credits_header_t;

#define BTL_OPENIB_RDMA_CREDITS_HEADER_HTON(h)     \
do {                                               \
    h.rdma_credits = htons(h.rdma_credits);        \
} while (0)

#define BTL_OPENIB_RDMA_CREDITS_HEADER_NTOH(h)     \
do {                                               \
    h.rdma_credits = ntohs(h.rdma_credits);        \
} while (0)

enum mca_btl_openib_frag_type_t {
    MCA_BTL_OPENIB_FRAG_EAGER,
    MCA_BTL_OPENIB_FRAG_MAX,
    MCA_BTL_OPENIB_SEND_FRAG_FRAG,
    MCA_BTL_OPENIB_RECV_FRAG_FRAG,
    MCA_BTL_OPENIB_FRAG_EAGER_RDMA,
    MCA_BTL_OPENIB_FRAG_CONTROL
};
typedef enum mca_btl_openib_frag_type_t mca_btl_openib_frag_type_t;

/**
 * IB send fragment derived type.
 */
struct mca_btl_openib_frag_t {
    mca_btl_base_descriptor_t base; 
    struct mca_btl_base_endpoint_t *endpoint; 
    mca_btl_openib_footer_t *ftr;
    mca_btl_openib_header_t *hdr;
    mca_btl_base_segment_t segment; 
    size_t size; 
    int rc;
    mca_btl_openib_frag_type_t type; 
    union{ 
        struct ibv_recv_wr rd_desc; 
        struct ibv_send_wr sr_desc; 
    } wr_desc;  
    struct ibv_sge sg_entry;  
    struct mca_btl_openib_reg_t *registration;
}; 
typedef struct mca_btl_openib_frag_t mca_btl_openib_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_openib_frag_t);

typedef struct mca_btl_openib_frag_t mca_btl_openib_send_frag_eager_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_openib_send_frag_eager_t); 

typedef struct mca_btl_openib_frag_t mca_btl_openib_send_frag_max_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_openib_send_frag_max_t); 

typedef struct mca_btl_openib_frag_t mca_btl_openib_send_frag_frag_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_openib_send_frag_frag_t); 

typedef struct mca_btl_openib_frag_t mca_btl_openib_recv_frag_frag_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_openib_recv_frag_frag_t); 

typedef struct mca_btl_openib_frag_t mca_btl_openib_recv_frag_eager_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_openib_recv_frag_eager_t); 

typedef struct mca_btl_openib_frag_t mca_btl_openib_recv_frag_max_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_openib_recv_frag_max_t); 

typedef struct mca_btl_openib_frag_t mca_btl_openib_send_frag_control_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_openib_send_frag_control_t); 

/*
 * Allocate an IB send descriptor
 *
 */

#define MCA_BTL_IB_FRAG_ALLOC_CREDIT_WAIT(btl, frag, rc)               \
{                                                                      \
                                                                       \
    ompi_free_list_item_t *item;                                       \
    OMPI_FREE_LIST_WAIT(&((mca_btl_openib_module_t*)btl)->send_free_control, item, rc);       \
    frag = (mca_btl_openib_frag_t*) item;                              \
}

#define MCA_BTL_IB_FRAG_ALLOC_EAGER(btl, frag, rc)                     \
{                                                                      \
                                                                       \
    ompi_free_list_item_t *item;                                       \
    OMPI_FREE_LIST_GET(&((mca_btl_openib_module_t*)btl)->send_free_eager, item, rc);       \
    frag = (mca_btl_openib_frag_t*) item;                              \
}

#define MCA_BTL_IB_FRAG_ALLOC_MAX(btl, frag, rc)                               \
{                                                                      \
                                                                       \
    ompi_free_list_item_t *item;                                            \
    OMPI_FREE_LIST_GET(&((mca_btl_openib_module_t*)btl)->send_free_max, item, rc);       \
    frag = (mca_btl_openib_frag_t*) item;                                  \
}

#define MCA_BTL_IB_FRAG_ALLOC_SEND_FRAG(btl, frag, rc)                               \
{                                                                      \
                                                                       \
    ompi_free_list_item_t *item;                                            \
    OMPI_FREE_LIST_GET(&((mca_btl_openib_module_t*)btl)->send_free_frag, item, rc);       \
    frag = (mca_btl_openib_frag_t*) item;                                  \
}

#define MCA_BTL_IB_FRAG_ALLOC_RECV_FRAG(btl, frag, rc)                               \
{                                                                      \
                                                                       \
    ompi_free_list_item_t *item;                                            \
    OMPI_FREE_LIST_GET(&((mca_btl_openib_module_t*)btl)->recv_free_frag, item, rc);       \
    frag = (mca_btl_openib_frag_t*) item;                                  \
}

#define MCA_BTL_IB_FRAG_RETURN(btl, frag)                                  \
{   do {                                                                   \
        ompi_free_list_t* my_list = NULL;                                  \
        switch(frag->type) {                                               \
         case MCA_BTL_OPENIB_FRAG_EAGER_RDMA:                              \
         case MCA_BTL_OPENIB_FRAG_EAGER:                                   \
          my_list = &btl->send_free_eager;                                 \
          break;                                                           \
         case MCA_BTL_OPENIB_FRAG_MAX:                                     \
          my_list = &btl->send_free_max;                                   \
          break;                                                           \
         case MCA_BTL_OPENIB_FRAG_CONTROL:                                    \
          my_list = &btl->send_free_control;                                  \
          break;                                                           \
         case MCA_BTL_OPENIB_RECV_FRAG_FRAG:                               \
          my_list = &btl->recv_free_frag;                                  \
          break;                                                           \
         case MCA_BTL_OPENIB_SEND_FRAG_FRAG:                               \
          my_list = &btl->send_free_frag;                                  \
          break;                                                           \
        }                                                                  \
        OMPI_FREE_LIST_RETURN(my_list, (ompi_free_list_item_t*)(frag));    \
    } while(0);                                                            \
}

struct mca_btl_openib_module_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
