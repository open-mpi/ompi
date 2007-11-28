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
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
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
    uint8_t cm_seen;
    uint16_t credits;
};
typedef struct mca_btl_openib_header_t mca_btl_openib_header_t;
#define BTL_OPENIB_RDMA_CREDITS_FLAG (1<<15)
#define BTL_OPENIB_IS_RDMA_CREDITS(I) ((I)&BTL_OPENIB_RDMA_CREDITS_FLAG)
#define BTL_OPENIB_CREDITS(I) ((I)&~BTL_OPENIB_RDMA_CREDITS_FLAG)

#define BTL_OPENIB_HEADER_HTON(h)     \
do {                                  \
    (h).credits = htons((h).credits); \
} while (0)

#define BTL_OPENIB_HEADER_NTOH(h)     \
do {                                  \
    (h).credits = ntohs((h).credits); \
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
        (h).seq = htonl((h).seq);               \
        MCA_BTL_OPENIB_FTR_SIZE_REVERSE(h);     \
    } while (0)
    
#define BTL_OPENIB_FOOTER_NTOH(h)               \
    do {                                        \
        (h).seq = ntohl((h).seq);               \
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
#define MCA_BTL_OPENIB_CONTROL_RDMA    1

struct mca_btl_openib_control_header_t {
    uint8_t type;
};
typedef struct mca_btl_openib_control_header_t mca_btl_openib_control_header_t;

struct mca_btl_openib_eager_rdma_header_t {
    mca_btl_openib_control_header_t control;
    uint8_t padding[3]; 
    uint32_t rkey;
    ompi_ptr_t rdma_start;
};
typedef struct mca_btl_openib_eager_rdma_header_t mca_btl_openib_eager_rdma_header_t;

#define BTL_OPENIB_EAGER_RDMA_CONTROL_HEADER_HTON(h)       \
    do {                                                   \
        (h).rkey = htonl((h).rkey);                        \
        (h).rdma_start.lval = hton64((h).rdma_start.lval); \
    } while (0)
    
#define BTL_OPENIB_EAGER_RDMA_CONTROL_HEADER_NTOH(h)         \
    do {                                                     \
        (h).rkey = ntohl((h).rkey);                          \
        (h).rdma_start.lval = ntoh64((h).rdma_start.lval);   \
    } while (0)
    
    
struct mca_btl_openib_rdma_credits_header_t {
    mca_btl_openib_control_header_t control;
    uint8_t qpn;
    uint16_t rdma_credits;
};
typedef struct mca_btl_openib_rdma_credits_header_t mca_btl_openib_rdma_credits_header_t;

#define BTL_OPENIB_RDMA_CREDITS_HEADER_HTON(h)     \
do {                                               \
    (h).rdma_credits = htons((h).rdma_credits);    \
} while (0)

#define BTL_OPENIB_RDMA_CREDITS_HEADER_NTOH(h)     \
do {                                               \
    (h).rdma_credits = ntohs((h).rdma_credits);    \
} while (0)

enum mca_btl_openib_frag_type_t {
    MCA_BTL_OPENIB_FRAG_RECV,
    MCA_BTL_OPENIB_FRAG_RECV_USER,
    MCA_BTL_OPENIB_FRAG_SEND,
    MCA_BTL_OPENIB_FRAG_SEND_USER,
    MCA_BTL_OPENIB_FRAG_EAGER_RDMA,
    MCA_BTL_OPENIB_FRAG_CONTROL
};
typedef enum mca_btl_openib_frag_type_t mca_btl_openib_frag_type_t;

#define openib_frag_type(f) (to_base_frag(f)->type)
/**
 * IB fragment derived type.
 */

/* base openib frag */
typedef struct mca_btl_openib_frag_t {
    mca_btl_base_descriptor_t base; 
    mca_btl_base_segment_t segment; 
    mca_btl_openib_frag_type_t type; 
    ompi_free_list_t* list;
} mca_btl_openib_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_frag_t);

#define to_base_frag(f) ((mca_btl_openib_frag_t*)(f))

/* frag used for communication */
typedef struct mca_btl_openib_com_frag_t {
    mca_btl_openib_frag_t super;
    struct ibv_sge sg_entry;
    struct mca_btl_openib_reg_t *registration;
    struct mca_btl_base_endpoint_t *endpoint; 
} mca_btl_openib_com_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_com_frag_t);

#define to_com_frag(f) ((mca_btl_openib_com_frag_t*)(f))

typedef struct mca_btl_openib_out_frag_t {
    mca_btl_openib_com_frag_t super;
    struct ibv_send_wr sr_desc; 
} mca_btl_openib_out_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_out_frag_t);

#define to_out_frag(f) ((mca_btl_openib_out_frag_t*)(f))

typedef struct mca_btl_openib_com_frag_t mca_btl_openib_in_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_in_frag_t);

#define to_in_frag(f) ((mca_btl_openib_in_frag_t*)(f))

typedef struct mca_btl_openib_send_frag_t {
    mca_btl_openib_out_frag_t super;
    mca_btl_openib_header_t *hdr;
    mca_btl_openib_footer_t *ftr;
    uint8_t qp_idx;
} mca_btl_openib_send_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_send_frag_t);

#define to_send_frag(f) ((mca_btl_openib_send_frag_t*)(f))

typedef struct mca_btl_openib_recv_frag_t {
    mca_btl_openib_in_frag_t super;
    mca_btl_openib_header_t *hdr;
    mca_btl_openib_footer_t *ftr;
    struct ibv_recv_wr rd_desc;
    uint8_t qp_idx;
} mca_btl_openib_recv_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_recv_frag_t);

#define to_recv_frag(f) ((mca_btl_openib_recv_frag_t*)(f))

typedef struct mca_btl_openib_out_frag_t mca_btl_openib_put_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_openib_put_frag_t);

#define to_put_frag(f) ((mca_btl_openib_put_frag_t*)(f))

typedef struct mca_btl_openib_get_frag_t {
    mca_btl_openib_in_frag_t super;
    struct ibv_send_wr sr_desc; 
} mca_btl_openib_get_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_get_frag_t); 

#define to_get_frag(f) ((mca_btl_openib_get_frag_t*)(f))

typedef struct mca_btl_openib_send_frag_t mca_btl_openib_send_control_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_openib_send_control_frag_t); 

#define to_send_control_frag(f) ((mca_btl_openib_send_control_frag_t*)(f))
/*
 * Allocate an IB send descriptor
 *
 */

#define MCA_BTL_IB_FRAG_ALLOC_CREDIT_WAIT(btl, frag, rc)               \
    do {                                                               \
        ompi_free_list_item_t *item;                                   \
        OMPI_FREE_LIST_WAIT(&(btl)->send_free_control, item, rc);      \
        frag = to_send_control_frag(item);                             \
    } while(0)

#define MCA_BTL_IB_FRAG_ALLOC_BY_SIZE(btl, frag, _size, rc)             \
    do {                                                                \
        int qp;                                                         \
        ompi_free_list_item_t* item = NULL;                             \
        for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {      \
            if(mca_btl_openib_component.qp_infos[qp].size >= _size) {   \
                OMPI_FREE_LIST_GET(&(btl)->qps[qp].send_free, item, rc);\
                if(item)                                                \
                    break;                                              \
            }                                                           \
        }                                                               \
        frag = to_com_frag(item);                                       \
    } while(0);
        
#define MCA_BTL_IB_FRAG_ALLOC_SEND_USER(btl, frag, rc)                 \
    do {                                                               \
        ompi_free_list_item_t *item;                                   \
        OMPI_FREE_LIST_GET(&(btl)->send_user_free, item, rc);          \
        frag = to_com_frag(item);                                      \
    } while(0)

#define MCA_BTL_IB_FRAG_ALLOC_RECV_USER(btl, frag, rc)                  \
    do {                                                                \
        ompi_free_list_item_t *item;                                    \
        OMPI_FREE_LIST_GET(&(btl)->recv_user_free, item, rc);           \
        frag = to_com_frag(item);                                       \
    } while(0)

#define MCA_BTL_IB_FRAG_RETURN(frag)                                    \
    do {                                                                \
        OMPI_FREE_LIST_RETURN(to_base_frag(frag)->list,                 \
                (ompi_free_list_item_t*)(frag));                        \
    } while(0);

#define MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(list)                         \
    while(!opal_list_is_empty(list)){                                    \
        opal_list_item_t *frag_item;                                     \
        frag_item = opal_list_remove_first(list);                        \
        MCA_BTL_IB_FRAG_RETURN(frag_item);                               \
    }                                                                    \

struct mca_btl_openib_module_t;

struct mca_btl_openib_frag_init_data_t {
    uint8_t order;
    ompi_free_list_t* list;
};
typedef struct mca_btl_openib_frag_init_data_t mca_btl_openib_frag_init_data_t;

void mca_btl_openib_frag_init(ompi_free_list_item_t* item, void* ctx);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
