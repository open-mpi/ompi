/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * Copyright (c) 2006-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2010-2012 Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_IB_FRAG_H
#define MCA_BTL_IB_FRAG_H

#include "opal_config.h"
#include "opal/align.h"
#include "opal/mca/btl/btl.h"

#include <infiniband/verbs.h>

BEGIN_C_DECLS

struct mca_btl_iwarp_reg_t;

struct mca_btl_iwarp_header_t {
    mca_btl_base_tag_t tag;
    uint8_t cm_seen;
    uint16_t credits;
#if OPAL_OPENIB_PAD_HDR
    uint8_t padding[4];
#endif
};
typedef struct mca_btl_iwarp_header_t mca_btl_iwarp_header_t;
#define BTL_IWARP_RDMA_CREDITS_FLAG (1<<15)
#define BTL_IWARP_IS_RDMA_CREDITS(I) ((I)&BTL_IWARP_RDMA_CREDITS_FLAG)
#define BTL_IWARP_CREDITS(I) ((I)&~BTL_IWARP_RDMA_CREDITS_FLAG)

#define BTL_IWARP_HEADER_HTON(h)     \
do {                                  \
    (h).credits = htons((h).credits); \
} while (0)

#define BTL_IWARP_HEADER_NTOH(h)     \
do {                                  \
    (h).credits = ntohs((h).credits); \
} while (0)

typedef struct mca_btl_iwarp_header_coalesced_t {
    mca_btl_base_tag_t tag;
    uint32_t size;
    uint32_t alloc_size;
#if OPAL_OPENIB_PAD_HDR
    uint8_t padding[4];
#endif
} mca_btl_iwarp_header_coalesced_t;

#define BTL_IWARP_HEADER_COALESCED_NTOH(h)     \
    do {                                        \
        (h).size = ntohl((h).size);             \
        (h).alloc_size = ntohl((h).alloc_size); \
     } while(0)

#define BTL_IWARP_HEADER_COALESCED_HTON(h)     \
    do {                                        \
        (h).size = htonl((h).size);             \
        (h).alloc_size = htonl((h).alloc_size); \
     } while(0)

#if OPAL_OPENIB_PAD_HDR
/* BTL_IWARP_FTR_PADDING
 * This macro is used to keep the pointer to iwarp footers aligned for
 * systems like SPARC64 that take a big performance hit when addresses
 * are not aligned (and by default sigbus instead of coercing the type on
 * an unaligned address).
 *
 * We assure alignment of a packet's structures when OPAL_OPENIB_PAD_HDR
 * is set to 1.  When this is the case then several structures are padded
 * to assure alignment and the mca_btl_iwarp_footer_t structure itself
 * will uses the BTL_IWARP_FTR_PADDING macro to shift the location of the
 * pointer to assure proper alignment after the PML Header and data.
 * For example sending a 1 byte data packet the memory layout without
 * footer alignment would look something like the following:
 *
 * 0x00   : mca_btl_iwarp_coalesced_header_t (12 bytes + 4 byte pad)
 * 0x10   : mca_btl_iwarp_control_header_t (1 byte + 7 byte pad)
 * 0x18   : mca_btl_iwarp_header_t (4 bytes + 4 byte pad)
 * 0x20   : PML Header and data (16 bytes PML + 1 byte data)
 * 0x29   : mca_btl_iwarp_footer_t (4 bytes + 4 byte pad)
 * 0x31   : end of packet
 *
 * By applying the BTL_IWARP_FTR_PADDING() in the progress_one_device
 * and post_send routines we adjust the pointer to mca_btl_iwarp_footer_t
 * from 0x29 to 0x2C thus correctly aligning the start of the
 * footer pointer.  This adjustment will cause the padding field of
 * mca_btl_iwarp_footer_t to overlap with the neighboring memory but since
 * we never use the padding we do not end up inadvertently overwriting
 * memory that does not belong to the fragment.
 */
#define BTL_IWARP_FTR_PADDING(size) \
    OPAL_ALIGN_PAD_AMOUNT(size, sizeof(uint64_t))

/* BTL_IWARP_ALIGN_COALESCE_HDR
 * This macro is used in btl_iwarp.c, while creating a coalesce fragment,
 * to align the coalesce headers.
 */
#define BTL_IWARP_ALIGN_COALESCE_HDR(ptr) \
  OPAL_ALIGN_PTR(ptr, sizeof(uint32_t), unsigned char*)

/* BTL_IWARP_COALESCE_HDR_PADDING
 * This macro is used in btl_iwarp_component.c, while parsing an incoming
 * coalesce fragment, to determine the padding amount used to align the
 * mca_btl_iwarp_coalesce_hdr_t.
 */
#define BTL_IWARP_COALESCE_HDR_PADDING(ptr) \
  OPAL_ALIGN_PAD_AMOUNT(ptr, sizeof(uint32_t))
#else
#define BTL_IWARP_FTR_PADDING(size) 0
#define BTL_IWARP_ALIGN_COALESCE_HDR(ptr) ptr
#define BTL_IWARP_COALESCE_HDR_PADDING(ptr) 0
#endif

struct mca_btl_iwarp_footer_t {
#if OPAL_ENABLE_DEBUG
    uint32_t seq;
#endif
    union {
        uint32_t size;
        uint8_t buf[4];
    } u;
#if OPAL_OPENIB_PAD_HDR
#if OPAL_ENABLE_DEBUG
    /* this footer needs to be of a 8-byte multiple so by adding the
     * seq field you throw this off and you cannot just remove the
     * padding because the padding is needed in order to adjust the alignment
     * and not overwrite other packets.
     */
    uint8_t padding[12];
#else
    uint8_t padding[8];
#endif
#endif
};
typedef struct mca_btl_iwarp_footer_t mca_btl_iwarp_footer_t;

#ifdef WORDS_BIGENDIAN
#define MCA_BTL_IWARP_FTR_SIZE_REVERSE(ftr)
#else
#define MCA_BTL_IWARP_FTR_SIZE_REVERSE(ftr)    \
    do {                                        \
        uint8_t tmp = (ftr).u.buf[0];           \
        (ftr).u.buf[0]=(ftr).u.buf[2];          \
        (ftr).u.buf[2]=tmp;                     \
    } while (0)
#endif

#if OPAL_ENABLE_DEBUG
#define BTL_IWARP_FOOTER_SEQ_HTON(h)  ((h).seq = htonl((h).seq))
#define BTL_IWARP_FOOTER_SEQ_NTOH(h)  ((h).seq = ntohl((h).seq))
#else
#define BTL_IWARP_FOOTER_SEQ_HTON(h)
#define BTL_IWARP_FOOTER_SEQ_NTOH(h)
#endif

#define BTL_IWARP_FOOTER_HTON(h)               \
    do {                                        \
        BTL_IWARP_FOOTER_SEQ_HTON(h);          \
        MCA_BTL_IWARP_FTR_SIZE_REVERSE(h);     \
    } while (0)

#define BTL_IWARP_FOOTER_NTOH(h)               \
    do {                                        \
        BTL_IWARP_FOOTER_SEQ_NTOH(h);          \
        MCA_BTL_IWARP_FTR_SIZE_REVERSE(h);     \
    } while (0)

#define MCA_BTL_IWARP_CONTROL_CREDITS      0
#define MCA_BTL_IWARP_CONTROL_RDMA         1
#define MCA_BTL_IWARP_CONTROL_COALESCED    2
#define MCA_BTL_IWARP_CONTROL_CTS          3

struct mca_btl_iwarp_control_header_t {
    uint8_t  type;
#if OPAL_OPENIB_PAD_HDR
    uint8_t  padding[7];
#endif
};
typedef struct mca_btl_iwarp_control_header_t mca_btl_iwarp_control_header_t;

struct mca_btl_iwarp_eager_rdma_header_t {
    mca_btl_iwarp_control_header_t control;
    uint32_t rkey;
    opal_ptr_t rdma_start;
};
typedef struct mca_btl_iwarp_eager_rdma_header_t mca_btl_iwarp_eager_rdma_header_t;

#define BTL_IWARP_EAGER_RDMA_CONTROL_HEADER_HTON(h)       \
    do {                                                   \
        (h).rkey = htonl((h).rkey);                        \
        (h).rdma_start.lval = hton64((h).rdma_start.lval); \
    } while (0)

#define BTL_IWARP_EAGER_RDMA_CONTROL_HEADER_NTOH(h)         \
    do {                                                     \
        (h).rkey = ntohl((h).rkey);                          \
        (h).rdma_start.lval = ntoh64((h).rdma_start.lval);   \
    } while (0)


struct mca_btl_iwarp_rdma_credits_header_t {
    mca_btl_iwarp_control_header_t control;
#if OPAL_OPENIB_PAD_HDR
    uint8_t  padding[1];
#endif
    uint8_t qpn;
    uint16_t rdma_credits;
};
typedef struct mca_btl_iwarp_rdma_credits_header_t mca_btl_iwarp_rdma_credits_header_t;

#define BTL_IWARP_RDMA_CREDITS_HEADER_HTON(h)     \
do {                                               \
    (h).rdma_credits = htons((h).rdma_credits);    \
} while (0)

#define BTL_IWARP_RDMA_CREDITS_HEADER_NTOH(h)     \
do {                                               \
    (h).rdma_credits = ntohs((h).rdma_credits);    \
} while (0)

enum mca_btl_iwarp_frag_type_t {
    MCA_BTL_IWARP_FRAG_RECV,
    MCA_BTL_IWARP_FRAG_RECV_USER,
    MCA_BTL_IWARP_FRAG_SEND,
    MCA_BTL_IWARP_FRAG_SEND_USER,
    MCA_BTL_IWARP_FRAG_EAGER_RDMA,
    MCA_BTL_IWARP_FRAG_CONTROL,
    MCA_BTL_IWARP_FRAG_COALESCED
};
typedef enum mca_btl_iwarp_frag_type_t mca_btl_iwarp_frag_type_t;

#define iwarp_frag_type(f) (to_base_frag(f)->type)
/**
 * IB fragment derived type.
 */
/* base iwarp frag */
typedef struct mca_btl_iwarp_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_base_segment_t segment;
    mca_btl_iwarp_frag_type_t type;
    opal_free_list_t* list;
} mca_btl_iwarp_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_iwarp_frag_t);

#define to_base_frag(f) ((mca_btl_iwarp_frag_t*)(f))

/* frag used for communication */
typedef struct mca_btl_iwarp_com_frag_t {
    mca_btl_iwarp_frag_t super;
    struct ibv_sge sg_entry;
    struct mca_btl_iwarp_reg_t *registration;
    struct mca_btl_base_endpoint_t *endpoint;
    /* number of unsignaled frags sent before this frag. */
    uint32_t n_wqes_inflight;
} mca_btl_iwarp_com_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_iwarp_com_frag_t);

#define to_com_frag(f) ((mca_btl_iwarp_com_frag_t*)(f))

typedef struct mca_btl_iwarp_out_frag_t {
    mca_btl_iwarp_com_frag_t super;
    struct ibv_send_wr sr_desc;
} mca_btl_iwarp_out_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_iwarp_out_frag_t);

#define to_out_frag(f) ((mca_btl_iwarp_out_frag_t*)(f))

typedef struct mca_btl_iwarp_com_frag_t mca_btl_iwarp_in_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_iwarp_in_frag_t);

#define to_in_frag(f) ((mca_btl_iwarp_in_frag_t*)(f))

typedef struct mca_btl_iwarp_send_frag_t {
    mca_btl_iwarp_out_frag_t super;
    mca_btl_iwarp_header_t *hdr, *chdr;
    mca_btl_iwarp_footer_t *ftr;
    uint8_t qp_idx;
    uint32_t coalesced_length;
    opal_list_t coalesced_frags;
} mca_btl_iwarp_send_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_iwarp_send_frag_t);

#define to_send_frag(f) ((mca_btl_iwarp_send_frag_t*)(f))

typedef struct mca_btl_iwarp_recv_frag_t {
    mca_btl_iwarp_in_frag_t super;
    mca_btl_iwarp_header_t *hdr;
    mca_btl_iwarp_footer_t *ftr;
    struct ibv_recv_wr rd_desc;
    uint8_t qp_idx;
} mca_btl_iwarp_recv_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_iwarp_recv_frag_t);

#define to_recv_frag(f) ((mca_btl_iwarp_recv_frag_t*)(f))

typedef struct mca_btl_iwarp_put_frag_t {
    mca_btl_iwarp_out_frag_t super;
    struct {
	mca_btl_base_rdma_completion_fn_t func;
	mca_btl_base_registration_handle_t *local_handle;
	void *context;
	void *data;
    } cb;
} mca_btl_iwarp_put_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_iwarp_put_frag_t);

#define to_put_frag(f) ((mca_btl_iwarp_put_frag_t*)(f))

typedef struct mca_btl_iwarp_get_frag_t {
    mca_btl_iwarp_in_frag_t super;
    struct ibv_send_wr sr_desc;
    struct {
	mca_btl_base_rdma_completion_fn_t func;
	mca_btl_base_registration_handle_t *local_handle;
	void *context;
	void *data;
    } cb;
} mca_btl_iwarp_get_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_iwarp_get_frag_t);

#define to_get_frag(f) ((mca_btl_iwarp_get_frag_t*)(f))

typedef struct mca_btl_iwarp_send_frag_t mca_btl_iwarp_send_control_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_iwarp_send_control_frag_t);

#define to_send_control_frag(f) ((mca_btl_iwarp_send_control_frag_t*)(f))

typedef struct mca_btl_iwarp_coalesced_frag_t {
    mca_btl_iwarp_frag_t super;
    mca_btl_iwarp_send_frag_t *send_frag;
    mca_btl_iwarp_header_coalesced_t *hdr;
    bool sent;
} mca_btl_iwarp_coalesced_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_iwarp_coalesced_frag_t);

#define to_coalesced_frag(f) ((mca_btl_iwarp_coalesced_frag_t*)(f))

/*
 * Allocate an IB send descriptor
 *
 */

static inline mca_btl_iwarp_send_control_frag_t *
alloc_control_frag(mca_btl_iwarp_module_t *btl)
{
    return to_send_control_frag(opal_free_list_wait (&btl->device->send_free_control));
}

static inline uint8_t frag_size_to_order(mca_btl_iwarp_module_t* btl,
        size_t size)
{
    int qp;
    for(qp = 0; qp < mca_btl_iwarp_component.num_qps; qp++)
        if(mca_btl_iwarp_component.qp_infos[qp].size >= size)
            return qp;

    return MCA_BTL_NO_ORDER;
}

static inline mca_btl_iwarp_com_frag_t *alloc_send_user_frag(void)
{
    return to_com_frag(opal_free_list_get (&mca_btl_iwarp_component.send_user_free));
}

static inline mca_btl_iwarp_com_frag_t *alloc_recv_user_frag(void)
{
    return to_com_frag(opal_free_list_get (&mca_btl_iwarp_component.recv_user_free));
}

static inline mca_btl_iwarp_coalesced_frag_t *alloc_coalesced_frag(void)
{
    return to_coalesced_frag(opal_free_list_get (&mca_btl_iwarp_component.send_free_coalesced));
}

#define MCA_BTL_IB_FRAG_RETURN(frag)                                    \
    do {                                                                \
        opal_free_list_return (to_base_frag(frag)->list,                \
                (opal_free_list_item_t*)(frag));                        \
    } while(0)

#define MCA_BTL_IWARP_CLEAN_PENDING_FRAGS(list)                        \
    do {                                                                \
        opal_list_item_t *_frag_item;                                   \
        while (NULL != (_frag_item = opal_list_remove_first(list))) {   \
            MCA_BTL_IB_FRAG_RETURN(_frag_item);                         \
        }                                                               \
    } while (0)

struct mca_btl_iwarp_module_t;

struct mca_btl_iwarp_frag_init_data_t {
    uint8_t order;
    opal_free_list_t* list;
};
typedef struct mca_btl_iwarp_frag_init_data_t mca_btl_iwarp_frag_init_data_t;

int mca_btl_iwarp_frag_init(opal_free_list_item_t* item, void* ctx);


END_C_DECLS
#endif
