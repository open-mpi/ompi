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
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_BTL_USNIC_FRAG_H
#define OMPI_BTL_USNIC_FRAG_H

#define OMPI_BTL_USNIC_FRAG_ALIGN (8)

#include <infiniband/verbs.h>

#include "btl_usnic.h"
#include "btl_usnic_module.h"


BEGIN_C_DECLS

/*
 * Forward declarations to avoid include loops
 */
struct ompi_btl_usnic_module_t;

/*
 * Some definitions:
 * frag - what the upper layer hands us to send, may be large or small
 * segment - one packet on the wire
 * chunk - when a fragment is too big to fit into one segment, it is
 *      broken into chunks, each chunk fitting in one segment
 */

/**
 * Fragment types
 * The upper layer may give us very large "fragements" to send, larger than
 * an MTU.  We break fragments into segments for sending, a segment being 
 * defined to fit within an MTU.
 */
typedef enum {
    OMPI_BTL_USNIC_FRAG_LARGE_SEND,
    OMPI_BTL_USNIC_FRAG_SMALL_SEND,
    OMPI_BTL_USNIC_FRAG_PUT_DEST
} ompi_btl_usnic_frag_type_t;

static inline const char *
usnic_frag_type(ompi_btl_usnic_frag_type_t t)
{
    switch (t) {
    case OMPI_BTL_USNIC_FRAG_LARGE_SEND: return "large";
    case OMPI_BTL_USNIC_FRAG_SMALL_SEND: return "small";
    case OMPI_BTL_USNIC_FRAG_PUT_DEST: return "put dest";
    default: return "unknown";
    }
}

typedef enum {
    OMPI_BTL_USNIC_SEG_ACK,
    OMPI_BTL_USNIC_SEG_FRAG,
    OMPI_BTL_USNIC_SEG_CHUNK,
    OMPI_BTL_USNIC_SEG_RECV
} ompi_btl_usnic_seg_type_t;

static inline const char *
usnic_seg_type(ompi_btl_usnic_seg_type_t t)
{
    switch (t) {
    case OMPI_BTL_USNIC_SEG_ACK: return "ACK";
    case OMPI_BTL_USNIC_SEG_FRAG: return "FRAG";
    case OMPI_BTL_USNIC_SEG_CHUNK: return "CHUNK";
    case OMPI_BTL_USNIC_SEG_RECV: return "RECV";
    default: return "unknown";
    }
}


typedef struct ompi_btl_usnic_reg_t {
    mca_mpool_base_registration_t base;
    struct ibv_mr* mr;
} ompi_btl_usnic_reg_t;


/* UDP headers are always 42 bytes long */
#define OMPI_BTL_USNIC_UDP_HDR_SZ (42)

#define OMPI_BTL_USNIC_PROTO_HDR_SZ    \
    (mca_btl_usnic_component.use_udp ? \
     OMPI_BTL_USNIC_UDP_HDR_SZ :       \
     sizeof(struct ibv_grh))

/**
 * usnic header type
 */
typedef enum {
    OMPI_BTL_USNIC_PAYLOAD_TYPE_ACK = 1,
    OMPI_BTL_USNIC_PAYLOAD_TYPE_FRAG = 2,       /* an entire fragment */
    OMPI_BTL_USNIC_PAYLOAD_TYPE_CHUNK = 3       /* one chunk of fragment */
} ompi_btl_usnic_payload_type_t;

/**
 * BTL header that goes after the protocol header.  Since this is not
 * a stream, we can put the fields in whatever order make the least
 * holes.
 */
typedef struct {

    /* Hashed RTE process name of the sender */
    uint64_t sender;

    /* Sliding window sequence number (echoed back in an ACK). */
    ompi_btl_usnic_seq_t pkt_seq;
    ompi_btl_usnic_seq_t ack_seq;       /* for piggy-backing ACKs */

    /* payload legnth (in bytes).  We unfortunately have to include
       this in our header because the L2 layer may artifically inflate
       the length of the packet to meet a minimum size */
    uint16_t payload_len; 

    /* If this is an emulated PUT, store at this address on receiver */
    char *put_addr;

    /* Type of BTL header (see enum, above) */
    uint8_t payload_type;

    /* true if there is piggy-backed ACK */
    uint8_t ack_present;

    /* tag for upper layer */
    mca_btl_base_tag_t tag;
} ompi_btl_usnic_btl_header_t; 

/**
 * BTL header for a chunk of a fragment
 */
typedef struct {
    ompi_btl_usnic_btl_header_t ch_hdr;

    uint32_t ch_frag_id;        /* ID for collecting segments of same frag */
    uint32_t ch_frag_size;      /* total frag len */
    uint32_t ch_frag_offset;    /* where in fragment this goes */
} ompi_btl_usnic_btl_chunk_header_t;

/**
 * Descriptor for a common segment.  This is exactly one packet and may
 * be send or receive
 */
typedef struct ompi_btl_usnic_segment_t {
    ompi_free_list_item_t us_list;

    ompi_btl_usnic_seg_type_t us_type;

    /* allow for 2 SG entries */
    struct ibv_sge us_sg_entry[2];

    /* header for chunked frag is different */
    union {
        ompi_btl_usnic_btl_header_t *uus_btl_header;
        ompi_btl_usnic_btl_chunk_header_t *uus_btl_chunk_header;
    } us_hdr;
#define us_btl_header us_hdr.uus_btl_header
#define us_btl_chunk_header us_hdr.uus_btl_chunk_header

    union {
        uint8_t *raw;
        void *ompi_header;
    } us_payload;
} ompi_btl_usnic_segment_t;

struct ompi_btl_usnic_endpoint_t;

/**
 * Descriptor for a recv segment.  This is exactly one packet and may
 * be part of a large or small send or may be an ACK
 */
typedef struct ompi_btl_usnic_recv_segment_t {
    ompi_btl_usnic_segment_t rs_base;
    mca_btl_base_descriptor_t rs_desc;
    mca_btl_base_segment_t rs_segment;

    /* receive segments have protocol header prepended */
    uint8_t *rs_protocol_header;

    ompi_btl_usnic_endpoint_t *rs_endpoint;

    /* verbs recv desc */
    struct ibv_recv_wr rs_recv_desc;

} ompi_btl_usnic_recv_segment_t;

/**
 * Descriptor for a send segment.  This is exactly one packet and may
 * be part of a large or small send or may be an ACK
 */
typedef struct ompi_btl_usnic_send_segment_t {
    ompi_btl_usnic_segment_t ss_base;

    /* verbs send desc */
    struct ibv_send_wr ss_send_desc;

    /* channel upon which send was posted */
    ompi_btl_usnic_channel_id_t ss_channel;

    struct ompi_btl_usnic_send_frag_t *ss_parent_frag;
    int ss_hotel_room;          /* current retrans room, or -1 if none */

    /* How many times is this frag on a hardware queue? */
    uint32_t ss_send_posted;
    bool ss_ack_pending;        /* true until this segment is ACKed */

} ompi_btl_usnic_send_segment_t;

typedef ompi_btl_usnic_send_segment_t ompi_btl_usnic_frag_segment_t;
typedef ompi_btl_usnic_send_segment_t ompi_btl_usnic_chunk_segment_t;

/**
 * Common part of usNIC fragment descriptor
 */
typedef struct ompi_btl_usnic_frag_t {
    mca_btl_base_descriptor_t uf_base;

    /* fragment descriptor type */
    ompi_btl_usnic_frag_type_t uf_type;

    /* utility segments */
    mca_btl_base_segment_t uf_src_seg[2];
    mca_btl_base_segment_t uf_dst_seg[1];

    /* freelist this came from */
    ompi_free_list_t *uf_freelist;
} ompi_btl_usnic_frag_t;

/**
 * Common part of usNIC send fragment descriptor
 */
typedef struct ompi_btl_usnic_send_frag_t {
    ompi_btl_usnic_frag_t sf_base;

    struct mca_btl_base_endpoint_t *sf_endpoint;

    size_t sf_size;             /* total_fragment size (upper + user payload) */

    struct opal_convertor_t sf_convertor; /* copy of original message data if
                                             convertor required */

    uint32_t sf_seg_post_cnt;   /* total segs currently posted for this frag */
    size_t sf_ack_bytes_left;   /* bytes remaining to be ACKed */

    struct ompi_btl_usnic_send_frag_t *sf_next;
} ompi_btl_usnic_send_frag_t;

/**
 * Descriptor for a large fragment
 * Large fragment uses two SG entries - one points to upper layer header,
 * other points to data.
 */
typedef struct ompi_btl_usnic_large_send_frag_t {
    ompi_btl_usnic_send_frag_t lsf_base;

    char lsf_ompi_header[64];   /* space for upper layer header */
    mca_btl_base_tag_t lsf_tag; /* save tag */

    uint32_t lsf_frag_id;       /* fragment ID for reassembly */

    size_t lsf_cur_offset;      /* next byte offset to be enqueued on the
                                   endpoint (incl. any convertor payload) */
    size_t lsf_bytes_left;      /* bytes remaining to give enqueue on the
                                   endpoint (incl. any convertor payload) */
    size_t lsf_pack_bytes_left; /* bytes remaining to be packed into chunk
                                   segments (incl. any convertor payload) */
    uint8_t *lsf_cur_ptr;       /* current packing pointer */
    int lsf_cur_sge;
    size_t lsf_bytes_left_in_sge;

    uint8_t *lsf_buffer;        /* attached storage for usnic_alloc() */

    opal_list_t lsf_seg_chain;  /* chain of segments for converted data */

    bool lsf_pack_on_the_fly;   /* true if we are packing on the fly */
} ompi_btl_usnic_large_send_frag_t;

/* Shortcut member macros.  Access uf_src_seg array instead of the descriptor's
 * des_src ptr to save a deref. */
#define lsf_des_src lsf_base.sf_base.uf_src_seg
#define lsf_des_src_cnt lsf_base.sf_base.uf_base.des_src_cnt

/**
 * small send fragment
 * Small send will optimistically use 2 SG entries in hopes of performing
 * an inline send, but will convert to a single SG entry is inline cannot
 * be done and data must be copied.  
 * First segment will point to registered memory of associated segment to
 * hold BTL and upper layer headers.
 * Second segment will point directly to user data.  If inlining fails, we
 * will copy user data into the registered memory after the upper layer header
 * and convert to a single segment.
 */
typedef struct ompi_btl_usnic_small_send_frag_t {
    ompi_btl_usnic_send_frag_t ssf_base;

    /* small fragments have embedded segs */
    ompi_btl_usnic_send_segment_t ssf_segment;

} ompi_btl_usnic_small_send_frag_t;

/**
 * descriptor for a put destination
 */
typedef ompi_btl_usnic_frag_t ompi_btl_usnic_put_dest_frag_t;

/**
 * A simple buffer that can be enqueued on an ompi_free_list_t that is intended
 * to be used for fragment reassembly.  Nominally the free list code supports
 * this via the rb_super.ptr field, but that field is only allocated and
 * non-NULL if an mpool is used, and we don't need this reassembly memory to be
 * registered.
 */
typedef struct ompi_btl_usnic_rx_buf_t {
    ompi_free_list_item_t rb_super;
    char buf[1]; /* flexible array member for frag reassembly */
} ompi_btl_usnic_rx_buf_t;

OBJ_CLASS_DECLARATION(ompi_btl_usnic_send_frag_t);
OBJ_CLASS_DECLARATION(ompi_btl_usnic_small_send_frag_t);
OBJ_CLASS_DECLARATION(ompi_btl_usnic_large_send_frag_t);
OBJ_CLASS_DECLARATION(ompi_btl_usnic_put_dest_frag_t);

OBJ_CLASS_DECLARATION(ompi_btl_usnic_segment_t);
OBJ_CLASS_DECLARATION(ompi_btl_usnic_frag_segment_t);
OBJ_CLASS_DECLARATION(ompi_btl_usnic_chunk_segment_t);
OBJ_CLASS_DECLARATION(ompi_btl_usnic_recv_segment_t);

OBJ_CLASS_DECLARATION(ompi_btl_usnic_rx_buf_t);

typedef ompi_btl_usnic_send_segment_t ompi_btl_usnic_ack_segment_t;
OBJ_CLASS_DECLARATION(ompi_btl_usnic_ack_segment_t);

/*
 * Alloc a send frag from the send pool
 */
static inline ompi_btl_usnic_small_send_frag_t *
ompi_btl_usnic_small_send_frag_alloc(ompi_btl_usnic_module_t *module)
{
    ompi_free_list_item_t *item;
    ompi_btl_usnic_small_send_frag_t *frag;

    OMPI_FREE_LIST_GET_MT(&(module->small_send_frags), item);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    frag = (ompi_btl_usnic_small_send_frag_t*) item;

    /* this belongs in constructor... */
    frag->ssf_base.sf_base.uf_freelist = &(module->small_send_frags);

    assert(frag);
    assert(OMPI_BTL_USNIC_FRAG_SMALL_SEND == frag->ssf_base.sf_base.uf_type);

    return frag;
}

static inline ompi_btl_usnic_large_send_frag_t *
ompi_btl_usnic_large_send_frag_alloc(ompi_btl_usnic_module_t *module)
{
    ompi_free_list_item_t *item;
    ompi_btl_usnic_large_send_frag_t *frag;

    OMPI_FREE_LIST_GET_MT(&(module->large_send_frags), item);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    frag = (ompi_btl_usnic_large_send_frag_t*) item;

    /* this belongs in constructor... */
    frag->lsf_base.sf_base.uf_freelist = &(module->large_send_frags);

    assert(frag);
    assert(OMPI_BTL_USNIC_FRAG_LARGE_SEND == frag->lsf_base.sf_base.uf_type);

    return frag;
}

static inline ompi_btl_usnic_put_dest_frag_t *
ompi_btl_usnic_put_dest_frag_alloc(
    struct ompi_btl_usnic_module_t *module)
{
    ompi_free_list_item_t *item;
    ompi_btl_usnic_put_dest_frag_t *frag;

    OMPI_FREE_LIST_GET_MT(&(module->put_dest_frags), item);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    frag = (ompi_btl_usnic_put_dest_frag_t*) item;

    /* this belongs in constructor... */
    frag->uf_freelist = &(module->put_dest_frags);

    assert(frag);
    assert(OMPI_BTL_USNIC_FRAG_PUT_DEST == frag->uf_type);

    return frag;
}

/*
 * A send frag can be returned to the freelist when all of the
 * following are true:
 *
 * 1. upper layer is freeing it (via module.free())
 * 2. Or all of these:
 *    a) it finishes sending all its segments
 *    b) all of its segments have been ACKed
 *    c) it is owned by the BTL
 */
static inline bool
ompi_btl_usnic_send_frag_ok_to_return(
    ompi_btl_usnic_module_t *module,
    ompi_btl_usnic_send_frag_t *frag)
{
    assert(frag);

    if (OPAL_LIKELY(frag->sf_base.uf_base.des_flags &
                MCA_BTL_DES_FLAGS_BTL_OWNERSHIP) &&
        0 == frag->sf_ack_bytes_left &&
        0 == frag->sf_seg_post_cnt) {
        return true;
    }

    return false;
}

static inline void
ompi_btl_usnic_frag_return(
    struct ompi_btl_usnic_module_t *module,
    ompi_btl_usnic_frag_t *frag)
{
#if MSGDEBUG1
    opal_output(0, "freeing frag %p, type %s\n", (void *)frag,
            usnic_frag_type(frag->uf_type));
#endif
    frag->uf_src_seg[0].seg_len = 0;
    frag->uf_src_seg[1].seg_len = 0;

    /* If this is a large fragment, we need to free any
     * attached storage
     */
    if (frag->uf_type == OMPI_BTL_USNIC_FRAG_LARGE_SEND) {
        ompi_btl_usnic_large_send_frag_t *lfrag;
        lfrag = (ompi_btl_usnic_large_send_frag_t *)frag;
        if (lfrag->lsf_buffer != NULL) {
            free(lfrag->lsf_buffer);
            lfrag->lsf_buffer = NULL;
        }
        lfrag->lsf_pack_on_the_fly = false;

        if (2 == lfrag->lsf_des_src_cnt &&
            NULL == lfrag->lsf_des_src[1].seg_addr.pval) {
            opal_convertor_cleanup(&lfrag->lsf_base.sf_convertor);
        }
    } else if (frag->uf_type == OMPI_BTL_USNIC_FRAG_SMALL_SEND) {
        ompi_btl_usnic_small_send_frag_t *sfrag;
        sfrag = (ompi_btl_usnic_small_send_frag_t *)frag;
        sfrag->ssf_segment.ss_send_desc.send_flags &= ~IBV_SEND_INLINE;
    }

    OMPI_FREE_LIST_RETURN_MT(frag->uf_freelist, &(frag->uf_base.super));
}

/*
 * Return a send frag if it's all done and owned by BTL
 */
static inline void
ompi_btl_usnic_send_frag_return_cond(
    struct ompi_btl_usnic_module_t *module,
    ompi_btl_usnic_send_frag_t *frag)
{
    if (ompi_btl_usnic_send_frag_ok_to_return(module, frag)) {
        ompi_btl_usnic_frag_return(module, &frag->sf_base);
    }
}

/*
 * Return a frag if it's all done and owned by BTL
 * If this is a PUT destination, only condition is that we own it.  If it's 
 * a send frag, there are other conditions, so use the specific send frag 
 * return checker.
 */
static inline void
ompi_btl_usnic_frag_return_cond(
    struct ompi_btl_usnic_module_t *module,
    ompi_btl_usnic_frag_t *frag)
{
    if (OMPI_BTL_USNIC_FRAG_PUT_DEST == frag->uf_type) {
        if (OPAL_LIKELY(frag->uf_base.des_flags &
                                    MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
            ompi_btl_usnic_frag_return(module, frag);
        }
    } else {
        ompi_btl_usnic_send_frag_return_cond(module,
                (ompi_btl_usnic_send_frag_t *)frag);
    }
}

static inline ompi_btl_usnic_chunk_segment_t *
ompi_btl_usnic_chunk_segment_alloc(
    ompi_btl_usnic_module_t *module)
{
    ompi_free_list_item_t *item;
    ompi_btl_usnic_send_segment_t *seg;

    OMPI_FREE_LIST_GET_MT(&(module->chunk_segs), item);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    seg = (ompi_btl_usnic_send_segment_t*) item;
    seg->ss_channel = USNIC_DATA_CHANNEL;
    seg->ss_send_desc.send_flags = IBV_SEND_SIGNALED;

    assert(seg);
    assert(OMPI_BTL_USNIC_SEG_CHUNK == seg->ss_base.us_type);

    return seg;
}

static inline void
ompi_btl_usnic_chunk_segment_return(
    ompi_btl_usnic_module_t *module,
    ompi_btl_usnic_chunk_segment_t *seg)
{
    assert(seg);
    assert(OMPI_BTL_USNIC_SEG_CHUNK == seg->ss_base.us_type);

    OMPI_FREE_LIST_RETURN_MT(&(module->chunk_segs), &(seg->ss_base.us_list));
}

/*
 * Alloc an ACK segment
 */
static inline ompi_btl_usnic_ack_segment_t *
ompi_btl_usnic_ack_segment_alloc(ompi_btl_usnic_module_t *module)
{
    ompi_free_list_item_t *item;
    ompi_btl_usnic_send_segment_t *ack;

    OMPI_FREE_LIST_GET_MT(&(module->ack_segs), item);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    ack = (ompi_btl_usnic_ack_segment_t*) item;
    ack->ss_channel = USNIC_PRIORITY_CHANNEL;
    ack->ss_send_desc.send_flags = IBV_SEND_SIGNALED;

    assert(ack);
    assert(OMPI_BTL_USNIC_SEG_ACK == ack->ss_base.us_type);

    return ack;
}

/* 
 * Return an ACK segment
 */
static inline void
ompi_btl_usnic_ack_segment_return(
    ompi_btl_usnic_module_t *module,
    ompi_btl_usnic_ack_segment_t *ack)
{
    assert(ack);
    assert(OMPI_BTL_USNIC_SEG_ACK == ack->ss_base.us_type);

    OMPI_FREE_LIST_RETURN_MT(&(module->ack_segs), &(ack->ss_base.us_list));
}

/* returns the expected L2 packet size in bytes for the given FRAG recv
 * segment, based on the payload_len */
static inline uint32_t
ompi_btl_usnic_frag_seg_proto_size(ompi_btl_usnic_recv_segment_t *rseg)
{
    ompi_btl_usnic_segment_t *bseg = &rseg->rs_base;

    MSGDEBUG1_OUT("us_type=%d\n", bseg->us_type);
    assert(OMPI_BTL_USNIC_PAYLOAD_TYPE_FRAG == bseg->us_btl_header->payload_type);

    return (OMPI_BTL_USNIC_PROTO_HDR_SZ +
            sizeof(*bseg->us_btl_header) +
            bseg->us_btl_header->payload_len);
}

/* returns the expected L2 packet size in bytes for the given CHUNK recv
 * segment, based on the payload_len */
static inline uint32_t
ompi_btl_usnic_chunk_seg_proto_size(ompi_btl_usnic_recv_segment_t *rseg)
{
    ompi_btl_usnic_segment_t *bseg = &rseg->rs_base;

    assert(OMPI_BTL_USNIC_PAYLOAD_TYPE_CHUNK ==
           bseg->us_btl_chunk_header->ch_hdr.payload_type);

    return (OMPI_BTL_USNIC_PROTO_HDR_SZ +
            sizeof(*bseg->us_btl_chunk_header) +
            bseg->us_btl_chunk_header->ch_hdr.payload_len);
}

END_C_DECLS

#endif
