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
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
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
 * frag - what the PML later hands us to send, may be large or small
 * segment - one packet on the wire
 * chunk - when a fragment is too big to fit into one segment, it is
 *      broken into chunks, each chunk fitting in one segment
 */

/**
 * Fragment types
 * The PML may give us very large "fragements" to send, larger than
 * an MTU.  We break fragments into segments for sending, a segment being 
 * defined to fit within an MTU.
 */
typedef enum {
    OMPI_BTL_USNIC_FRAG_LARGE_SEND,
    OMPI_BTL_USNIC_FRAG_SMALL_SEND,
    OMPI_BTL_USNIC_FRAG_PUT_DEST
} ompi_btl_usnic_frag_type_t;

typedef enum {
    OMPI_BTL_USNIC_SEG_ACK,
    OMPI_BTL_USNIC_SEG_FRAG,
    OMPI_BTL_USNIC_SEG_CHUNK,
    OMPI_BTL_USNIC_SEG_RECV
} ompi_btl_usnic_seg_type_t;


typedef struct ompi_btl_usnic_reg_t {
    mca_mpool_base_registration_t base;
    struct ibv_mr* mr;
} ompi_btl_usnic_reg_t;

/* 
 * Header that is the beginning of every usnic packet buffer.
 */
typedef struct {
    /* Verbs UD global resource header (GRH), which appears on the
       receiving side only. */
    struct ibv_grh grh;
} ompi_btl_usnic_protocol_header_t;

/**
 * usnic header type
 */
typedef enum {
    OMPI_BTL_USNIC_PAYLOAD_TYPE_ACK = 1,
    OMPI_BTL_USNIC_PAYLOAD_TYPE_FRAG = 2,       /* an entire PML fragment */
    OMPI_BTL_USNIC_PAYLOAD_TYPE_CHUNK = 3       /* one chunk of PML frag */
} ompi_btl_usnic_payload_type_t;

/**
 * BTL header that goes after the protocol header.  Since this is not
 * a stream, we can put the fields in whatever order make the least
 * holes.
 */
typedef struct {
    /* Hashed RTE process name of the sender */
    uint64_t sender;

    /* Sliding window sequence number (echoed back in an ACK).  This
       is 64 bits. */
    ompi_btl_usnic_seq_t seq;
    ompi_btl_usnic_seq_t ack_seq;       /* for piggy-backing ACKs */

    /* payload legnth (in bytes).  We unfortunately have to include
       this in our header because the L2 layer may artifically inflate
       the length of the packet to meet a minimum size */
    uint16_t payload_len; 

    /* If this is an emulated PUT, store at this address on receiver */
    char *put_addr;

    /* Type of BTL header (see enum, above) */
    uint8_t payload_type;
    /* Yuck */
    uint8_t padding;
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

/*
 * Enums for the states of frags
 */
typedef enum {
    /* Frag states: all frags */
    FRAG_ALLOCED = 0x01,

    /* Frag states: send frags */
    FRAG_SEND_ACKED = 0x02,
    FRAG_SEND_ENQUEUED = 0x04,
    FRAG_PML_CALLED_BACK = 0x08,
    FRAG_IN_HOTEL = 0x10,

    /* Frag states: receive frags */
    FRAG_RECV_WR_POSTED = 0x40,

    FRAG_MAX = 0xff
} ompi_btl_usnic_frag_state_flags_t;


/* 
 * Convenience macros for states
 */
#define FRAG_STATE_SET(frag, state) (frag)->state_flags |= (state)
#define FRAG_STATE_CLR(frag, state) (frag)->state_flags &= ~(state)
#define FRAG_STATE_GET(frag, state) ((frag)->state_flags & (state))
#define FRAG_STATE_ISSET(frag, state) (((frag)->state_flags & (state)) != 0)

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
        mca_btl_base_header_t *pml_header;
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
    ompi_btl_usnic_protocol_header_t *rs_protocol_header;

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
    uint32_t ss_flags;

    struct ompi_btl_usnic_send_frag_t *ss_parent_frag;
    int ss_hotel_room;          /* current retrans room, or -1 if none */

    /* How many times is this frag on a hardware queue? */
    uint32_t ss_send_posted;
    bool ss_ack_pending;        /* true until this segment is ACKed */

} ompi_btl_usnic_send_segment_t;

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

    size_t sf_size;            /* total_fragment size (PML + user payload) */

    /* original message data if convertor required */
    struct opal_convertor_t* sf_convertor;

    uint32_t sf_seg_post_cnt;   /* total segs currently posted for this frag */
    size_t sf_ack_bytes_left;   /* bytes remaining to be ACKed */

    struct ompi_btl_usnic_send_frag_t *sf_next;
} ompi_btl_usnic_send_frag_t;

/**
 * Descriptor for a large fragment
 * Large fragment uses two SG entries - one points to PML header,
 * other points to data.
 */
typedef struct ompi_btl_usnic_large_send_frag_t {
    ompi_btl_usnic_send_frag_t lsf_base;

    char lsf_pml_header[64];    /* space for PML header */

    uint32_t lsf_frag_id;       /* fragment ID for reassembly */
    size_t lsf_cur_offset;      /* current offset into message */
    size_t lsf_bytes_left;      /* bytes remaining to send */
    
} ompi_btl_usnic_large_send_frag_t;

/**
 * small send fragment
 * Small send will optimistically use 2 SG entries in hopes of performing
 * an inline send, but will convert to a single SG entry is inline cannot
 * be done and data must be copied.  
 * First segment will point to registered memory of associated segment to
 * hold BTL and PML headers.
 * Second segment will point directly to user data.  If inlining fails, we
 * will copy user data into the registered memory after the PML header and 
 * convert to a single segment.
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

OBJ_CLASS_DECLARATION(ompi_btl_usnic_send_frag_t);
OBJ_CLASS_DECLARATION(ompi_btl_usnic_small_send_frag_t);
OBJ_CLASS_DECLARATION(ompi_btl_usnic_large_send_frag_t);
OBJ_CLASS_DECLARATION(ompi_btl_usnic_put_dest_frag_t);

typedef ompi_btl_usnic_send_segment_t ompi_btl_usnic_frag_segment_t;
typedef ompi_btl_usnic_send_segment_t ompi_btl_usnic_chunk_segment_t;

OBJ_CLASS_DECLARATION(ompi_btl_usnic_segment_t);
OBJ_CLASS_DECLARATION(ompi_btl_usnic_frag_segment_t);
OBJ_CLASS_DECLARATION(ompi_btl_usnic_chunk_segment_t);
OBJ_CLASS_DECLARATION(ompi_btl_usnic_recv_segment_t);

typedef ompi_btl_usnic_send_segment_t ompi_btl_usnic_ack_segment_t;
OBJ_CLASS_DECLARATION(ompi_btl_usnic_ack_segment_t);

/*
 * Alloc a send frag from the send pool
 */
static inline ompi_btl_usnic_small_send_frag_t *
ompi_btl_usnic_small_send_frag_alloc(ompi_btl_usnic_module_t *module)
{
    int rc;
    ompi_free_list_item_t *item;
    ompi_btl_usnic_small_send_frag_t *frag;

    OMPI_FREE_LIST_GET(&(module->small_send_frags), item, rc);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    frag = (ompi_btl_usnic_small_send_frag_t*) item;

    /* this belongs in constructor... */
    frag->ssf_base.sf_base.uf_freelist = &(module->small_send_frags);

    /* always clear flag */
    frag->ssf_segment.ss_send_desc.send_flags = IBV_SEND_SIGNALED;

    assert(frag);
    assert(OMPI_BTL_USNIC_FRAG_SMALL_SEND == frag->ssf_base.sf_base.uf_type);

    return frag;
}

static inline ompi_btl_usnic_large_send_frag_t *
ompi_btl_usnic_large_send_frag_alloc(ompi_btl_usnic_module_t *module)
{
    int rc;
    ompi_free_list_item_t *item;
    ompi_btl_usnic_large_send_frag_t *frag;

    OMPI_FREE_LIST_GET(&(module->large_send_frags), item, rc);
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
    int rc;
    ompi_free_list_item_t *item;
    ompi_btl_usnic_put_dest_frag_t *frag;

    OMPI_FREE_LIST_GET(&(module->put_dest_frags), item, rc);
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
 * 1. PML is freeing it (via module.free())
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
    OMPI_FREE_LIST_RETURN(frag->uf_freelist, &(frag->uf_base.super));
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

static inline ompi_btl_usnic_chunk_segment_t *
ompi_btl_usnic_chunk_segment_alloc(
    ompi_btl_usnic_module_t *module)
{
    int rc;
    ompi_free_list_item_t *item;
    ompi_btl_usnic_send_segment_t *seg;

    OMPI_FREE_LIST_GET(&(module->chunk_segs), item, rc);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    seg = (ompi_btl_usnic_send_segment_t*) item;
    seg->ss_channel = USNIC_DATA_CHANNEL;

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

    OMPI_FREE_LIST_RETURN(&(module->chunk_segs), &(seg->ss_base.us_list));
}


/*
 * Alloc an ACK segment
 */
static inline ompi_btl_usnic_ack_segment_t *
ompi_btl_usnic_ack_segment_alloc(ompi_btl_usnic_module_t *module)
{
    int rc;
    ompi_free_list_item_t *item;
    ompi_btl_usnic_send_segment_t *ack;

    OMPI_FREE_LIST_GET(&(module->ack_segs), item, rc);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    ack = (ompi_btl_usnic_ack_segment_t*) item;
    ack->ss_channel = USNIC_PRIORITY_CHANNEL;

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

    OMPI_FREE_LIST_RETURN(&(module->ack_segs), &(ack->ss_base.us_list));
}

END_C_DECLS

#endif
