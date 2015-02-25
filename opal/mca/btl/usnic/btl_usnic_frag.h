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
 * Copyright (c) 2013-2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_BTL_USNIC_FRAG_H
#define OPAL_BTL_USNIC_FRAG_H

#define OPAL_BTL_USNIC_FRAG_ALIGN (8)

#include "btl_usnic.h"
#include "btl_usnic_module.h"

BEGIN_C_DECLS

/*
 * Forward declarations to avoid include loops
 */
struct opal_btl_usnic_module_t;

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
    OPAL_BTL_USNIC_FRAG_LARGE_SEND,
    OPAL_BTL_USNIC_FRAG_SMALL_SEND,
    OPAL_BTL_USNIC_FRAG_PUT_DEST
} opal_btl_usnic_frag_type_t;

static inline const char *
usnic_frag_type(opal_btl_usnic_frag_type_t t)
{
    switch (t) {
    case OPAL_BTL_USNIC_FRAG_LARGE_SEND: return "large";
    case OPAL_BTL_USNIC_FRAG_SMALL_SEND: return "small";
    case OPAL_BTL_USNIC_FRAG_PUT_DEST: return "put dest";
    default: return "unknown";
    }
}

typedef enum {
    OPAL_BTL_USNIC_SEG_ACK,
    OPAL_BTL_USNIC_SEG_FRAG,
    OPAL_BTL_USNIC_SEG_CHUNK,
    OPAL_BTL_USNIC_SEG_RECV
} opal_btl_usnic_seg_type_t;

static inline const char *
usnic_seg_type_str(opal_btl_usnic_seg_type_t t)
{
    switch (t) {
    case OPAL_BTL_USNIC_SEG_ACK:   return "ACK";
    case OPAL_BTL_USNIC_SEG_FRAG:  return "FRAG";
    case OPAL_BTL_USNIC_SEG_CHUNK: return "CHUNK";
    case OPAL_BTL_USNIC_SEG_RECV:  return "RECV";
    default:                       return "unknown";
    }
}


/*
 * usnic registration handle (passed over the network to peers as a
 * cookie).
 *
 * Currently, this struct is meaningless (but it must be defined /
 * exist) because we are emulating RDMA and do not have
 * btl_register_mem and btl_deregister_mem functions (and we set
 * module.btl_registration_handle_size to 0, not sizeof(struct
 * mca_btl_base_registration_handle_t)).
 */
struct mca_btl_base_registration_handle_t {
    /* Maybe we'll need fields like this */
    uint32_t lkey;
    uint32_t rkey;
};

/*
 * usnic local registration
 */
typedef struct opal_btl_usnic_reg_t {
    mca_mpool_base_registration_t base;
    struct fid_mr *ur_mr;
} opal_btl_usnic_reg_t;


/**
 * usnic header type
 */
typedef enum {
    OPAL_BTL_USNIC_PAYLOAD_TYPE_ACK = 1,
    OPAL_BTL_USNIC_PAYLOAD_TYPE_FRAG = 2,       /* an entire fragment */
    OPAL_BTL_USNIC_PAYLOAD_TYPE_CHUNK = 3       /* one chunk of fragment */
} opal_btl_usnic_payload_type_t;

/**
 * BTL header that goes after the protocol header.  Since this is not
 * a stream, we can put the fields in whatever order make the least
 * holes.
 */
typedef struct {

    /* Hashed RTE process name of the sender */
    uint64_t sender;

    /* Sliding window sequence number (echoed back in an ACK). */
    opal_btl_usnic_seq_t pkt_seq;
    opal_btl_usnic_seq_t ack_seq;       /* for piggy-backing ACKs */

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
} opal_btl_usnic_btl_header_t;

/**
 * BTL header for a chunk of a fragment
 */
typedef struct {
    opal_btl_usnic_btl_header_t ch_hdr;

    uint32_t ch_frag_id;        /* ID for collecting segments of same frag */
    uint32_t ch_frag_size;      /* total frag len */
    uint32_t ch_frag_offset;    /* where in fragment this goes */
} opal_btl_usnic_btl_chunk_header_t;

/**
 * Descriptor for a common segment.  This is exactly one packet and may
 * be sent or received.
 */
typedef struct opal_btl_usnic_segment_t {
    opal_free_list_item_t us_list;

    opal_btl_usnic_seg_type_t us_type;

    /* header for chunked frag is different */
    union {
        opal_btl_usnic_btl_header_t *uus_btl_header;
        opal_btl_usnic_btl_chunk_header_t *uus_btl_chunk_header;
    } us_hdr;
#define us_btl_header us_hdr.uus_btl_header
#define us_btl_chunk_header us_hdr.uus_btl_chunk_header

    union {
        uint8_t *raw;
        void *ompi_header;
    } us_payload;
} opal_btl_usnic_segment_t;

struct opal_btl_usnic_endpoint_t;

/**
 * Descriptor for a recv segment.  This is exactly one packet and may
 * be part of a large or small send or may be an ACK
 */
typedef struct opal_btl_usnic_recv_segment_t {
    opal_btl_usnic_segment_t rs_base;
    mca_btl_base_descriptor_t rs_desc;
    mca_btl_base_segment_t rs_segment;

    /* receive segments have protocol header prepended */
    uint8_t *rs_protocol_header;
    size_t rs_len;

    struct opal_btl_usnic_recv_segment_t *rs_next;

    opal_btl_usnic_endpoint_t *rs_endpoint;

} opal_btl_usnic_recv_segment_t;

/**
 * Descriptor for a send segment.  This is exactly one packet and may
 * be part of a large or small send or may be an ACK
 */
typedef struct opal_btl_usnic_send_segment_t {
    opal_btl_usnic_segment_t ss_base;

    uint8_t *ss_ptr;
    size_t ss_len;

    /* channel upon which send was posted */
    opal_btl_usnic_channel_id_t ss_channel;

    struct opal_btl_usnic_send_frag_t *ss_parent_frag;
    int ss_hotel_room;          /* current retrans room, or -1 if none */

    /* How many times is this frag on a hardware queue? */
    uint32_t ss_send_posted;
    bool ss_ack_pending;        /* true until this segment is ACKed */

} opal_btl_usnic_send_segment_t;

typedef opal_btl_usnic_send_segment_t opal_btl_usnic_frag_segment_t;
typedef opal_btl_usnic_send_segment_t opal_btl_usnic_chunk_segment_t;

/**
 * Common part of usNIC fragment descriptor
 */
typedef struct opal_btl_usnic_frag_t {
    mca_btl_base_descriptor_t uf_base;

    /* fragment descriptor type */
    opal_btl_usnic_frag_type_t uf_type;

    /* utility segments (just seg_addr/seg_len) */
    mca_btl_base_segment_t uf_local_seg[2];
    mca_btl_base_segment_t uf_remote_seg[1];

    /* freelist this came from */
    opal_free_list_t *uf_freelist;
} opal_btl_usnic_frag_t;

/**
 * Common part of usNIC send fragment descriptor
 */
typedef struct opal_btl_usnic_send_frag_t {
    opal_btl_usnic_frag_t sf_base;

    struct mca_btl_base_endpoint_t *sf_endpoint;

    size_t sf_size;             /* total_fragment size (upper + user payload) */

    struct opal_convertor_t sf_convertor; /* copy of original message data if
                                             convertor required */

    uint32_t sf_seg_post_cnt;   /* total segs currently posted for this frag */
    size_t sf_ack_bytes_left;   /* bytes remaining to be ACKed */

    struct opal_btl_usnic_send_frag_t *sf_next;
} opal_btl_usnic_send_frag_t;

/**
 * Descriptor for a large fragment
 * Large fragment uses two SG entries - one points to upper layer header,
 * other points to data.
 */
typedef struct opal_btl_usnic_large_send_frag_t {
    opal_btl_usnic_send_frag_t lsf_base;

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
} opal_btl_usnic_large_send_frag_t;

/* Shortcut member macros.  Access uf_src_seg array instead of the descriptor's
 * des_src ptr to save a deref. */
#define lsf_des_src       lsf_base.sf_base.uf_local_seg

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
typedef struct opal_btl_usnic_small_send_frag_t {
    opal_btl_usnic_send_frag_t ssf_base;

    /* small fragments have embedded segs */
    opal_btl_usnic_send_segment_t ssf_segment;

} opal_btl_usnic_small_send_frag_t;

/**
 * descriptor for a put destination
 */
typedef opal_btl_usnic_frag_t opal_btl_usnic_put_dest_frag_t;

/**
 * A simple buffer that can be enqueued on an opal_free_list_t that is intended
 * to be used for fragment reassembly.  Nominally the free list code supports
 * this via the rb_super.ptr field, but that field is only allocated and
 * non-NULL if an mpool is used, and we don't need this reassembly memory to be
 * registered.
 */
typedef struct opal_btl_usnic_rx_buf_t {
    opal_free_list_item_t rb_super;
    char buf[1]; /* flexible array member for frag reassembly */
} opal_btl_usnic_rx_buf_t;

OBJ_CLASS_DECLARATION(opal_btl_usnic_send_frag_t);
OBJ_CLASS_DECLARATION(opal_btl_usnic_small_send_frag_t);
OBJ_CLASS_DECLARATION(opal_btl_usnic_large_send_frag_t);
OBJ_CLASS_DECLARATION(opal_btl_usnic_put_dest_frag_t);

OBJ_CLASS_DECLARATION(opal_btl_usnic_segment_t);
OBJ_CLASS_DECLARATION(opal_btl_usnic_frag_segment_t);
OBJ_CLASS_DECLARATION(opal_btl_usnic_chunk_segment_t);
OBJ_CLASS_DECLARATION(opal_btl_usnic_recv_segment_t);

OBJ_CLASS_DECLARATION(opal_btl_usnic_rx_buf_t);

typedef opal_btl_usnic_send_segment_t opal_btl_usnic_ack_segment_t;
OBJ_CLASS_DECLARATION(opal_btl_usnic_ack_segment_t);

/*
 * Alloc a send frag from the send pool
 */
static inline opal_btl_usnic_small_send_frag_t *
opal_btl_usnic_small_send_frag_alloc(opal_btl_usnic_module_t *module)
{
    opal_free_list_item_t *item;
    opal_btl_usnic_small_send_frag_t *frag;

    USNIC_COMPAT_FREE_LIST_GET(&(module->small_send_frags), item);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    frag = (opal_btl_usnic_small_send_frag_t*) item;

    /* this belongs in constructor... */
    frag->ssf_base.sf_base.uf_freelist = &(module->small_send_frags);

    assert(frag);
    assert(OPAL_BTL_USNIC_FRAG_SMALL_SEND == frag->ssf_base.sf_base.uf_type);

    return frag;
}

static inline opal_btl_usnic_large_send_frag_t *
opal_btl_usnic_large_send_frag_alloc(opal_btl_usnic_module_t *module)
{
    opal_free_list_item_t *item;
    opal_btl_usnic_large_send_frag_t *frag;

    USNIC_COMPAT_FREE_LIST_GET(&(module->large_send_frags), item);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    frag = (opal_btl_usnic_large_send_frag_t*) item;

    /* this belongs in constructor... */
    frag->lsf_base.sf_base.uf_freelist = &(module->large_send_frags);

    assert(frag);
    assert(OPAL_BTL_USNIC_FRAG_LARGE_SEND == frag->lsf_base.sf_base.uf_type);

    return frag;
}

static inline opal_btl_usnic_put_dest_frag_t *
opal_btl_usnic_put_dest_frag_alloc(
    struct opal_btl_usnic_module_t *module)
{
    opal_free_list_item_t *item;
    opal_btl_usnic_put_dest_frag_t *frag;

    USNIC_COMPAT_FREE_LIST_GET(&(module->put_dest_frags), item);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    frag = (opal_btl_usnic_put_dest_frag_t*) item;

    /* this belongs in constructor... */
    frag->uf_freelist = &(module->put_dest_frags);

    assert(frag);
    assert(OPAL_BTL_USNIC_FRAG_PUT_DEST == frag->uf_type);

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
opal_btl_usnic_send_frag_ok_to_return(
    opal_btl_usnic_module_t *module,
    opal_btl_usnic_send_frag_t *frag)
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
opal_btl_usnic_frag_return(
    struct opal_btl_usnic_module_t *module,
    opal_btl_usnic_frag_t *frag)
{
#if MSGDEBUG1
    opal_output(0, "freeing frag %p, type %s\n", (void *)frag,
            usnic_frag_type(frag->uf_type));
#endif
    frag->uf_local_seg[0].seg_len = 0;
    frag->uf_local_seg[1].seg_len = 0;

    /* If this is a large fragment, we need to free any
     * attached storage
     */
    if (frag->uf_type == OPAL_BTL_USNIC_FRAG_LARGE_SEND) {
        opal_btl_usnic_large_send_frag_t *lfrag;
        lfrag = (opal_btl_usnic_large_send_frag_t *)frag;
        if (lfrag->lsf_buffer != NULL) {
            free(lfrag->lsf_buffer);
            lfrag->lsf_buffer = NULL;
        }
        lfrag->lsf_pack_on_the_fly = false;

        /* JMS This should never happen any more, right? */
        if (2 == lfrag->lsf_base.sf_base.uf_base.USNIC_SEND_LOCAL_COUNT &&
            NULL == lfrag->lsf_des_src[1].seg_addr.pval) {
            opal_convertor_cleanup(&lfrag->lsf_base.sf_convertor);
        }
    }

    USNIC_COMPAT_FREE_LIST_RETURN(frag->uf_freelist, &(frag->uf_base.super));
}

/*
 * Return a send frag if it's all done and owned by BTL
 */
static inline void
opal_btl_usnic_send_frag_return_cond(
    struct opal_btl_usnic_module_t *module,
    opal_btl_usnic_send_frag_t *frag)
{
    if (opal_btl_usnic_send_frag_ok_to_return(module, frag)) {
        opal_btl_usnic_frag_return(module, &frag->sf_base);
    }
}

/*
 * Return a frag if it's all done and owned by BTL
 * If this is a PUT destination, only condition is that we own it.  If it's
 * a send frag, there are other conditions, so use the specific send frag
 * return checker.
 */
static inline void
opal_btl_usnic_frag_return_cond(
    struct opal_btl_usnic_module_t *module,
    opal_btl_usnic_frag_t *frag)
{
    if (OPAL_BTL_USNIC_FRAG_PUT_DEST == frag->uf_type) {
        if (OPAL_LIKELY(frag->uf_base.des_flags &
                                    MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
            opal_btl_usnic_frag_return(module, frag);
        }
    } else {
        opal_btl_usnic_send_frag_return_cond(module,
                (opal_btl_usnic_send_frag_t *)frag);
    }
}

static inline opal_btl_usnic_chunk_segment_t *
opal_btl_usnic_chunk_segment_alloc(
    opal_btl_usnic_module_t *module)
{
    opal_free_list_item_t *item;
    opal_btl_usnic_send_segment_t *seg;

    USNIC_COMPAT_FREE_LIST_GET(&(module->chunk_segs), item);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    seg = (opal_btl_usnic_send_segment_t*) item;
    seg->ss_channel = USNIC_DATA_CHANNEL;

    assert(seg);
    assert(OPAL_BTL_USNIC_SEG_CHUNK == seg->ss_base.us_type);

    return seg;
}

static inline void
opal_btl_usnic_chunk_segment_return(
    opal_btl_usnic_module_t *module,
    opal_btl_usnic_chunk_segment_t *seg)
{
    assert(seg);
    assert(OPAL_BTL_USNIC_SEG_CHUNK == seg->ss_base.us_type);

    USNIC_COMPAT_FREE_LIST_RETURN(&(module->chunk_segs), &(seg->ss_base.us_list));
}

/*
 * Alloc an ACK segment
 */
static inline opal_btl_usnic_ack_segment_t *
opal_btl_usnic_ack_segment_alloc(opal_btl_usnic_module_t *module)
{
    opal_free_list_item_t *item;
    opal_btl_usnic_send_segment_t *ack;

    USNIC_COMPAT_FREE_LIST_GET(&(module->ack_segs), item);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    ack = (opal_btl_usnic_ack_segment_t*) item;
    ack->ss_channel = USNIC_PRIORITY_CHANNEL;

    assert(ack);
    assert(OPAL_BTL_USNIC_SEG_ACK == ack->ss_base.us_type);

    return ack;
}

/*
 * Return an ACK segment
 */
static inline void
opal_btl_usnic_ack_segment_return(
    opal_btl_usnic_module_t *module,
    opal_btl_usnic_ack_segment_t *ack)
{
    assert(ack);
    assert(OPAL_BTL_USNIC_SEG_ACK == ack->ss_base.us_type);

    USNIC_COMPAT_FREE_LIST_RETURN(&(module->ack_segs), &(ack->ss_base.us_list));
}

/* Compute and set the proper value for sfrag->sf_size.  This must not be used
 * during usnic_alloc, since the PML might change the segment size after
 * usnic_alloc returns. */
static inline void
opal_btl_usnic_compute_sf_size(opal_btl_usnic_send_frag_t *sfrag)
{
    opal_btl_usnic_frag_t *frag;

    frag = &sfrag->sf_base;

    /* JMS This can be a put or a send, and the buffers are different... */
#if 0
    assert(frag->uf_base.USNIC_SEND_LOCAL_COUNT > 0);
    assert(frag->uf_base.USNIC_SEND_LOCAL_COUNT <= 2);

    /* belt and suspenders: second len should be zero if only one SGE */
    assert(2 == frag->uf_base.USNIC_SEND_LOCAL_COUNT ||
        0 == frag->uf_local_seg[1].seg_len);
#endif

    sfrag->sf_size = 0;
    sfrag->sf_size += frag->uf_local_seg[0].seg_len;
    sfrag->sf_size += frag->uf_local_seg[1].seg_len;
}

END_C_DECLS

#endif
