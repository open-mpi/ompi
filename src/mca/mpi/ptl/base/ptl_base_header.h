/*
 * $HEADER$
 */

#ifndef MCA_PTL_BASE_HEADER_H
#define MCA_PTL_BASE_HEADER_H

#include "mca/mpi/ptl/ptl.h"


#define MCA_PTL_HDR_TYPE_MATCH  0
#define MCA_PTL_HDR_TYPE_FRAG   1
#define MCA_PTL_HDR_TYPE_ACK    2
#define MCA_PTL_HDR_TYPE_NACK   3

#define MCA_PTL_FLAGS_ACK_MATCHED     1
#define MCA_PTL_FLAGS_ACK_AGGREGATE   2


/* Defines the common header attributes - must be first element in each header type */
struct mca_ptl_base_common_header_t {
    /* type of envelope */
    uint8_t hdr_type;
    /* flags indicating how fragment should be processed */
    uint8_t hdr_flags;
    /* size of header - allow for variable length */
    uint16_t hdr_size;
};
typedef struct mca_ptl_base_common_header_t mca_ptl_base_common_header_t;


/* 
 *  Common header definition for all fragments.
 */

struct mca_ptl_base_frag_header_t {
    /* common header */
    mca_ptl_base_common_header_t hdr_common;
    /* fragment length */
    uint32_t hdr_frag_length;
    /* offset into message */
    uint32_t hdr_frag_offset;
    /* fragment sequence number */
    mca_ptl_base_sequence_t hdr_frag_seq;
    /* pointer to source fragment */
    lam_ptr_t hdr_src_ptr;
    /* pointer to matched receive */
    lam_ptr_t hdr_dst_ptr;
};
typedef struct mca_ptl_base_frag_header_t mca_ptl_base_frag_header_t;


/* 
 *  Header definition for the first fragment, contains the additional
 *  attributes required to match the corresponding posted receive.
 */

struct mca_ptl_base_match_header_t {
    /* fragment info */
    mca_ptl_base_frag_header_t hdr_frag;
    /* communicator index */
    uint32_t hdr_contextid;
    /* source rank */
    int32_t hdr_src;
    /* destination rank */
    int32_t hdr_dst;
    /* user tag */
    int32_t hdr_tag;
    /* message length */
    uint32_t hdr_msg_length;
    /* message sequence number */
    mca_ptl_base_sequence_t hdr_msg_seq;
};
typedef struct mca_ptl_base_match_header_t mca_ptl_base_match_header_t;


/*
 *  Header used to acknowledgment outstanding fragment(s).
 */

struct mca_ptl_base_ack_header_t {
    /* common header */
    mca_ptl_base_common_header_t hdr_common;
    /* source fragment */
    lam_ptr_t hdr_src_ptr;
    /* matched receive request */
    lam_ptr_t hdr_dst_ptr;
    /* sequence range */
};
typedef struct mca_ptl_base_ack_header_t mca_ptl_base_ack_header_t;


/*
 * Union of defined header types.
 */
union mca_ptl_base_header_t {
    mca_ptl_base_common_header_t hdr_common;
    mca_ptl_base_match_header_t hdr_match;
    mca_ptl_base_frag_header_t hdr_frag;
    mca_ptl_base_ack_header_t hdr_ack;
};
typedef union mca_ptl_base_header_t mca_ptl_base_header_t;


#endif
