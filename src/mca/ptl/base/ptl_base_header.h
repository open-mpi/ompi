/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_BASE_HEADER_H
#define MCA_PTL_BASE_HEADER_H

#include "mca/ptl/ptl.h"


#define MCA_PTL_HDR_TYPE_MATCH  0
#define MCA_PTL_HDR_TYPE_FRAG   1
#define MCA_PTL_HDR_TYPE_ACK    2
#define MCA_PTL_HDR_TYPE_NACK   3

#define MCA_PTL_FLAGS_ACK_MATCHED     1
#define MCA_PTL_FLAGS_ACK_AGGREGATE   2


/**
 * Common header attributes - must be first element in each header type 
 */
struct mca_ptl_base_common_header_t {
    uint8_t hdr_type;  /**< type of envelope */
    uint8_t hdr_flags; /**< flags indicating how fragment should be processed */
    uint16_t hdr_size; /**< size of header - allow for variable length */
};
typedef struct mca_ptl_base_common_header_t mca_ptl_base_common_header_t;


/**
 *  Basic header for all fragments.
 */
struct mca_ptl_base_frag_header_t {
    mca_ptl_base_common_header_t hdr_common; /**< common attributes */
    uint32_t hdr_frag_length;                /**< fragment length */
    uint32_t hdr_frag_offset;                /**< offset into message */
    mca_ptl_sequence_t hdr_frag_seq;    /**< fragment sequence number */
    lam_ptr_t hdr_src_ptr;                   /**< pointer to source fragment */
    lam_ptr_t hdr_dst_ptr;                   /**< pointer to matched receive */
};
typedef struct mca_ptl_base_frag_header_t mca_ptl_base_frag_header_t;


/**
 *  Header definition for the first fragment, contains the additional
 *  attributes required to match the corresponding posted receive.
 */
struct mca_ptl_base_match_header_t {
    mca_ptl_base_frag_header_t hdr_frag; /**< fragment attributes */
    uint32_t hdr_contextid;              /**< communicator index */
    int32_t hdr_src;                     /**< source rank */
    int32_t hdr_dst;                     /**< destination rank */
    int32_t hdr_tag;                     /**< user tag */
    uint32_t hdr_msg_length;             /**< message length */
    mca_ptl_sequence_t hdr_msg_seq; /**< message sequence number */
};
typedef struct mca_ptl_base_match_header_t mca_ptl_base_match_header_t;


/**
 *  Header used to acknowledgment outstanding fragment(s).
 */
struct mca_ptl_base_ack_header_t {
    mca_ptl_base_common_header_t hdr_common; /**< common attributes */
    lam_ptr_t hdr_src_ptr;                   /**< source fragment */
    lam_ptr_t hdr_dst_ptr;                   /**< matched receive request */
    /* sequence range? */
};
typedef struct mca_ptl_base_ack_header_t mca_ptl_base_ack_header_t;


/**
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
