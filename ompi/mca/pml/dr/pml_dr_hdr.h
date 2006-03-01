/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
/**
 * @file
 */
#ifndef MCA_PML_DR_HEADER_H
#define MCA_PML_DR_HEADER_H

#include "ompi_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/types.h"

#define MCA_PML_DR_HDR_TYPE_MATCH     1
#define MCA_PML_DR_HDR_TYPE_RNDV      2
#define MCA_PML_DR_HDR_TYPE_ACK       3
#define MCA_PML_DR_HDR_TYPE_FRAG      4

#define MCA_PML_DR_HDR_FLAGS_NBO      1  /* is the hdr in network byte order */
#define MCA_PML_DR_HDR_FLAGS_VFRAG    2
#define MCA_PML_DR_HDR_FLAGS_MATCH    4  /* is the ack in response to a match */
#define MCA_PML_DR_HDR_FLAGS_RNDV     8  /* is the ack in response to a rndv */
#define MCA_PML_DR_HDR_FLAGS_BUFFERED 16
/**
 * Common hdr attributes - must be first element in each hdr type 
 */
struct mca_pml_dr_common_hdr_t {
    uint8_t hdr_type;  /**< type of envelope */
    uint8_t hdr_flags; /**< flags indicating how fragment should be processed */
    uint16_t hdr_csum; /**< checksum over header */
};
typedef struct mca_pml_dr_common_hdr_t mca_pml_dr_common_hdr_t;

#define MCA_PML_DR_COMMON_HDR_NTOH(h) 
#define MCA_PML_DR_COMMON_HDR_HTON(h) 

/**
 *  Header definition for the first fragment, contains the 
 *  attributes required to match the corresponding posted receive.
 */
struct mca_pml_dr_match_hdr_t {
    mca_pml_dr_common_hdr_t hdr_common;    /**< common attributes */
    uint32_t hdr_vid;                      /**< vfrag id */
    uint16_t hdr_ctx;                      /**< communicator index */
    uint16_t hdr_seq;                      /**< message sequence number */
    int32_t  hdr_src;                      /**< source rank */
    int32_t  hdr_tag;                      /**< user tag */
    uint32_t hdr_csum;                     /**< checksum over data */
    ompi_ptr_t hdr_src_req;                /**< pointer to source request - returned in ack */
};
typedef struct mca_pml_dr_match_hdr_t mca_pml_dr_match_hdr_t;

#define MCA_PML_DR_MATCH_HDR_NTOH(h) \
    do { \
    MCA_PML_DR_COMMON_HDR_NTOH((h).hdr_common); \
    (h).hdr_ctx = ntohs((h).hdr_ctx); \
    (h).hdr_src = ntohl((h).hdr_src); \
    (h).hdr_tag = ntohl((h).hdr_tag); \
    (h).hdr_seq = ntohs((h).hdr_seq); \
    } while (0)

#define MCA_PML_DR_MATCH_HDR_HTON(h) \
    do { \
    MCA_PML_DR_COMMON_HDR_HTON((h).hdr_common); \
    (h).hdr_ctx = htons((h).hdr_ctx); \
    (h).hdr_src = htonl((h).hdr_src); \
    (h).hdr_tag = htonl((h).hdr_tag); \
    (h).hdr_seq = htons((h).hdr_seq); \
    } while (0) 
    
/**
 * Header definition for the first fragment when an acknowledgment
 * is required. This could be the first fragment of a large message
 * or a short message that requires an ack (synchronous).
 */
struct mca_pml_dr_rendezvous_hdr_t {
    mca_pml_dr_match_hdr_t hdr_match;
    uint64_t hdr_msg_length;            /**< message length */
};
typedef struct mca_pml_dr_rendezvous_hdr_t mca_pml_dr_rendezvous_hdr_t;

#define MCA_PML_DR_RNDV_HDR_NTOH(h) \
    do { \
    MCA_PML_DR_MATCH_HDR_NTOH((h).hdr_match); \
    (h).hdr_msg_length = ntoh64((h).hdr_msg_length); \
    } while (0)

#define MCA_PML_DR_RNDV_HDR_HTON(h) \
    do { \
    MCA_PML_DR_MATCH_HDR_HTON((h).hdr_match); \
    (h).hdr_msg_length = hton64((h).hdr_msg_length); \
    } while (0) 

/**
 *  Header for subsequent fragments.
 */
struct mca_pml_dr_frag_hdr_t {
    mca_pml_dr_common_hdr_t hdr_common;    /**< common attributes */
    uint32_t hdr_vid;                      /**< virtual frag id */
    uint16_t hdr_vlen;                     /**< length of entire vfrag */
    uint16_t hdr_frag_idx;                 /**< bit index of this frag w/in vfrag */
    uint32_t hdr_frag_csum;                /**< checksum over data */
    uint64_t hdr_frag_offset;              /**< absolute offset of this fragment */
    ompi_ptr_t hdr_src_req;                /**< pointer to source req */
    ompi_ptr_t hdr_dst_req;                /**< pointer to receive req */
};
typedef struct mca_pml_dr_frag_hdr_t mca_pml_dr_frag_hdr_t;

#define MCA_PML_DR_FRAG_HDR_NTOH(h) \
    do { \
    MCA_PML_DR_COMMON_HDR_NTOH((h).hdr_common); \
    (h).hdr_frag_offset = ntoh64((h).hdr_frag_offset); \
    } while (0)

#define MCA_PML_DR_FRAG_HDR_HTON(h) \
    do { \
    MCA_PML_DR_COMMON_HDR_HTON((h).hdr_common); \
    (h).hdr_frag_offset = hton64((h).hdr_frag_offset); \
    } while (0)


/**
 *  Header used to acknowledgment outstanding fragment(s).
 */

struct mca_pml_dr_ack_hdr_t {
    mca_pml_dr_common_hdr_t hdr_common;       /**< common attributes */
    uint32_t   hdr_vid;                       /**< virtual fragment id */
    uint64_t   hdr_vmask;                     /**< acknowledged frags */
    ompi_ptr_t hdr_src_req;                   /**< source request */
    ompi_ptr_t hdr_dst_req;                   /**< matched receive request */
};
typedef struct mca_pml_dr_ack_hdr_t mca_pml_dr_ack_hdr_t;

#define MCA_PML_DR_ACK_HDR_NTOH(h) \
    do { \
    MCA_PML_DR_COMMON_HDR_NTOH(h.hdr_common); \
    (h).hdr_dst_size = ntoh64((h).hdr_dst_size); \
    } while (0)

#define MCA_PML_DR_ACK_HDR_HTON(h) \
    do { \
    MCA_PML_DR_COMMON_HDR_HTON((h).hdr_common); \
    (h).hdr_dst_size = hton64((h).hdr_dst_size); \
    } while (0) 

/**
 * Union of defined hdr types.
 */
union mca_pml_dr_hdr_t {
    mca_pml_dr_common_hdr_t hdr_common;
    mca_pml_dr_match_hdr_t hdr_match;
    mca_pml_dr_rendezvous_hdr_t hdr_rndv;
    mca_pml_dr_frag_hdr_t hdr_frag;
    mca_pml_dr_ack_hdr_t hdr_ack;
};
typedef union mca_pml_dr_hdr_t mca_pml_dr_hdr_t;


#endif
