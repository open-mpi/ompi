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
#ifndef MCA_PML_OB1_HEADER_H
#define MCA_PML_OB1_HEADER_H

#include "ompi_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/types.h"

#define MCA_PML_OB1_HDR_TYPE_MATCH     1
#define MCA_PML_OB1_HDR_TYPE_RNDV      2
#define MCA_PML_OB1_HDR_TYPE_RGET      3
#define MCA_PML_OB1_HDR_TYPE_ACK       4
#define MCA_PML_OB1_HDR_TYPE_NACK      5
#define MCA_PML_OB1_HDR_TYPE_FRAG      6
#define MCA_PML_OB1_HDR_TYPE_GET       7
#define MCA_PML_OB1_HDR_TYPE_PUT       8
#define MCA_PML_OB1_HDR_TYPE_FIN       9
#define MCA_PML_OB1_HDR_TYPE_MAX       10

#define MCA_PML_OB1_HDR_FLAGS_ACK     1  /* is an ack required */
#define MCA_PML_OB1_HDR_FLAGS_NBO     2  /* is the hdr in network byte order */
#define MCA_PML_OB1_HDR_FLAGS_PIN     4  /* is user buffer pinned */
#define MCA_PML_OB1_HDR_FLAGS_CONTIG  8  /* is user buffer contiguous */

/**
 * Common hdr attributes - must be first element in each hdr type 
 */
struct mca_pml_ob1_common_hdr_t {
    uint8_t hdr_type;  /**< type of envelope */
    uint8_t hdr_flags; /**< flags indicating how fragment should be processed */
};
typedef struct mca_pml_ob1_common_hdr_t mca_pml_ob1_common_hdr_t;

#define MCA_PML_OB1_COMMON_HDR_NTOH(h) 
#define MCA_PML_OB1_COMMON_HDR_HTON(h) 

/**
 *  Header definition for the first fragment, contains the 
 *  attributes required to match the corresponding posted receive.
 */
struct mca_pml_ob1_match_hdr_t {
    mca_pml_ob1_common_hdr_t hdr_common;   /**< common attributes */
    uint16_t hdr_ctx;                      /**< communicator index */
    int32_t  hdr_src;                      /**< source rank */
    int32_t  hdr_tag;                      /**< user tag */
    uint16_t hdr_seq;                      /**< message sequence number */
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t  hdr_padding[2];               /**< explicitly pad to 16 bytes.  Compilers seem to already prefer to do this, but make it explicit just in case */
#endif
};
typedef struct mca_pml_ob1_match_hdr_t mca_pml_ob1_match_hdr_t;

#define MCA_PML_OB1_MATCH_HDR_NTOH(h) \
    do { \
    MCA_PML_OB1_COMMON_HDR_NTOH((h).hdr_common); \
    (h).hdr_ctx = ntohs((h).hdr_ctx); \
    (h).hdr_src = ntohl((h).hdr_src); \
    (h).hdr_tag = ntohl((h).hdr_tag); \
    (h).hdr_seq = ntohs((h).hdr_seq); \
    } while (0)

#define MCA_PML_OB1_MATCH_HDR_HTON(h) \
    do { \
    MCA_PML_OB1_COMMON_HDR_HTON((h).hdr_common); \
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
struct mca_pml_ob1_rendezvous_hdr_t {
    mca_pml_ob1_match_hdr_t hdr_match;
    uint64_t hdr_msg_length;            /**< message length */
    ompi_ptr_t hdr_src_req;             /**< pointer to source request - returned in ack */
};
typedef struct mca_pml_ob1_rendezvous_hdr_t mca_pml_ob1_rendezvous_hdr_t;

/* Note that hdr_src_req is not put in network byte order because it
   is never processed by the receiver, other than being copied into
   the ack header */
#define MCA_PML_OB1_RNDV_HDR_NTOH(h) \
    do { \
    MCA_PML_OB1_MATCH_HDR_NTOH((h).hdr_match); \
    (h).hdr_msg_length = ntoh64((h).hdr_msg_length); \
    } while (0)

#define MCA_PML_OB1_RNDV_HDR_HTON(h) \
    do { \
    MCA_PML_OB1_MATCH_HDR_HTON((h).hdr_match); \
    (h).hdr_msg_length = hton64((h).hdr_msg_length); \
    } while (0) 

/**
 * Header definition for a combined rdma rendezvous/get
 */
struct mca_pml_ob1_rget_hdr_t {
    mca_pml_ob1_rendezvous_hdr_t hdr_rndv;
    uint32_t hdr_seg_cnt;                     /**< number of segments for rdma */
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t hdr_padding[4];
#endif
    ompi_ptr_t hdr_des;                       /**< source descriptor */
    mca_btl_base_segment_t hdr_segs[1];       /**< list of segments for rdma */
};
typedef struct mca_pml_ob1_rget_hdr_t mca_pml_ob1_rget_hdr_t;

/**
 *  Header for subsequent fragments.
 */
struct mca_pml_ob1_frag_hdr_t {
    mca_pml_ob1_common_hdr_t hdr_common;     /**< common attributes */
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t hdr_padding[6];
#endif
    uint64_t hdr_frag_offset;                /**< offset into message */
    ompi_ptr_t hdr_src_req;                  /**< pointer to source request */
    ompi_ptr_t hdr_dst_req;                  /**< pointer to matched receive */
};
typedef struct mca_pml_ob1_frag_hdr_t mca_pml_ob1_frag_hdr_t;

#define MCA_PML_OB1_FRAG_HDR_NTOH(h) \
    do { \
    MCA_PML_OB1_COMMON_HDR_NTOH((h).hdr_common); \
    (h).hdr_frag_offset = ntoh64((h).hdr_frag_offset); \
    } while (0)

#define MCA_PML_OB1_FRAG_HDR_HTON(h) \
    do { \
    MCA_PML_OB1_COMMON_HDR_HTON((h).hdr_common); \
    (h).hdr_frag_offset = hton64((h).hdr_frag_offset); \
    } while (0)


/**
 *  Header used to acknowledgment outstanding fragment(s).
 */

struct mca_pml_ob1_ack_hdr_t {
    mca_pml_ob1_common_hdr_t hdr_common;      /**< common attributes */
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t hdr_padding[6];
#endif
    ompi_ptr_t hdr_src_req;                   /**< source request */
    ompi_ptr_t hdr_dst_req;                   /**< matched receive request */
    uint64_t hdr_rdma_offset;                 /**< starting point rdma protocol */
};
typedef struct mca_pml_ob1_ack_hdr_t mca_pml_ob1_ack_hdr_t;

/* Note that the request headers are not put in NBO because the
   src_req is already in receiver's byte order and the dst_req is not
   used by the receiver for anything other than backpointers in return
   headers */
#define MCA_PML_OB1_ACK_HDR_NTOH(h) \
    do { \
    MCA_PML_OB1_COMMON_HDR_NTOH(h.hdr_common); \
    (h).hdr_rdma_offset = ntoh64((h).hdr_rdma_offset); \
    } while (0)

#define MCA_PML_OB1_ACK_HDR_HTON(h) \
    do { \
    MCA_PML_OB1_COMMON_HDR_HTON((h).hdr_common); \
    (h).hdr_rdma_offset = hton64((h).hdr_rdma_offset); \
    } while (0) 

/**
 *  Header used to initiate an RDMA operation.
 */

struct mca_pml_ob1_rdma_hdr_t {
    mca_pml_ob1_common_hdr_t hdr_common;      /**< common attributes */
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t hdr_padding[2];                   /** two to pad out the hdr to a 4 byte alignment.  hdr_req will then be 8 byte aligned after 4 for hdr_seg_cnt */
#endif
    uint32_t hdr_seg_cnt;                     /**< number of segments for rdma */
    ompi_ptr_t hdr_req;                       /**< destination request */
    ompi_ptr_t hdr_des;                       /**< source descriptor */
    uint64_t hdr_rdma_offset;                 /**< current offset into user buffer */ 
    mca_btl_base_segment_t hdr_segs[1];       /**< list of segments for rdma */
};
typedef struct mca_pml_ob1_rdma_hdr_t mca_pml_ob1_rdma_hdr_t;

/**
 *  Header used to complete an RDMA operation.
 */

struct mca_pml_ob1_fin_hdr_t {
    mca_pml_ob1_common_hdr_t hdr_common;      /**< common attributes */
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t hdr_padding[6];
#endif
    ompi_ptr_t hdr_des;                       /**< completed descriptor */
};
typedef struct mca_pml_ob1_fin_hdr_t mca_pml_ob1_fin_hdr_t;

#define MCA_PML_OB1_FIN_HDR_NTOH(h) \
    do { \
    MCA_PML_OB1_COMMON_HDR_NTOH(h.hdr_common); \
    } while (0)

#define MCA_PML_OB1_FIN_HDR_HTON(h) \
    do { \
    MCA_PML_OB1_COMMON_HDR_HTON((h).hdr_common); \
    } while (0) 

/**
 * Union of defined hdr types.
 */
union mca_pml_ob1_hdr_t {
    mca_pml_ob1_common_hdr_t hdr_common;
    mca_pml_ob1_match_hdr_t hdr_match;
    mca_pml_ob1_rendezvous_hdr_t hdr_rndv;
    mca_pml_ob1_rget_hdr_t hdr_rget;
    mca_pml_ob1_frag_hdr_t hdr_frag;
    mca_pml_ob1_ack_hdr_t hdr_ack;
    mca_pml_ob1_rdma_hdr_t hdr_rdma;
    mca_pml_ob1_fin_hdr_t hdr_fin;
};
typedef union mca_pml_ob1_hdr_t mca_pml_ob1_hdr_t;


#endif
