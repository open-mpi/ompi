/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_BASE_HEADER_H
#define MCA_PTL_BASE_HEADER_H

#include "ompi_config.h"
#include "mca/ptl/ptl.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#define MCA_PTL_HDR_TYPE_MATCH  0
#define MCA_PTL_HDR_TYPE_FRAG   1
#define MCA_PTL_HDR_TYPE_ACK    2
#define MCA_PTL_HDR_TYPE_NACK   3
#define MCA_PTL_HDR_TYPE_GET    4
#define MCA_PTL_HDR_TYPE_FIN    5
#define MCA_PTL_HDR_TYPE_FIN_ACK 6

#define MCA_PTL_FLAGS_ACK    1  /* is an ack required */
#define MCA_PTL_FLAGS_NBO    2  /* is the header in network byte order */


/*
 * Convert a 64 bit value to network byte order.
 */

static inline uint64_t hton64(uint64_t val)
{ 
    union { uint64_t ll;                
            uint32_t l[2]; 
    } w, r;      

    /* platform already in network byte order? */
    if(htonl(1) == 1L)
        return val;
    w.ll = val;
    r.l[0] = htonl(w.l[1]);
    r.l[1] = htonl(w.l[0]);
    return r.ll; 
}


/*
 * Convert a 64 bit value from network to host byte order.
 */

static inline uint64_t ntoh64(uint64_t val)
{ 
    union { uint64_t ll;                
            uint32_t l[2]; 
    } w, r;      

    /* platform already in network byte order? */
    if(htonl(1) == 1L)
        return val;
    w.ll = val;
    r.l[0] = ntohl(w.l[1]);
    r.l[1] = ntohl(w.l[0]);
    return r.ll; 
}


/**
 * Common header attributes - must be first element in each header type 
 */
struct mca_ptl_base_common_header_t {
    uint8_t hdr_type;  /**< type of envelope */
    uint8_t hdr_flags; /**< flags indicating how fragment should be processed */
    uint16_t hdr_size; /**< size of header - allow for variable length */
};
typedef struct mca_ptl_base_common_header_t mca_ptl_base_common_header_t;

#define MCA_PTL_BASE_COMMON_HDR_NTOH(h) \
   (h).hdr_size = ntohs((h).hdr_size)

#define MCA_PTL_BASE_COMMON_HDR_HTON(h) \
   (h).hdr_size = htons((h).hdr_size)


/**
 *  Basic header for all fragments.
 */
struct mca_ptl_base_frag_header_t {
    mca_ptl_base_common_header_t hdr_common; /**< common attributes */
    uint32_t hdr_frag_length;                /**< fragment length */
    uint32_t hdr_frag_offset;                /**< offset into message */
    mca_ptl_sequence_t hdr_frag_seq;         /**< fragment sequence number */
    ompi_ptr_t hdr_src_ptr;                   /**< pointer to source fragment */
    ompi_ptr_t hdr_dst_ptr;                   /**< pointer to matched receive */
};
typedef struct mca_ptl_base_frag_header_t mca_ptl_base_frag_header_t;

#define MCA_PTL_BASE_FRAG_HDR_NTOH(h) \
    do { \
    MCA_PTL_BASE_COMMON_HDR_NTOH((h).hdr_common); \
    (h).hdr_frag_length = ntohl((h).hdr_frag_length); \
    (h).hdr_frag_offset = ntohl((h).hdr_frag_offset); \
    (h).hdr_frag_seq = ntoh64((h).hdr_frag_seq); \
    } while (0)

#define MCA_PTL_BASE_FRAG_HDR_HTON(h) \
    do { \
    MCA_PTL_BASE_COMMON_HDR_HTON((h).hdr_common); \
    (h).hdr_frag_length = htonl((h).hdr_frag_length); \
    (h).hdr_frag_offset = htonl((h).hdr_frag_offset); \
    (h).hdr_frag_seq = hton64((h).hdr_frag_seq); \
    } while (0)

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
    mca_ptl_sequence_t hdr_msg_seq;      /**< message sequence number */
};
typedef struct mca_ptl_base_match_header_t mca_ptl_base_match_header_t;

#define MCA_PTL_BASE_MATCH_HDR_NTOH(h) \
    do { \
    MCA_PTL_BASE_FRAG_HDR_NTOH((h).hdr_frag); \
    (h).hdr_contextid = ntohl((h).hdr_contextid); \
    (h).hdr_src = ntohl((h).hdr_src); \
    (h).hdr_dst = ntohl((h).hdr_dst); \
    (h).hdr_tag = ntohl((h).hdr_tag); \
    (h).hdr_msg_length = ntohl((h).hdr_msg_length); \
    (h).hdr_msg_seq = ntoh64((h).hdr_msg_seq); \
    } while (0)

#define MCA_PTL_BASE_MATCH_HDR_HTON(h) \
    do { \
    MCA_PTL_BASE_FRAG_HDR_HTON((h).hdr_frag); \
    (h).hdr_contextid = htonl((h).hdr_contextid); \
    (h).hdr_src = htonl((h).hdr_src); \
    (h).hdr_dst = htonl((h).hdr_dst); \
    (h).hdr_tag = htonl((h).hdr_tag); \
    (h).hdr_msg_length = htonl((h).hdr_msg_length); \
    (h).hdr_msg_seq = hton64((h).hdr_msg_seq); \
    } while (0) 
    

/**
 *  Header used to acknowledgment outstanding fragment(s).
 */
struct mca_ptl_base_ack_header_t {
    mca_ptl_base_common_header_t hdr_common; /**< common attributes */
    ompi_ptr_t hdr_src_ptr;                   /**< source fragment */
    ompi_ptr_t hdr_dst_match;                 /**< matched receive request */
    ompi_ptr_t hdr_dst_addr;                  /**< posted receive buffer */
    uint32_t  hdr_dst_size;                  /**< size of posted buffer */
    /* sequence range? */
};
typedef struct mca_ptl_base_ack_header_t mca_ptl_base_ack_header_t;

#define MCA_PTL_BASE_ACK_HDR_NTOH(h) \
    do { \
    MCA_PTL_BASE_COMMON_HDR_NTOH(h.hdr_common); \
    (h).hdr_dst_size = ntohl((h).hdr_dst_size); \
    } while (0)

#define MCA_PTL_BASE_ACK_HDR_HTON(h) \
    do { \
    MCA_PTL_BASE_COMMON_HDR_HTON((h).hdr_common); \
    (h).hdr_dst_size = htonl((h).hdr_dst_size); \
    } while (0) 

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
