#ifndef _IOF_BASE_HEADER_
#define _IOF_BASE_HEADER_

#include "ompi_config.h"
#include "mca/iof/iof.h"


#define MCA_IOF_BASE_HDR_MSG    0
#define MCA_IOF_BASE_HDR_ACK    1 
#define MCA_IOF_BASE_HDR_PUB    2
#define MCA_IOF_BASE_HDR_UNPUB  3
#define MCA_IOF_BASE_HDR_SUB    4
#define MCA_IOF_BASE_HDR_UNSUB  5


/*
 * Maximum size of msg 
 */
#define MCA_IOF_BASE_MSG_MAX 2048

/**
 * Convert process name from network to host byte order.
 *
 * @param name
 */
#define OMPI_PROCESS_NAME_NTOH(n) \
    n.cellid = ntohl((n).cellid); \
    n.jobid = ntohl((n).jobid); \
    n.vpid = ntohl((n).vpid);
                                                                                                                 
/**
 * Convert process name from host to network byte order.
 *
 * @param name
 */
#define OMPI_PROCESS_NAME_HTON(n) \
    n.cellid = htonl((n).cellid); \
    n.jobid = htonl((n).jobid); \
    n.vpid = htonl((n).vpid);
                                                                                                                 
#define OMPI_PROCESS_NAME_ARGS(n) \
    n.cellid,n.jobid,n.vpid

/**
 *
 */

struct mca_iof_base_common_header_t {
    uint8_t  hdr_type;
    uint8_t  hdr_reserve;
    int16_t  hdr_status;
};
typedef struct mca_iof_base_common_header_t mca_iof_base_common_header_t;

#define MCA_IOF_BASE_HDR_CMN_NTOH(h) \
    (h).hdr_status = ntohs((h).hdr_status)

#define MCA_IOF_BASE_HDR_CMN_HTON(h) \
    (h).hdr_status = htons((h).hdr_status)


/**
 *
 */

struct mca_iof_base_msg_header_t {
    mca_iof_base_common_header_t hdr_common;
    ompi_process_name_t msg_src;
    int32_t  msg_tag;
    uint32_t msg_seq;
    uint32_t msg_len;
};
typedef struct mca_iof_base_msg_header_t mca_iof_base_msg_header_t;

#define MCA_IOF_BASE_HDR_MSG_NTOH(h) \
    MCA_IOF_BASE_HDR_CMN_NTOH((h).hdr_common); \
    OMPI_PROCESS_NAME_NTOH((h).msg_src); \
    (h).msg_tag = ntohl((h).msg_tag); \
    (h).msg_seq = ntohl((h).msg_seq); \
    (h).msg_len = ntohl((h).msg_len);

#define MCA_IOF_BASE_HDR_MSG_HTON(h) \
    MCA_IOF_BASE_HDR_CMN_HTON((h).hdr_common); \
    OMPI_PROCESS_NAME_HTON((h).msg_src); \
    (h).msg_tag = htonl((h).msg_tag); \
    (h).msg_seq = htonl((h).msg_seq); \
    (h).msg_len = htonl((h).msg_len);


/**
 * Publish/Unpublish
 */

struct mca_iof_base_pub_header_t {
    mca_iof_base_common_header_t hdr_common;
    ompi_process_name_t pub_name;
    ompi_process_name_t pub_proxy;
    int32_t pub_mask;
    int32_t pub_tag;
};
typedef struct mca_iof_base_pub_header_t mca_iof_base_pub_header_t;

#define MCA_IOF_BASE_HDR_PUB_NTOH(h) \
    MCA_IOF_BASE_HDR_CMN_NTOH((h).hdr_common); \
    OMPI_PROCESS_NAME_NTOH((h).pub_proxy); \
    OMPI_PROCESS_NAME_NTOH((h).pub_name); \
    (h).pub_mask = ntohl((h).pub_mask); \
    (h).pub_tag = ntohl((h).pub_tag);

#define MCA_IOF_BASE_HDR_PUB_HTON(h) \
    MCA_IOF_BASE_HDR_CMN_HTON((h).hdr_common); \
    OMPI_PROCESS_NAME_HTON((h).pub_name); \
    OMPI_PROCESS_NAME_HTON((h).pub_proxy); \
    (h).pub_mask = htonl((h).pub_mask); \
    (h).pub_tag = htonl((h).pub_tag);

/**
 * Subscription message.
 */

struct mca_iof_base_sub_header_t {
    mca_iof_base_common_header_t hdr_common;
    ompi_process_name_t src_name;
    ompi_ns_cmp_bitmask_t src_mask;
    int32_t src_tag;
    ompi_process_name_t dst_name;
    ompi_ns_cmp_bitmask_t dst_mask;
    int32_t dst_tag;
};
typedef struct mca_iof_base_sub_header_t mca_iof_base_sub_header_t;

#define MCA_IOF_BASE_HDR_SUB_NTOH(h) \
    MCA_IOF_BASE_HDR_CMN_NTOH((h).hdr_common); \
    OMPI_PROCESS_NAME_NTOH((h).src_name); \
    (h).src_tag = ntohl((h).src_tag); \
    OMPI_PROCESS_NAME_NTOH((h).dst_name); \
    (h).dst_tag = ntohl((h).dst_tag);

#define MCA_IOF_BASE_HDR_SUB_HTON(h) \
    MCA_IOF_BASE_HDR_CMN_HTON((h).hdr_common); \
    OMPI_PROCESS_NAME_HTON((h).src_name); \
    (h).src_tag = htonl((h).src_tag); \
    OMPI_PROCESS_NAME_HTON((h).dst_name); \
    (h).dst_tag = htonl((h).dst_tag);

/**
 *
 */

union mca_iof_base_header_t {
    mca_iof_base_common_header_t hdr_common;
    mca_iof_base_msg_header_t hdr_msg;
    mca_iof_base_sub_header_t hdr_sub;
    mca_iof_base_pub_header_t hdr_pub;
};
typedef union mca_iof_base_header_t mca_iof_base_header_t;


#endif

