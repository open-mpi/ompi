#ifndef _IOF_BASE_HEADER_
#define _IOF_BASE_HEADER_

#include "ompi_config.h"
#include "mca/iof/iof.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
                                                                                                                                        
#define ORTE_IOF_BASE_HDR_MSG    0
#define ORTE_IOF_BASE_HDR_ACK    1 
#define ORTE_IOF_BASE_HDR_PUB    2
#define ORTE_IOF_BASE_HDR_UNPUB  3
#define ORTE_IOF_BASE_HDR_SUB    4
#define ORTE_IOF_BASE_HDR_UNSUB  5


/*
 * Maximum size of msg 
 */
#define ORTE_IOF_BASE_MSG_MAX 2048

/**
 * Convert process name from network to host byte order.
 *
 * @param name
 */
#define ORTE_PROCESS_NAME_NTOH(n) \
    n.cellid = ntohl((n).cellid); \
    n.jobid = ntohl((n).jobid); \
    n.vpid = ntohl((n).vpid);
                                                                                                                 
/**
 * Convert process name from host to network byte order.
 *
 * @param name
 */
#define ORTE_PROCESS_NAME_HTON(n) \
    n.cellid = htonl((n).cellid); \
    n.jobid = htonl((n).jobid); \
    n.vpid = htonl((n).vpid);
                                                                                                                 
#define ORTE_PROCESS_NAME_ARGS(n) \
    n.cellid,n.jobid,n.vpid

/**
 * Fields common to all headers.
 */

struct orte_iof_base_common_header_t {
    uint8_t  hdr_type;
    uint8_t  hdr_reserve;
    int16_t  hdr_status;
};
typedef struct orte_iof_base_common_header_t orte_iof_base_common_header_t;

#define ORTE_IOF_BASE_HDR_CMN_NTOH(h) \
    (h).hdr_status = ntohs((h).hdr_status)

#define ORTE_IOF_BASE_HDR_CMN_HTON(h) \
    (h).hdr_status = htons((h).hdr_status)


/**
 * Header for data.
 */

struct orte_iof_base_msg_header_t {
    orte_iof_base_common_header_t hdr_common;
    orte_process_name_t msg_src;
    int32_t  msg_tag;
    uint32_t msg_seq;
    uint32_t msg_len;
};
typedef struct orte_iof_base_msg_header_t orte_iof_base_msg_header_t;

#define ORTE_IOF_BASE_HDR_MSG_NTOH(h) \
    ORTE_IOF_BASE_HDR_CMN_NTOH((h).hdr_common); \
    ORTE_PROCESS_NAME_NTOH((h).msg_src); \
    (h).msg_tag = ntohl((h).msg_tag); \
    (h).msg_seq = ntohl((h).msg_seq); \
    (h).msg_len = ntohl((h).msg_len);

#define ORTE_IOF_BASE_HDR_MSG_HTON(h) \
    ORTE_IOF_BASE_HDR_CMN_HTON((h).hdr_common); \
    ORTE_PROCESS_NAME_HTON((h).msg_src); \
    (h).msg_tag = htonl((h).msg_tag); \
    (h).msg_seq = htonl((h).msg_seq); \
    (h).msg_len = htonl((h).msg_len);


/**
 * Publish/Unpublish
 */

struct orte_iof_base_pub_header_t {
    orte_iof_base_common_header_t hdr_common;
    orte_process_name_t pub_name;
    orte_process_name_t pub_proxy;
    int32_t pub_mask;
    int32_t pub_tag;
};
typedef struct orte_iof_base_pub_header_t orte_iof_base_pub_header_t;

#define ORTE_IOF_BASE_HDR_PUB_NTOH(h) \
    ORTE_IOF_BASE_HDR_CMN_NTOH((h).hdr_common); \
    ORTE_PROCESS_NAME_NTOH((h).pub_proxy); \
    ORTE_PROCESS_NAME_NTOH((h).pub_name); \
    (h).pub_mask = ntohl((h).pub_mask); \
    (h).pub_tag = ntohl((h).pub_tag);

#define ORTE_IOF_BASE_HDR_PUB_HTON(h) \
    ORTE_IOF_BASE_HDR_CMN_HTON((h).hdr_common); \
    ORTE_PROCESS_NAME_HTON((h).pub_name); \
    ORTE_PROCESS_NAME_HTON((h).pub_proxy); \
    (h).pub_mask = htonl((h).pub_mask); \
    (h).pub_tag = htonl((h).pub_tag);

/**
 * Subscription message.
 */

struct orte_iof_base_sub_header_t {
    orte_iof_base_common_header_t hdr_common;
    orte_process_name_t src_name;
    orte_ns_cmp_bitmask_t src_mask;
    int32_t src_tag;
    orte_process_name_t dst_name;
    orte_ns_cmp_bitmask_t dst_mask;
    int32_t dst_tag;
};
typedef struct orte_iof_base_sub_header_t orte_iof_base_sub_header_t;

#define ORTE_IOF_BASE_HDR_SUB_NTOH(h) \
    ORTE_IOF_BASE_HDR_CMN_NTOH((h).hdr_common); \
    ORTE_PROCESS_NAME_NTOH((h).src_name); \
    (h).src_tag = ntohl((h).src_tag); \
    ORTE_PROCESS_NAME_NTOH((h).dst_name); \
    (h).dst_tag = ntohl((h).dst_tag);

#define ORTE_IOF_BASE_HDR_SUB_HTON(h) \
    ORTE_IOF_BASE_HDR_CMN_HTON((h).hdr_common); \
    ORTE_PROCESS_NAME_HTON((h).src_name); \
    (h).src_tag = htonl((h).src_tag); \
    ORTE_PROCESS_NAME_HTON((h).dst_name); \
    (h).dst_tag = htonl((h).dst_tag);

/**
 * Union of all header types.
 */

union orte_iof_base_header_t {
    orte_iof_base_common_header_t hdr_common;
    orte_iof_base_msg_header_t hdr_msg;
    orte_iof_base_sub_header_t hdr_sub;
    orte_iof_base_pub_header_t hdr_pub;
};
typedef union orte_iof_base_header_t orte_iof_base_header_t;


#endif

