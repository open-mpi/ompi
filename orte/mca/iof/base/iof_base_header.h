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
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _IOF_BASE_HEADER_
#define _IOF_BASE_HEADER_

#include "orte_config.h"
#include "orte/mca/iof/iof.h"

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
#define ORTE_IOF_BASE_HDR_CLOSE  6


/*
 * Maximum size of msg 
 */
#define ORTE_IOF_BASE_MSG_MAX 2048

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
    orte_process_name_t msg_origin;
    orte_process_name_t msg_proxy;
    int32_t  msg_tag;
    uint32_t msg_seq;
    uint32_t msg_len;
};
typedef struct orte_iof_base_msg_header_t orte_iof_base_msg_header_t;

#define ORTE_IOF_BASE_HDR_MSG_NTOH(h) \
    ORTE_IOF_BASE_HDR_CMN_NTOH((h).hdr_common); \
    ORTE_PROCESS_NAME_NTOH((h).msg_origin); \
    ORTE_PROCESS_NAME_NTOH((h).msg_proxy); \
    (h).msg_tag = ntohl((h).msg_tag); \
    (h).msg_seq = ntohl((h).msg_seq); \
    (h).msg_len = ntohl((h).msg_len);

#define ORTE_IOF_BASE_HDR_MSG_HTON(h) \
    ORTE_IOF_BASE_HDR_CMN_HTON((h).hdr_common); \
    ORTE_PROCESS_NAME_HTON((h).msg_origin); \
    ORTE_PROCESS_NAME_HTON((h).msg_proxy); \
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
    orte_process_name_t origin_name;
    orte_ns_cmp_bitmask_t origin_mask;
    int32_t origin_tag;
    orte_process_name_t target_name;
    orte_ns_cmp_bitmask_t target_mask;
    int32_t target_tag;
};
typedef struct orte_iof_base_sub_header_t orte_iof_base_sub_header_t;

#define ORTE_IOF_BASE_HDR_SUB_NTOH(h) \
    ORTE_IOF_BASE_HDR_CMN_NTOH((h).hdr_common); \
    ORTE_PROCESS_NAME_NTOH((h).origin_name); \
    (h).origin_tag = ntohl((h).origin_tag); \
    ORTE_PROCESS_NAME_NTOH((h).target_name); \
    (h).target_tag = ntohl((h).target_tag);

#define ORTE_IOF_BASE_HDR_SUB_HTON(h) \
    ORTE_IOF_BASE_HDR_CMN_HTON((h).hdr_common); \
    ORTE_PROCESS_NAME_HTON((h).origin_name); \
    (h).origin_tag = htonl((h).origin_tag); \
    ORTE_PROCESS_NAME_HTON((h).target_name); \
    (h).target_tag = htonl((h).target_tag);

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

