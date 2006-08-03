/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifndef OMPI_MCA_OSC_RDMA_HDR_H
#define OMPI_MCA_OSC_RDMA_HDR_H

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/types.h"

#define OMPI_OSC_RDMA_HDR_PUT        0x0001
#define OMPI_OSC_RDMA_HDR_ACC        0x0002
#define OMPI_OSC_RDMA_HDR_GET        0x0004
#define OMPI_OSC_RDMA_HDR_REPLY      0x0008
#define OMPI_OSC_RDMA_HDR_POST       0x0010
#define OMPI_OSC_RDMA_HDR_COMPLETE   0x0020
#define OMPI_OSC_RDMA_HDR_LOCK_REQ   0x0040
#define OMPI_OSC_RDMA_HDR_UNLOCK_REQ 0x0080

#define OMPI_OSC_RDMA_HDR_FLAG_NBO   0x0001

struct ompi_osc_rdma_base_header_t {
    uint8_t hdr_type;
    /* eventually, this will include endian information */
    uint8_t hdr_flags;
};
typedef struct ompi_osc_rdma_base_header_t ompi_osc_rdma_base_header_t;

#define OMPI_OSC_RDMA_BASE_HDR_NTOH(h)
#define OMPI_OSC_RDMA_BASE_HDR_HTON(h)

struct ompi_osc_rdma_send_header_t {
    ompi_osc_rdma_base_header_t hdr_base;
    uint16_t hdr_windx;

    int32_t hdr_origin;
    ompi_ptr_t hdr_origin_sendreq;
    int32_t hdr_origin_tag;

    int32_t hdr_target_disp;
    int32_t hdr_target_count;
    int32_t hdr_target_op;

    int32_t hdr_msg_length; /* 0 if payload is not included */
};
typedef struct ompi_osc_rdma_send_header_t ompi_osc_rdma_send_header_t;

#define OMPI_OSC_RDMA_SEND_HDR_HTON(hdr) \
    do { \
        OMPI_OSC_RDMA_BASE_HDR_HTON((hdr).hdr_base) \
        (hdr).hdr_windx = htons((hdr).hdr_windx); \
        (hdr).hdr_origin = htonl((hdr).hdr_origin); \
        (hdr).hdr_origin_tag = htonl((hdr).hdr_origin_tag); \
        (hdr).hdr_target_disp = htonl((hdr).hdr_target_disp); \
        (hdr).hdr_target_count = htonl((hdr).hdr_target_count); \
        (hdr).hdr_target_op = htonl((hdr).hdr_target_op); \
        (hdr).hdr_msg_length = htonl((hdr).hdr_msg_length); \
    } while (0)

#define OMPI_OSC_RDMA_SEND_HDR_NTOH(hdr) \
    do { \
        OMPI_OSC_RDMA_BASE_HDR_NTOH((hdr).hdr_base) \
        (hdr).hdr_windx = ntohs((hdr).hdr_windx); \
        (hdr).hdr_origin = ntohl((hdr).hdr_origin); \
        (hdr).hdr_origin_tag = ntohl((hdr).hdr_origin_tag); \
        (hdr).hdr_target_disp = ntohl((hdr).hdr_target_disp); \
        (hdr).hdr_target_count = ntohl((hdr).hdr_target_count); \
        (hdr).hdr_target_op = ntohl((hdr).hdr_target_op); \
        (hdr).hdr_msg_length = ntohl((hdr).hdr_msg_length); \
    } while (0)


struct ompi_osc_rdma_reply_header_t {
    ompi_osc_rdma_base_header_t hdr_base;

    ompi_ptr_t hdr_origin_sendreq;

    int32_t hdr_target_tag;
    int32_t hdr_msg_length;
};
typedef struct ompi_osc_rdma_reply_header_t ompi_osc_rdma_reply_header_t;

#define OMPI_OSC_RDMA_REPLY_HDR_HTON(hdr) \
    do { \
        OMPI_OSC_RDMA_BASE_HDR_HTON((hdr).hdr_base) \
        (hdr).hdr_target_tag = htonl((hdr).hdr_target_tag); \
        (hdr).hdr_msg_length = htonl((hdr).hdr_msg_length); \
    } while (0)

#define OMPI_OSC_RDMA_REPLY_HDR_NTOH(hdr) \
    do { \
        OMPI_OSC_RDMA_BASE_HDR_NTOH((hdr).hdr_base) \
        (hdr).hdr_target_tag = ntohl((hdr).hdr_target_tag); \
        (hdr).hdr_msg_length = ntohl((hdr).hdr_msg_length); \
    } while (0)


struct ompi_osc_rdma_control_header_t {
    ompi_osc_rdma_base_header_t hdr_base;
    int16_t hdr_windx;
    int32_t hdr_value[2];
};
typedef struct ompi_osc_rdma_control_header_t ompi_osc_rdma_control_header_t;

#define OMPI_OSC_RDMA_CONTROL_HDR_HTON(hdr) \
    do { \
        OMPI_OSC_RDMA_BASE_HDR_HTON((hdr).hdr_base) \
        (hdr).hdr_windx = htons((hdr).hdr_windx); \
        (hdr).hdr_value[0] = htonl((hdr).hdr_value[0]); \
        (hdr).hdr_value[1] = htonl((hdr).hdr_value[1]); \
    } while (0)

#define OMPI_OSC_RDMA_CONTROL_HDR_NTOH(hdr) \
    do { \
        OMPI_OSC_RDMA_BASE_HDR_NTOH((hdr).hdr_base) \
        (hdr).hdr_windx = ntohs((hdr).hdr_windx); \
        (hdr).hdr_value[0] = ntohl((hdr).hdr_value[0]); \
        (hdr).hdr_value[1] = ntohl((hdr).hdr_value[1]); \
    } while (0)

#endif /* OMPI_MCA_OSC_RDMA_HDR_H */
