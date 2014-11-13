/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012-2013 Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MCA_OSC_PT2PT_HDR_H
#define OMPI_MCA_OSC_PT2PT_HDR_H

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/types.h"

enum ompi_osc_pt2pt_hdr_type_t {
    OMPI_OSC_PT2PT_HDR_TYPE_PUT          = 0x01,
    OMPI_OSC_PT2PT_HDR_TYPE_PUT_LONG     = 0x02,
    OMPI_OSC_PT2PT_HDR_TYPE_ACC          = 0x03,
    OMPI_OSC_PT2PT_HDR_TYPE_ACC_LONG     = 0x04,
    OMPI_OSC_PT2PT_HDR_TYPE_GET          = 0x05,
    OMPI_OSC_PT2PT_HDR_TYPE_CSWAP        = 0x06,
    OMPI_OSC_PT2PT_HDR_TYPE_CSWAP_LONG   = 0x07,
    OMPI_OSC_PT2PT_HDR_TYPE_GET_ACC      = 0x08,
    OMPI_OSC_PT2PT_HDR_TYPE_GET_ACC_LONG = 0x09,
    OMPI_OSC_PT2PT_HDR_TYPE_COMPLETE     = 0x10,
    OMPI_OSC_PT2PT_HDR_TYPE_POST         = 0x11,
    OMPI_OSC_PT2PT_HDR_TYPE_LOCK_REQ     = 0x12,
    OMPI_OSC_PT2PT_HDR_TYPE_LOCK_ACK     = 0x13,
    OMPI_OSC_PT2PT_HDR_TYPE_UNLOCK_REQ   = 0x14,
    OMPI_OSC_PT2PT_HDR_TYPE_UNLOCK_ACK   = 0x15,
    OMPI_OSC_PT2PT_HDR_TYPE_FLUSH_REQ    = 0x16,
    OMPI_OSC_PT2PT_HDR_TYPE_FLUSH_ACK    = 0x17,
    OMPI_OSC_PT2PT_HDR_TYPE_FRAG         = 0x20,
};
typedef enum ompi_osc_pt2pt_hdr_type_t ompi_osc_pt2pt_hdr_type_t;

#define OMPI_OSC_PT2PT_HDR_FLAG_NBO            0x01
#define OMPI_OSC_PT2PT_HDR_FLAG_VALID          0x02
#define OMPI_OSC_PT2PT_HDR_FLAG_PASSIVE_TARGET 0x04
#define OMPI_OSC_PT2PT_HDR_FLAG_LARGE_DATATYPE 0x08

struct ompi_osc_pt2pt_header_base_t {
    /** fragment type. 8 bits */
    uint8_t type;
    /** fragment flags. 8 bits */
    uint8_t flags;
};
typedef struct ompi_osc_pt2pt_header_base_t ompi_osc_pt2pt_header_base_t;

struct ompi_osc_pt2pt_header_put_t {
    ompi_osc_pt2pt_header_base_t base;

    uint16_t tag;
    uint32_t count;
    uint64_t len;
    uint64_t displacement;
};
typedef struct ompi_osc_pt2pt_header_put_t ompi_osc_pt2pt_header_put_t;

struct ompi_osc_pt2pt_header_acc_t {
    ompi_osc_pt2pt_header_base_t base;

    uint16_t tag;
    uint32_t count;
    uint32_t op;
    uint64_t len;
    uint64_t displacement;
};
typedef struct ompi_osc_pt2pt_header_acc_t ompi_osc_pt2pt_header_acc_t;

struct ompi_osc_pt2pt_header_get_t {
    ompi_osc_pt2pt_header_base_t base;

    uint16_t tag;
    uint32_t count;
    uint64_t len;
    uint64_t displacement;
};
typedef struct ompi_osc_pt2pt_header_get_t ompi_osc_pt2pt_header_get_t;

struct ompi_osc_pt2pt_header_complete_t {
    ompi_osc_pt2pt_header_base_t base;
    int frag_count;
};
typedef struct ompi_osc_pt2pt_header_complete_t ompi_osc_pt2pt_header_complete_t;

struct ompi_osc_pt2pt_header_cswap_t {
    ompi_osc_pt2pt_header_base_t base;

    uint16_t tag;

    uint32_t len;
    uint64_t displacement;
};
typedef struct ompi_osc_pt2pt_header_cswap_t ompi_osc_pt2pt_header_cswap_t;

struct ompi_osc_pt2pt_header_post_t {
    ompi_osc_pt2pt_header_base_t base;
    uint16_t windx;
};
typedef struct ompi_osc_pt2pt_header_post_t ompi_osc_pt2pt_header_post_t;

struct ompi_osc_pt2pt_header_lock_t {
    ompi_osc_pt2pt_header_base_t base;
    int32_t lock_type;
    uint64_t lock_ptr;
};
typedef struct ompi_osc_pt2pt_header_lock_t ompi_osc_pt2pt_header_lock_t;

struct ompi_osc_pt2pt_header_lock_ack_t {
    ompi_osc_pt2pt_header_base_t base;
    uint16_t windx;
    uint32_t source;
    uint64_t lock_ptr;
};
typedef struct ompi_osc_pt2pt_header_lock_ack_t ompi_osc_pt2pt_header_lock_ack_t;

struct ompi_osc_pt2pt_header_unlock_t {
    ompi_osc_pt2pt_header_base_t base;
    int32_t lock_type;
    uint32_t frag_count;
    uint64_t lock_ptr;
};
typedef struct ompi_osc_pt2pt_header_unlock_t ompi_osc_pt2pt_header_unlock_t;

struct ompi_osc_pt2pt_header_unlock_ack_t {
    ompi_osc_pt2pt_header_base_t base;
    uint64_t lock_ptr;
};
typedef struct ompi_osc_pt2pt_header_unlock_ack_t ompi_osc_pt2pt_header_unlock_ack_t;

struct ompi_osc_pt2pt_header_flush_t {
    ompi_osc_pt2pt_header_base_t base;
    uint32_t frag_count;
    uint64_t serial_number;
};
typedef struct ompi_osc_pt2pt_header_flush_t ompi_osc_pt2pt_header_flush_t;

struct ompi_osc_pt2pt_header_flush_ack_t {
    ompi_osc_pt2pt_header_base_t base;
    uint64_t serial_number;
};
typedef struct ompi_osc_pt2pt_header_flush_ack_t ompi_osc_pt2pt_header_flush_ack_t;

struct ompi_osc_pt2pt_frag_header_t {
    ompi_osc_pt2pt_header_base_t base;
    uint16_t windx; /* cid of communicator backing window (our window id) */
    uint32_t source; /* rank in window of source process */
    int32_t num_ops; /* number of operations in this buffer */
    uint32_t pad; /* ensure the fragment header is a multiple of 8 bytes */
};
typedef struct ompi_osc_pt2pt_frag_header_t ompi_osc_pt2pt_frag_header_t;

union ompi_osc_pt2pt_header_t {
    ompi_osc_pt2pt_header_base_t       base;
    ompi_osc_pt2pt_header_put_t        put;
    ompi_osc_pt2pt_header_acc_t        acc;
    ompi_osc_pt2pt_header_get_t        get;
    ompi_osc_pt2pt_header_complete_t   complete;
    ompi_osc_pt2pt_header_cswap_t      cswap;
    ompi_osc_pt2pt_header_post_t       post;
    ompi_osc_pt2pt_header_lock_t       lock;
    ompi_osc_pt2pt_header_lock_ack_t   lock_ack;
    ompi_osc_pt2pt_header_unlock_t     unlock;
    ompi_osc_pt2pt_header_unlock_ack_t unlock_ack;
    ompi_osc_pt2pt_header_flush_t      flush;
    ompi_osc_pt2pt_header_flush_ack_t  flush_ack;
    ompi_osc_pt2pt_frag_header_t       frag;
};
typedef union ompi_osc_pt2pt_header_t ompi_osc_pt2pt_header_t;

#endif /* OMPI_MCA_OSC_PT2PT_HDR_H */
