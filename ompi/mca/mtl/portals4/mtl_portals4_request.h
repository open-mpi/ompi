/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MTL_PORTALS_REQUEST_H
#define OMPI_MTL_PORTALS_REQUEST_H

#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/mtl.h"

struct ompi_mtl_portals4_message_t;
struct ompi_mtl_portals4_pending_request_t;


typedef enum { portals4_req_isend,
               portals4_req_send,
               portals4_req_recv,
               portals4_req_probe,
               portals4_req_recv_short,
               portals4_req_flowctl
} ompi_mtl_portals4_request_type_t;


struct ompi_mtl_portals4_base_request_t {
    struct mca_mtl_request_t super;
    ompi_mtl_portals4_request_type_t type;
    int (*event_callback)(ptl_event_t *ev, struct ompi_mtl_portals4_base_request_t*);
};
typedef struct ompi_mtl_portals4_base_request_t ompi_mtl_portals4_base_request_t;


struct ompi_mtl_portals4_isend_request_t {
    ompi_mtl_portals4_base_request_t super;
    void *buffer_ptr;
    ptl_handle_me_t me_h;
    uint64_t opcount;
#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    struct ompi_mtl_portals4_pending_request_t *pending;
#endif
    uint32_t event_count;
};
typedef struct ompi_mtl_portals4_isend_request_t ompi_mtl_portals4_isend_request_t;


struct ompi_mtl_portals4_send_request_t {
    ompi_mtl_portals4_isend_request_t super;
    int retval;
    volatile int complete;
};
typedef struct ompi_mtl_portals4_send_request_t ompi_mtl_portals4_send_request_t;


struct ompi_mtl_portals4_recv_request_t {
    ompi_mtl_portals4_base_request_t super;
    void *buffer_ptr;
    ptl_handle_md_t md_h;
    ptl_handle_me_t me_h;
    struct opal_convertor_t *convertor;
    void *delivery_ptr;
    size_t delivery_len;
    volatile bool req_started;
#if OPAL_ENABLE_DEBUG
    uint64_t opcount;
    ptl_hdr_data_t hdr_data;
#endif
};
typedef struct ompi_mtl_portals4_recv_request_t ompi_mtl_portals4_recv_request_t;


struct ompi_mtl_portals4_recv_short_request_t {
    ompi_mtl_portals4_base_request_t super;
    struct ompi_mtl_portals4_recv_short_block_t *block;
};
typedef struct ompi_mtl_portals4_recv_short_request_t ompi_mtl_portals4_recv_short_request_t;


struct ompi_mtl_portals4_probe_request_t {
    ompi_mtl_portals4_base_request_t super;
    volatile int req_complete;
    int found_match;
    struct ompi_status_public_t status;
    struct ompi_mtl_portals4_message_t *message;
};
typedef struct ompi_mtl_portals4_probe_request_t ompi_mtl_portals4_probe_request_t;


struct ompi_mtl_portals4_request_t {
    union {
        ompi_mtl_portals4_isend_request_t isend;
        ompi_mtl_portals4_send_request_t send;
        ompi_mtl_portals4_recv_request_t recv;
        ompi_mtl_portals4_recv_short_request_t recv_short;
        ompi_mtl_portals4_probe_request_t probe;
    } u;
};
typedef struct ompi_mtl_portals4_request_t ompi_mtl_portals4_request_t;


#endif
