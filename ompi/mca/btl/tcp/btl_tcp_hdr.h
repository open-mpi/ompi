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

#ifndef MCA_BTL_TCP_HDR_H
#define MCA_BTL_TCP_HDR_H


#include "ompi_config.h"
#include "ompi/mca/btl/base/base.h"
#include "btl_tcp.h" 

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * TCP header.
 */

#define MCA_BTL_TCP_HDR_TYPE_SEND 1
#define MCA_BTL_TCP_HDR_TYPE_PUT  2
#define MCA_BTL_TCP_HDR_TYPE_GET  3


struct mca_btl_tcp_hdr_t {
    mca_btl_base_header_t base;
    uint8_t  type;
    uint16_t count;
    uint64_t size; 
}; 
typedef struct mca_btl_tcp_hdr_t mca_btl_tcp_hdr_t; 

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
