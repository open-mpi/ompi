/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
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

#ifndef MCA_PTL_TCP_SEND_REQUEST_H
#define MCA_PTL_TCP_SEND_REQUEST_H

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#include "ompi_config.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_tcp_sendfrag.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
extern ompi_class_t mca_ptl_tcp_send_request_t_class;
/**
 * TCP send request derived type. The send request contains both the
 * base send request, and space for the first TCP send fragment descriptor.
 * This avoids the overhead of a second allocation for the initial send 
 * fragment on every send request.
 */
struct mca_ptl_tcp_send_request_t {
   mca_pml_base_send_request_t super;
   mca_ptl_tcp_send_frag_t req_frag; /* first fragment */
};
typedef struct mca_ptl_tcp_send_request_t mca_ptl_tcp_send_request_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

