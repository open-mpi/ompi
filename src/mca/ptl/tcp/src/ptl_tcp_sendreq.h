/*
 * $HEADER$
 */
/**
 * @file
 */

#ifndef MCA_PTL_TCP_SEND_REQUEST_H
#define MCA_PTL_TCP_SEND_REQUEST_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "ompi_config.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "ptl_tcp_sendfrag.h"

extern ompi_class_t mca_ptl_tcp_send_request_t_class;

/**
 * TCP send request derived type. The send request contains both the
 * base send request, and space for the first TCP send fragment descriptor.
 * This avoids the overhead of a second allocation for the initial send 
 * fragment on every send request.
 */
struct mca_ptl_tcp_send_request_t {
   mca_ptl_base_send_request_t super;
   mca_ptl_tcp_send_frag_t req_frag; /* first fragment */
};
typedef struct mca_ptl_tcp_send_request_t mca_ptl_tcp_send_request_t;


#endif

