/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PML_BASE_RECV_REQUEST_H
#define MCA_PML_BASE_RECV_REQUEST_H

#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_request.h"

extern ompi_class_t mca_ptl_base_recv_request_t_class;
struct mca_ptl_base_recv_frag_t;

/**
 * Base type for receive requests.
 */
struct mca_ptl_base_recv_request_t {
   mca_pml_base_request_t super;  /**< base request */
   size_t req_bytes_packed;       /**< size of message being received */
   size_t req_bytes_received;     /**< number of bytes received from network */
   size_t req_bytes_delivered;    /**< number of bytes delivered to user */
};
typedef struct mca_ptl_base_recv_request_t mca_ptl_base_recv_request_t;


/**
 * Initialize a receive request with call parameters.
 *
 * @param request (IN)       Receive request.
 * @param addr (IN)          User buffer.
 * @param count (IN)         Number of elements of indicated datatype.
 * @param datatype (IN)      User defined datatype.
 * @param src (IN)           Source rank w/in the communicator.
 * @param tag (IN)           User defined tag.
 * @param comm (IN)          Communicator.
 * @param persistent (IN)    Is this a ersistent request.
 */
#define MCA_PTL_BASE_RECV_REQUEST_INIT( \
    request, \
    addr, \
    count, \
    datatype, \
    src, \
    tag, \
    comm, \
    persistent) \
{ \
    (request)->req_bytes_packed = 0; \
    (request)->req_bytes_received = 0; \
    (request)->req_bytes_delivered = 0; \
    (request)->super.req_sequence = 0; \
    (request)->super.req_addr = addr; \
    (request)->super.req_count = count; \
    (request)->super.req_datatype = datatype; \
    (request)->super.req_peer = src; \
    (request)->super.req_tag = tag; \
    (request)->super.req_comm = comm; \
    (request)->super.req_proc = NULL; \
    (request)->super.req_persistent = persistent; \
    (request)->super.req_mpi_done = false; \
    (request)->super.req_pml_done = false; \
    (request)->super.req_free_called = false; \
}

/**
 * Attempt to match the request against the unexpected fragment list
 * for all source ranks w/in the communicator.
 *
 * @param request (IN)   Request to match.
 */
void mca_ptl_base_recv_request_match_wild(mca_ptl_base_recv_request_t* request);

/**
 * Attempt to match the request against the unexpected fragment list 
 * for a specific source rank.
 *
 * @param request (IN)   Request to match.
 */
void mca_ptl_base_recv_request_match_specific(mca_ptl_base_recv_request_t* request);

#endif

