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
/**
 * @file
 */
#ifndef MCA_PTL_BASE_SEND_REQUEST_H
#define MCA_PTL_BASE_SEND_REQUEST_H

#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Base type for send requests 
 */
struct mca_ptl_base_send_request_t {
    mca_pml_base_send_request_t req_send;
    size_t req_offset;                       /**< number of bytes that have been scheduled */
    size_t req_bytes_sent;                   /**< number of bytes that have been sent */
    ompi_ptr_t req_peer_match;               /**< matched receive at peer */
    ompi_ptr_t req_peer_addr;                /**< peers remote buffer address */
    uint64_t req_peer_size;                  /**< size of peers remote buffer */
    bool req_cached;                         /**< has this request been obtained from the ptls cache */
    volatile int32_t req_lock;               /**< lock used by the scheduler */
    struct mca_ptl_base_module_t* req_ptl;   /**< ptl allocated for first fragment */
    struct mca_ptl_base_peer_t* req_peer;    /**< peer associated w/ this ptl */
};
typedef struct mca_ptl_base_send_request_t mca_ptl_base_send_request_t;


OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_ptl_base_send_request_t);


/**
 * Atomically increase the request offset.
 *
 * @param  request (IN)  Send request.
 * @param  offset (IN)   Increment.
 */

static inline void mca_ptl_base_send_request_offset(
    mca_ptl_base_send_request_t* request,
    size_t offset)
{
    OPAL_THREAD_ADD_SIZE_T((&request->req_offset), offset);
}


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

