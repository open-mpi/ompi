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
/**
 * @file
 */
#ifndef MCA_PTL_BASE_RECV_REQUEST_H
#define MCA_PTL_BASE_RECV_REQUEST_H

#include "ompi/mca/ptl/ptl.h"
#include "ompi/mca/pml/base/pml_base_recvreq.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Base type for recv requests
 */
struct mca_ptl_base_recv_request_t {
    mca_pml_base_recv_request_t req_recv;
    size_t req_bytes_received;        /**< number of bytes received from network */
    size_t req_bytes_delivered;       /**< number of bytes delivered to user */
};
typedef struct mca_ptl_base_recv_request_t mca_ptl_base_recv_request_t;


OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_ptl_base_recv_request_t);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

