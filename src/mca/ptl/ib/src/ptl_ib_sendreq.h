/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
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

#ifndef MCA_PTL_IB_SEND_REQUEST_H
#define MCA_PTL_IB_SEND_REQUEST_H


#include "ompi_config.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "ptl_ib_sendfrag.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OBJ_CLASS_DECLARATION(mca_ptl_ib_send_request_t);

/**
 * IB send request derived type. The send request contains both the
 * base send request, and space for the first IB send fragment
 * descriptor.
 * This avoids the overhead of a second allocation for the initial send
 * fragment on every send request.
 */
struct mca_ptl_ib_send_request_t {
       mca_ptl_base_send_request_t          super;
       mca_ptl_ib_send_frag_t              *req_frag; 
       VAPI_rkey_t                          req_key;
};
typedef struct mca_ptl_ib_send_request_t mca_ptl_ib_send_request_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
