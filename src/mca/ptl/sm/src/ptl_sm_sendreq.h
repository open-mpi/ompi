/*
 * $HEADER$
 */
/**
 * @file
 */

#ifndef MCA_PTL_SM_SEND_REQUEST_H
#define MCA_PTL_SM_SEND_REQUEST_H

#include <sys/types.h>
#include "ompi_config.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "ptl_sm_sendfrag.h"

OBJ_CLASS_DECLARATION(mca_ptl_sm_send_request_t);


/**
 * Shared Memory (SM) send request derived type. The send request contains both the
 * base send request, and space for the first send fragment descriptor.
 * This avoids the overhead of a second allocation for the initial send 
 * fragment on every send request.
 */
struct mca_ptl_sm_send_request_t {
   mca_ptl_base_send_request_t super;
   mca_ptl_sm_send_frag_t req_frag; /* first fragment */
};
typedef struct mca_ptl_sm_send_request_t mca_ptl_sm_send_request_t;


#endif

