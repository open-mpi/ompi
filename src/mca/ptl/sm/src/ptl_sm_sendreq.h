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
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_sm_frag.h"

OBJ_CLASS_DECLARATION(mca_ptl_sm_send_request_t);


/**
 * Shared Memory (SM) send request derived type. The send request contains 
 * both base send request, and a pointer to the first fragment descriptor.
 */
struct mca_ptl_sm_send_request_t {

    /* base send descriptor */
    mca_pml_base_send_request_t super;
  
    /* pointer to first fragment descriptor */
    mca_ptl_sm_frag_t *req_frag;

};
typedef struct mca_ptl_sm_send_request_t mca_ptl_sm_send_request_t;


#endif

