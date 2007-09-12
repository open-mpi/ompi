/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef __INCLUDE_VPROTOCOL_REQUEST_H_
#define __INCLUDE_VPROTOCOL_REQUEST_H_

#include "ompi_config.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "../vprotocol.h"
#include "../../pml_v.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/** Rebuild the PML requests pools to make room for extra space at end of each
  * request. 
  * The extra data is allocated in each requests so that it can hold instances  
  * of the req_recv_class and req_send_class fields of the 
  * mca_vprotocol_base_module_t. If those fields are NULL the requests are not
  * recreated.
  *  @return OMPI_SUCCESS or failure status
  */
OMPI_DECLSPEC int mca_vprotocol_base_request_parasite(void);    

    
/** Gives the actual address of the protocol specific part of a recv request.
 *   @param request (IN) the address of an ompi_request.
 *   @return address of the custom vprotocol data associated with the request.
 */
#define VPROTOCOL_RECV_REQ(req) \
    (((uintptr_t) req) + mca_pml_v.host_pml_req_recv_size)
    
/** Gives the actual address of the protocol specific part of a send request.
 *   @param request (IN) the address of an ompi_request.
 *   @return address of the custom vprotocol data associated with the request.
 */
#define VPROTOCOL_SEND_REQ(req) \
    (((uintptr_t) req) + mca_pml_v.host_pml_req_send_size)

/** Unified macro to get the actual address of the protocol specific part of 
  * an send - or - recv request.
  *  @param request (IN) the address of an ompi_request.
  *  @return address of the custom vprotocol data associated with the request.
  */
#define VPROTOCOL_REQ(req) (                                                   \
    assert((MCA_PML_REQUEST_SEND ==                                            \
                ((mca_pml_base_request_t *) req)->req_type) ||                 \
           (MCA_PML_REQUEST_RECV ==                                            \
                ((mca_pml_base_request_t *) req)->req_type)),                  \
    ((MCA_PML_REQUEST_SEND == ((mca_pml_base_request_t *) req)->req_type)      \
        ? VPROTOCOL_SEND_REQ(req)                                              \
        : VPROTOCOL_RECV_REQ(req)                                              \
    )                                                                          \
)

    
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* __INCLUDE_VPROTOCOL_REQUEST_H_ */
