

#ifndef MCA_PTL_IB_SEND_REQUEST_H
#define MCA_PTL_IB_SEND_REQUEST_H


#include "ompi_config.h"
#include "mca/pml/base/pml_base_sendreq.h"
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
       mca_pml_base_send_request_t          super;

       mca_ptl_ib_send_frag_t               *req_frag; 
       /* first fragment */
       char                                 req_buf[8];
       /* temporary buffer to hold VAPI_rkey_t */
};
typedef struct mca_ptl_ib_send_request_t mca_ptl_ib_send_request_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
