/*
 * $HEADER$
 */
#include <string.h>
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"

static void mca_ptl_base_send_request_construct(mca_ptl_base_send_request_t* req);
static void mca_ptl_base_send_request_destruct(mca_ptl_base_send_request_t* req);


lam_class_t mca_ptl_base_send_request_t_class = { 
    "mca_ptl_base_send_request_t", 
    OBJ_CLASS(mca_pml_base_request_t),
    (lam_construct_t) mca_ptl_base_send_request_construct, 
    (lam_destruct_t) mca_ptl_base_send_request_destruct 
};


static void mca_ptl_base_send_request_construct(mca_ptl_base_send_request_t* request)
{
    /* no need to reinit for every send -- never changes */
    request->super.req_type = MCA_PML_REQUEST_SEND;
    OBJ_CONSTRUCT(&request->req_convertor, lam_convertor_t);
}

static void mca_ptl_base_send_request_destruct(mca_ptl_base_send_request_t* req)
{
}

