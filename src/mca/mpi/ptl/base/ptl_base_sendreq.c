/*
 * $HEADER$
 */
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"


lam_class_info_t mca_ptl_base_send_request_cls = { 
    "mca_ptl_base_send_request_t", 
    &mca_pml_base_request_cls,
    (class_init_t) mca_ptl_base_send_request_init, 
    (class_destroy_t) mca_ptl_base_send_request_destroy 
};


void mca_ptl_base_send_request_init(mca_ptl_base_send_request_t* req)
{
    SUPER_INIT(req, &mca_pml_base_request_cls);
    lam_list_init(&req->req_unacked_frags);
}

void mca_ptl_base_send_request_destroy(mca_ptl_base_send_request_t* req)
{
    lam_list_destroy(&req->req_unacked_frags);
    SUPER_DESTROY(&req->req_unacked_frags, &mca_pml_base_request_cls);
}

