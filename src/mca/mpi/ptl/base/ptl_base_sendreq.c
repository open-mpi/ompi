/*
 * $HEADER$
 */
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"

static void mca_ptl_base_send_request_init(mca_ptl_base_send_request_t* req);
static void mca_ptl_base_send_request_destroy(mca_ptl_base_send_request_t* req);


lam_class_info_t mca_ptl_base_send_request_cls = { 
    "mca_ptl_base_send_request_t", 
    &mca_pml_base_request_cls,
    (class_init_t) mca_ptl_base_send_request_init, 
    (class_destroy_t) mca_ptl_base_send_request_destroy 
};


static void mca_ptl_base_send_request_init(mca_ptl_base_send_request_t* req)
{
    SUPER_INIT(req, &mca_pml_base_request_cls);
    STATIC_INIT(req->req_unacked_frags, &lam_list_cls);
}

static void mca_ptl_base_send_request_destroy(mca_ptl_base_send_request_t* req)
{
    STATIC_DESTROY(req->req_unacked_frags);
    SUPER_DESTROY(&req->req_unacked_frags, &mca_pml_base_request_cls);
}

