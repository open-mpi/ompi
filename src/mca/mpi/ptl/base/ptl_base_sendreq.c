/*
 * $HEADER$
 */
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "mca/mpi/ptl/base/ptl_base_sendfrag.h"

static void mca_ptl_base_send_request_construct(mca_ptl_base_send_request_t* req);
static void mca_ptl_base_send_request_destruct(mca_ptl_base_send_request_t* req);


lam_class_info_t mca_ptl_base_send_request_t_class_info = { 
    "mca_ptl_base_send_request_t", 
    CLASS_INFO(mca_pml_base_request_t),
    (lam_construct_t) mca_ptl_base_send_request_construct, 
    (lam_destruct_t) mca_ptl_base_send_request_destruct 
};


static void mca_ptl_base_send_request_construct(mca_ptl_base_send_request_t* req)
{
    OBJ_CONSTRUCT_SUPER(req, mca_pml_base_request_t);
    OBJ_CONSTRUCT(&req->req_unacked_frags, lam_list_t);
}

static void mca_ptl_base_send_request_destruct(mca_ptl_base_send_request_t* req)
{
    OBJ_DESTRUCT(&req->req_unacked_frags);
    OBJ_DESTRUCT_SUPER(&req->req_unacked_frags, mca_pml_base_request_t);
}

void mca_ptl_base_send_request_progress(
    mca_ptl_base_send_request_t* req, 
    mca_ptl_base_send_frag_t* frag)
{
}


