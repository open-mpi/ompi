/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "mca/mpi/ptl/base/ptl_base_comm.h"
#include "mca/mpi/ptl/base/ptl_base_recvreq.h"
#include "mca/mpi/ptl/base/ptl_base_recvfrag.h"


lam_class_info_t mca_ptl_base_recv_request_cls = { 
    "mca_ptl_base_recv_request_t", 
    &mca_pml_base_request_cls,
    (class_init_t) mca_ptl_base_recv_request_init, 
    (class_destroy_t) mca_ptl_base_recv_request_destroy 
};
                                                                                                 

void mca_ptl_base_recv_request_init(mca_ptl_base_recv_request_t* req)
{
    SUPER_INIT(req, &mca_pml_base_request_cls);
}

void mca_ptl_base_recv_request_destroy(mca_ptl_base_recv_request_t* req)
{
    SUPER_DESTROY(req, &mca_pml_base_request_cls);
}


int mca_ptl_base_recv_request_match_specific(mca_ptl_base_recv_request_t* req)
{
    mca_pml_comm_t* comm = req->super.req_communicator->c_pml_comm;
    int req_peer = req->super.req_peer;
    /* FIX - need to check for matching fragments */
    THREAD_LOCK(comm->c_matching_lock+req_peer);
    lam_list_append(comm->c_specific_receives+req_peer, (lam_list_item_t*)req);
    THREAD_UNLOCK(comm->c_matching_lock+req->super.req_peer);
    return LAM_SUCCESS;
}


int mca_ptl_base_recv_request_match_wild(mca_ptl_base_recv_request_t* req)
{
    mca_pml_comm_t* comm = req->super.req_communicator->c_pml_comm;
    /* FIX - need to check for matching fragments */
    THREAD_LOCK(&comm->c_wild_lock);
    lam_list_append(&comm->c_wild_receives, (lam_list_item_t*)req);
    THREAD_UNLOCK(&comm->c_wild_lock);
    return LAM_SUCCESS;
}

void mca_ptl_base_recv_request_progress(mca_ptl_base_recv_request_t* req, mca_ptl_base_recv_frag_t* frag)
{

}

