#include "mca/mpi/ptl/base/ptl_base_comm.h"
#include "pml_teg_recvreq.h"


int mca_pml_teg_recv_request_start(mca_ptl_base_recv_request_t* req)
{
    int rc;
    THREAD_SCOPED_LOCK(&mca_pml_teg.teg_lock, 
        (req->req_sequence = mca_pml_teg.teg_recv_sequence++));

    req->super.req_status = MCA_PML_STATUS_INCOMPLETE;
    if(req->super.req_peer == LAM_ANY_TAG) {
        rc = mca_ptl_base_recv_request_match_wild(req);
    } else {
        rc = mca_ptl_base_recv_request_match_specific(req);
    }
    return rc;
}

