/*
 * $HEADER$
 */

#ifndef LAM_PML_TEG_RECV_REQUEST_H
#define LAM_PML_TEG_RECV_REQUEST_H

#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "mca/mpi/ptl/base/ptl_base_recvreq.h"
#include "mca/mpi/ptl/base/ptl_base_recvfrag.h"


/*
 *  Allocate a recv request. 
 */
static inline mca_ptl_base_recv_request_t* mca_pml_teg_recv_request_alloc(int *rc)
{
    return (mca_ptl_base_recv_request_t*)lam_free_list_get(&mca_pml_teg.teg_recv_requests, rc);
}

static inline void mca_pml_teg_recv_request_return(mca_ptl_base_recv_request_t* request)
{
    request->super.req_status = MCA_PML_STATUS_INVALID;
    lam_free_list_return(&mca_pml_teg.teg_recv_requests, (lam_list_item_t*)request);
}

/*
 * Progress an initialized request.
 */
static inline int mca_pml_teg_recv_request_start(mca_ptl_base_recv_request_t* req)
{
    THREAD_SCOPED_LOCK(&mca_pml_teg.teg_lock,
        (req->req_sequence = mca_pml_teg.teg_recv_sequence++));
                                                                                                                             
    req->super.req_status = MCA_PML_STATUS_INCOMPLETE;
    if(req->super.req_peer == LAM_ANY_TAG) {
        mca_ptl_base_recv_request_match_wild(req);
    } else {
        mca_ptl_base_recv_request_match_specific(req);
    }
    return LAM_SUCCESS;
}

void mca_pml_teg_recv_request_progress(
    mca_ptl_base_recv_request_t* recv_request,
    mca_ptl_base_recv_frag_t* recv_frag
);


#endif

