/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "lam/constants.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/pml/base/pml_base_sendreq.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"


/*
 *  Start a send request by calling the scheduler to stripe
 *  the message across available PTLs. If the scheduler cannot 
 *  fragment the entire message due to resource constraints,
 *  queue for later delivery.
 */

int mca_pml_teg_send_request_start(mca_pml_base_send_request_t* req)
{
    bool complete;
    int rc = mca_pml_teg_send_request_schedule(req, &complete);
    if(rc != LAM_SUCCESS)
        return rc;
    if(complete == false) {
        THREAD_LOCK(&mca_pml_teg.teg_lock);
        lam_list_append(&mca_pml_teg.teg_incomplete_sends, (lam_list_item_t*)req);
        THREAD_UNLOCK(&mca_pml_teg.teg_lock);
    }
    return LAM_SUCCESS;
}


/*
 *  Schedule message delivery across potentially multiple PTLs. Use
 *  two seperate PTL pools. One for the first fragment (low latency)
 *  and a second for the remaining fragments (high bandwidth). Note that
 *  the same PTL could exist in both pools. 
 *
 *  If the first fragment cannot be scheduled due to a resource constraint,
 *  the entire message is queued for later delivery by the progress engine. 
 *  Likewise, if the entire message cannot be fragmented, the message will 
 *  be queued and the remainder of the message scheduled/fragmented
 *  by the progress engine.
 *
 */

int mca_pml_teg_send_request_schedule(mca_pml_base_send_request_t* req, bool* complete)
{
    lam_proc_t *proc = lam_comm_lookup_peer(req->super.req_communicator, req->super.req_peer);
    mca_pml_proc_t* proc_pml = proc->proc_pml;

    /* allocate first fragment, if the first PTL in the list 
     * cannot allocate resources for the fragment, try the next 
     * available.
    */
    if(req->req_frags_allocated == 0) {
        size_t num_ptl_avail = proc_pml->proc_ptl_first.ptl_size;
        size_t i;
        for(i = 0; i < num_ptl_avail; i++) {
            mca_ptl_info_t* ptl_info = mca_ptl_array_get_next(&proc_pml->proc_ptl_first);
            mca_ptl_t* ptl = ptl_info->ptl;
            int rc = ptl->ptl_fragment(ptl, req, ptl->ptl_frag_first_size);
            if (rc == LAM_SUCCESS)
                break;
            else if (rc != LAM_ERR_TEMP_OUT_OF_RESOURCE)
                return rc;
        }
        /* has first fragment been allocated? */
        if(req->req_frags_allocated == 0) {
            *complete = false;
            return LAM_SUCCESS;
        }
    }

    /* allocate remaining bytes to PTLs */
    size_t bytes_remaining = req->req_length - req->req_bytes_fragmented;
    size_t num_ptl_avail = proc_pml->proc_ptl_next.ptl_size;
    size_t num_ptl = 0;
    while(bytes_remaining > 0 && num_ptl++ < num_ptl_avail) {
        mca_ptl_info_t* ptl_info = mca_ptl_array_get_next(&proc_pml->proc_ptl_next);
        mca_ptl_t* ptl = ptl_info->ptl;

        /* if this is the last PTL that is available to use, or the number of 
         * bytes remaining in the message is less than the PTLs minimum fragment 
         * size, then go ahead and give the rest of the message to this PTL.
         */
        size_t bytes_to_frag;
        if(num_ptl == num_ptl_avail || bytes_remaining < ptl->ptl_frag_min_size)
            bytes_to_frag = bytes_remaining;

        /* otherwise attempt to give the PTL a percentage of the message
         * based on a weighting factor. for simplicity calculate this as  
         * a percentage of the overall message length (regardless of amount 
         * previously assigned)
         */
        else {
            bytes_to_frag = ptl_info->ptl_weight * req->req_length;
            if(bytes_to_frag > bytes_remaining)
                bytes_to_frag = bytes_remaining;
        }

        int rc = ptl->ptl_fragment(ptl, req, bytes_to_frag); 
        if(rc != LAM_SUCCESS && rc != LAM_ERR_TEMP_OUT_OF_RESOURCE)
            return rc;
        bytes_remaining = req->req_length = req->req_bytes_fragmented;
    }
    *complete = (req->req_length == req->req_bytes_fragmented);
    return LAM_SUCCESS;
}


/*
 *  Check for queued messages that need to be scheduled. 
 *
 */

void mca_pml_teg_send_request_push()
{
    THREAD_LOCK(&mca_pml_teg.teg_lock);
    mca_pml_base_send_request_t* req;
    for(req =  (mca_pml_base_send_request_t*)lam_list_get_first(&mca_pml_teg.teg_incomplete_sends); 
        req != (mca_pml_base_send_request_t*)lam_list_get_end(&mca_pml_teg.teg_incomplete_sends);
        req =  (mca_pml_base_send_request_t*)lam_list_get_next(req)) {

        bool complete;
        int rc = mca_pml_teg_send_request_schedule(req, &complete);
        if(rc != LAM_SUCCESS) {
             // FIX 
             exit(-1);
        }
        if(complete) {
            req = (mca_pml_base_send_request_t*)lam_dbl_remove(
                &mca_pml_teg.teg_incomplete_sends, (lam_list_item_t*)req);
        }
    }
    THREAD_UNLOCK(&mca_pml_teg.teg_lock);
}


