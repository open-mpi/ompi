/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "lam/constants.h"
#include "mca/mpi/ptl/ptl.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_teg_sendreq.h"



/**
 *  Schedule message delivery across potentially multiple PTLs. 
 *
 *  @param request (IN)    Request to schedule
 *  @param complete (OUT)  Completion status
 *  @return status         Error status
 *
 *  The scheduler uses two seperate PTL pools. One for the first fragment 
 *  (low latency) and a second for the remaining fragments (high bandwidth). 
 *  Note that the same PTL could exist in both pools. 
 *
 *  If the first fragment cannot be scheduled due to a resource constraint,
 *  the entire message is queued for later delivery by the progress engine. 
 *  Likewise, if the entire message cannot be fragmented, the message will 
 *  be queued and the remainder of the message scheduled/fragmented
 *  by the progress engine.
 *
 */

void mca_pml_teg_send_request_schedule(mca_ptl_base_send_request_t* req)
{
    lam_proc_t *proc = lam_comm_lookup_peer(req->super.req_comm, req->super.req_peer);
    mca_pml_proc_t* proc_pml = proc->proc_pml;

    /* allocate remaining bytes to PTLs */
    size_t bytes_remaining = req->super.req_length - req->req_offset;
    size_t num_ptl_avail = proc_pml->proc_ptl_next.ptl_size;
    size_t num_ptl = 0;
    while(bytes_remaining > 0 && num_ptl++ < num_ptl_avail) {
        mca_ptl_proc_t* ptl_proc = mca_ptl_array_get_next(&proc_pml->proc_ptl_next);
        mca_ptl_t* ptl = ptl_proc->ptl;

        /* if this is the last PTL that is available to use, or the number of 
         * bytes remaining in the message is less than the PTLs minimum fragment 
         * size, then go ahead and give the rest of the message to this PTL.
         */
        size_t bytes_to_frag;
        if(num_ptl == num_ptl_avail || bytes_remaining < ptl->ptl_min_frag_size)
            bytes_to_frag = bytes_remaining;

        /* otherwise attempt to give the PTL a percentage of the message
         * based on a weighting factor. for simplicity calculate this as  
         * a percentage of the overall message length (regardless of amount 
         * previously assigned)
         */
        else {
            bytes_to_frag = ptl_proc->ptl_weight * req->super.req_length;
            if(bytes_to_frag > bytes_remaining)
                bytes_to_frag = bytes_remaining;
        }

        int rc = ptl->ptl_send(ptl, ptl_proc->ptl_peer, req, bytes_to_frag); 
        if(rc == LAM_SUCCESS)
            bytes_remaining = req->super.req_length - req->req_offset;
    }

    /* unable to complete send - signal request failed */
    if(bytes_remaining > 0) {
        lam_mutex_lock(&mca_pml_teg.teg_request_lock);
        req->super.req_mpi_done = true;
        /* FIX - set status correctly */
        if(mca_pml_teg.teg_request_waiting)
            lam_condition_broadcast(&mca_pml_teg.teg_request_cond);
        lam_mutex_unlock(&mca_pml_teg.teg_request_lock);
    }
}


void mca_pml_teg_send_request_progress(
    mca_ptl_base_send_request_t* req,
    mca_ptl_base_send_frag_t* frag)
{
    bool complete = false;
    lam_mutex_lock(&mca_pml_teg.teg_request_lock);
    req->req_bytes_sent += frag->super.frag_size;
    if (req->req_bytes_sent >= req->super.req_length) {
        req->super.req_mpi_done = true;
        if(mca_pml_teg.teg_request_waiting) {
            lam_condition_broadcast(&mca_pml_teg.teg_request_cond);
        }
        complete = true;
    } 
    lam_mutex_unlock(&mca_pml_teg.teg_request_lock);

    /* if first fragment - schedule remaining fragments */
    if(complete == false && req->req_frags == 1) {
        mca_pml_teg_send_request_schedule(req);
    }
}

