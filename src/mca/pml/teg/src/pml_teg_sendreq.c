/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "include/constants.h"
#include "mca/ptl/ptl.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_teg_sendreq.h"


/**
 *  Schedule message delivery across potentially multiple PTLs. 
 *
 *  @param request (IN)    Request to schedule
 *  @return status         Error status
 *
 */

void mca_pml_teg_send_request_schedule(mca_pml_base_send_request_t* req)
{
    ompi_proc_t *proc = ompi_comm_peer_lookup(req->req_base.req_comm, req->req_base.req_peer);
    mca_pml_proc_t* proc_pml = proc->proc_pml;

    /* allocate remaining bytes to PTLs */
    size_t bytes_remaining = req->req_bytes_packed - req->req_offset;
    size_t num_ptl_avail = proc_pml->proc_ptl_next.ptl_size;
    size_t num_ptl = 0;
    while(bytes_remaining > 0 && num_ptl++ < num_ptl_avail) {
        mca_ptl_proc_t* ptl_proc = mca_ptl_array_get_next(&proc_pml->proc_ptl_next);
        mca_ptl_t* ptl = ptl_proc->ptl;
        int rc;

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
            bytes_to_frag = (ptl_proc->ptl_weight * bytes_remaining) / 100;
            if(ptl->ptl_max_frag_size != 0 && bytes_to_frag > ptl->ptl_max_frag_size)
                bytes_to_frag = ptl->ptl_max_frag_size;
        }

        rc = ptl->ptl_put(ptl, ptl_proc->ptl_peer, req, req->req_offset, bytes_to_frag, 0);
        if(rc == OMPI_SUCCESS) {
            bytes_remaining = req->req_bytes_packed - req->req_offset;
        }
    }

    /* unable to complete send - signal request failed */
    if(bytes_remaining > 0) {
        OMPI_THREAD_LOCK(&mca_pml_teg.teg_request_lock);
        req->req_base.req_mpi_done = true;
        /* FIX - set status correctly */
        if(mca_pml_teg.teg_request_waiting)
            ompi_condition_broadcast(&mca_pml_teg.teg_request_cond);
        OMPI_THREAD_UNLOCK(&mca_pml_teg.teg_request_lock);
    }
}


/**
 *  Update the status of the send request to reflect the number of bytes
 *  "actually" sent (and acknowledged). This should be called by the
 *  lower layer PTL after the fragment is actually delivered and has been
 *  acknowledged (if required). Note that this routine should NOT be called
 *  directly by the PTL, a function pointer is setup on the PTL at init to 
 *  enable upcalls into the PML w/out directly linking to a specific PML 
 * implementation.
 */

void mca_pml_teg_send_request_progress(
    struct mca_ptl_t* ptl,
    mca_pml_base_send_request_t* req,
    size_t bytes_sent)
{
    bool first_frag;
    OMPI_THREAD_LOCK(&mca_pml_teg.teg_request_lock);
    first_frag = (req->req_bytes_sent == 0 && req->req_bytes_packed > 0);
    req->req_bytes_sent += bytes_sent;
    if (req->req_bytes_sent >= req->req_bytes_packed) {
        req->req_base.req_pml_done = true;
        if (req->req_base.req_mpi_done == false) {
            req->req_base.req_status.MPI_SOURCE = req->req_base.req_comm->c_my_rank;
            req->req_base.req_status.MPI_TAG = req->req_base.req_tag;
            req->req_base.req_status.MPI_ERROR = OMPI_SUCCESS;
            req->req_base.req_status._count = req->req_bytes_sent;
            req->req_base.req_mpi_done = true;
            if(mca_pml_teg.teg_request_waiting) {
                ompi_condition_broadcast(&mca_pml_teg.teg_request_cond);
            }
        } else if (req->req_base.req_free_called) {
            MCA_PML_TEG_FREE((ompi_request_t**)&req);
        }
        OMPI_THREAD_UNLOCK(&mca_pml_teg.teg_request_lock);
        return;
    } 
    OMPI_THREAD_UNLOCK(&mca_pml_teg.teg_request_lock);

    /* if first fragment - shedule remaining fragments */
    if(first_frag == true) {
        mca_pml_teg_send_request_schedule(req);
    }
}

