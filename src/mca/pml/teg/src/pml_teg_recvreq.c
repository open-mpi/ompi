#include "mca/ptl/base/ptl_base_comm.h"
#include "pml_teg_recvreq.h"

                                                                                                               
/*
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application. 
 */

void mca_pml_teg_recv_request_progress(
    struct mca_ptl_t* ptl,
    mca_ptl_base_recv_request_t* req,
    mca_ptl_base_recv_frag_t* frag)
{
    THREAD_LOCK(&mca_pml_teg.teg_request_lock);
    req->req_bytes_delivered += frag->super.frag_size;
    req->req_bytes_received += frag->super.frag_header.hdr_frag.hdr_frag_length;
    if (req->req_bytes_received >= req->req_bytes_packed) {
        /* initialize request status */
        req->super.req_status.MPI_SOURCE = req->super.req_peer;
        req->super.req_status.MPI_TAG = req->super.req_tag;
        req->super.req_status.MPI_ERROR = OMPI_SUCCESS;
        req->super.req_status._count = req->req_bytes_delivered;
        req->super.req_pml_done = true; 
        req->super.req_mpi_done = true;
        if(mca_pml_teg.teg_request_waiting) {
#if MCA_PML_TEG_STATISTICS
            mca_pml_teg.teg_condition_broadcasts++;
#endif
            ompi_condition_broadcast(&mca_pml_teg.teg_request_cond);
        }
    }
    THREAD_UNLOCK(&mca_pml_teg.teg_request_lock);
}

