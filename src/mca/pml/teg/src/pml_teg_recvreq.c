#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "pml_teg_recvreq.h"

                                                                                                               
static mca_ptl_base_recv_frag_t* mca_pml_teg_recv_request_match_specific_proc(
    mca_pml_base_recv_request_t* request, int proc);

/*
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application. 
 */

void mca_pml_teg_recv_request_progress(
    struct mca_ptl_t* ptl,
    mca_pml_base_recv_request_t* req,
    size_t bytes_received,
    size_t bytes_delivered)
{
    OMPI_THREAD_LOCK(&mca_pml_teg.teg_request_lock);
    req->req_bytes_received += bytes_received;
    req->req_bytes_delivered += bytes_delivered;
    if (req->req_bytes_received >= req->req_bytes_packed) {
        /* initialize request status */
        req->req_base.req_status.MPI_SOURCE = req->req_base.req_peer;
        req->req_base.req_status.MPI_TAG = req->req_base.req_tag;
        req->req_base.req_status.MPI_ERROR = OMPI_SUCCESS;
        req->req_base.req_status._count = req->req_bytes_delivered;
        req->req_base.req_pml_done = true; 
        req->req_base.req_mpi_done = true;
        if(mca_pml_teg.teg_request_waiting) {
#if MCA_PML_TEG_STATISTICS
            mca_pml_teg.teg_condition_broadcasts++;
#endif
            ompi_condition_broadcast(&mca_pml_teg.teg_request_cond);
        }
    }
    OMPI_THREAD_UNLOCK(&mca_pml_teg.teg_request_lock);
}


                                                                                                                    
/*
 * This routine is used to match a posted receive when the source process 
 * is specified.
*/

void mca_pml_teg_recv_request_match_specific(mca_pml_base_recv_request_t* request)
{
    ompi_communicator_t *comm = request->req_base.req_comm;
    mca_pml_ptl_comm_t* pml_comm = comm->c_pml_comm;
    int req_peer = request->req_base.req_peer;
    mca_ptl_base_recv_frag_t* frag;
   
    /* check for a specific match */
    OMPI_THREAD_LOCK(&pml_comm->c_matching_lock);

    /* assign sequence number */
    request->req_base.req_sequence = pml_comm->c_recv_seq++;

    if (ompi_list_get_size(&pml_comm->c_unexpected_frags[req_peer]) > 0 &&
        (frag = mca_pml_teg_recv_request_match_specific_proc(request, req_peer)) != NULL) {
        mca_ptl_t* ptl = frag->frag_base.frag_owner;
        OMPI_THREAD_UNLOCK(&pml_comm->c_matching_lock);
        ptl->ptl_matched(ptl, frag);
        return; /* match found */
    }

    /* We didn't find any matches.  Record this irecv so we can match 
     * it when the message comes in.
    */
    if(request->req_base.req_type != MCA_PML_REQUEST_IPROBE)
        ompi_list_append(pml_comm->c_specific_receives+req_peer, (ompi_list_item_t*)request);
    OMPI_THREAD_UNLOCK(&pml_comm->c_matching_lock);
}


/*
 * this routine is used to try and match a wild posted receive - where
 * wild is determined by the value assigned to the source process
*/

void mca_pml_teg_recv_request_match_wild(mca_pml_base_recv_request_t* request)
{
    ompi_communicator_t *comm = request->req_base.req_comm;
    mca_pml_ptl_comm_t* pml_comm = comm->c_pml_comm;
    int proc_count = comm->c_remote_group->grp_proc_count;
    int proc;

    /*
     * Loop over all the outstanding messages to find one that matches.
     * There is an outer loop over lists of messages from each
     * process, then an inner loop over the messages from the
     * process.
    */
    OMPI_THREAD_LOCK(&pml_comm->c_matching_lock);

    /* assign sequence number */
    request->req_base.req_sequence = pml_comm->c_recv_seq++;

    for (proc = 0; proc < proc_count; proc++) {
        mca_ptl_base_recv_frag_t* frag;

        /* continue if no frags to match */
        if (ompi_list_get_size(&pml_comm->c_unexpected_frags[proc]) == 0)
            continue;

        /* loop over messages from the current proc */
        if ((frag = mca_pml_teg_recv_request_match_specific_proc(request, proc)) != NULL) {
            mca_ptl_t* ptl = frag->frag_base.frag_owner;
            OMPI_THREAD_UNLOCK(&pml_comm->c_matching_lock);
            ptl->ptl_matched(ptl, frag);
            return; /* match found */
        }
    } 

    /* We didn't find any matches.  Record this irecv so we can match to
     * it when the message comes in.
    */
 
    if(request->req_base.req_type != MCA_PML_REQUEST_IPROBE)
        ompi_list_append(&pml_comm->c_wild_receives, (ompi_list_item_t*)request);
    OMPI_THREAD_UNLOCK(&pml_comm->c_matching_lock);
}


/*
 *  this routine tries to match a posted receive.  If a match is found,
 *  it places the request in the appropriate matched receive list. 
*/

static mca_ptl_base_recv_frag_t* mca_pml_teg_recv_request_match_specific_proc(
    mca_pml_base_recv_request_t* request, int proc)
{
    mca_pml_ptl_comm_t *pml_comm = request->req_base.req_comm->c_pml_comm;
    ompi_list_t* unexpected_frags = pml_comm->c_unexpected_frags+proc;
    mca_ptl_base_recv_frag_t* frag;
    int tag = request->req_base.req_tag;

    for (frag =  (mca_ptl_base_recv_frag_t*)ompi_list_get_first(unexpected_frags);
         frag != (mca_ptl_base_recv_frag_t*)ompi_list_get_end(unexpected_frags);
         frag =  (mca_ptl_base_recv_frag_t*)ompi_list_get_next(frag)) {
        mca_ptl_base_match_header_t* header = &frag->frag_base.frag_header.hdr_match;

        /* check first frag - we assume that process matching has been done already */
        if (((tag == OMPI_ANY_TAG) || (tag == header->hdr_tag))) {

            if (tag == OMPI_ANY_TAG && header->hdr_tag < 0) {
                continue;
            }
            ompi_list_remove_item(unexpected_frags, (ompi_list_item_t*)frag);
            request->req_bytes_packed = header->hdr_msg_length;
            request->req_base.req_tag = header->hdr_tag;
            request->req_base.req_peer = header->hdr_src;
            frag->frag_request = request;
            return frag;
        } 
    }
    return NULL;
}

