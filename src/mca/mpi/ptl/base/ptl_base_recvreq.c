/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "lam/types.h"
#include "mca/mpi/ptl/base/ptl_base_comm.h"
#include "mca/mpi/ptl/base/ptl_base_recvreq.h"
#include "mca/mpi/ptl/base/ptl_base_recvfrag.h"


static void mca_ptl_base_recv_request_init(mca_ptl_base_recv_request_t*);
static void mca_ptl_base_recv_request_destroy(mca_ptl_base_recv_request_t*);
static bool mca_ptl_base_recv_request_match_specific_proc(mca_ptl_base_recv_request_t*, int);


lam_class_info_t mca_ptl_base_recv_request_cls = { 
    "mca_ptl_base_recv_request_t", 
    &mca_pml_base_request_cls,
    (class_init_t) mca_ptl_base_recv_request_init, 
    (class_destroy_t) mca_ptl_base_recv_request_destroy 
};
                                                                                                 

static void mca_ptl_base_recv_request_init(mca_ptl_base_recv_request_t* request)
{
    SUPER_INIT(request, &mca_pml_base_request_cls);
}


static void mca_ptl_base_recv_request_destroy(mca_ptl_base_recv_request_t* request)
{
    SUPER_DESTROY(request, &mca_pml_base_request_cls);
}

                                                                                                                    
/*
 * This routine is used to match a posted receive when the source process 
 * is specified.
*/

void mca_ptl_base_recv_request_match_specific(mca_ptl_base_recv_request_t* request)
{
    lam_communicator_t *comm = request->super.req_communicator;
    mca_pml_comm_t* pml_comm = comm->c_pml_comm;
    int req_peer = request->super.req_peer;

    /* check for a specific match */
    if (lam_list_get_size(&pml_comm->c_unexpected_frags[req_peer]) > 0 &&
        mca_ptl_base_recv_request_match_specific_proc(request, req_peer)) {
        return; /* match found */
    }

    /* We didn't find any matches.  Record this irecv so we can match 
     * it when the message comes in.
    */

    THREAD_LOCK(pml_comm->c_matching_lock+req_peer);
    lam_list_append(pml_comm->c_specific_receives+req_peer, (lam_list_item_t*)request);
    THREAD_UNLOCK(pml_comm->c_matching_lock+req_peer);
}


/*
 * this routine is used to try and match a wild posted receive - where
 * wild is determined by the value assigned to the source process
*/

void mca_ptl_base_recv_request_match_wild(mca_ptl_base_recv_request_t* request)
{
    lam_communicator_t *comm = request->super.req_communicator;
    mca_pml_comm_t* pml_comm = comm->c_pml_comm;
    int proc_count = comm->c_remote_group->g_proc_count;
    int proc;

    /*
     * Loop over all the outstanding messages to find one that matches.
     * There is an outer loop over lists of messages from each
     * process, then an inner loop over the messages from the
     * process.
    */
    for (proc = 0; proc < proc_count; proc++) {
                                                                                                                  
        /* continue if no frags to match */
        if (lam_list_get_size(&pml_comm->c_unexpected_frags[proc]) == 0)
            continue;

        /* loop over messages from the current proc */
        if (mca_ptl_base_recv_request_match_specific_proc(request, proc)) {
            return; /* match found */
        }
    } 

    /* We didn't find any matches.  Record this irecv so we can match to
     * it when the message comes in.
    */
 
    THREAD_LOCK(&pml_comm->c_wild_lock);
    lam_list_append(&pml_comm->c_wild_receives, (lam_list_item_t*)request);
    THREAD_UNLOCK(&pml_comm->c_wild_lock);
}


/*
 *  this routine tries to match a posted receive.  If a match is found,
 *  it places the request in the appropriate matched receive list. 
*/

static bool mca_ptl_base_recv_request_match_specific_proc(mca_ptl_base_recv_request_t* request, int proc)
{
    mca_pml_comm_t *pml_comm = request->super.req_communicator->c_pml_comm;
    lam_list_t* unexpected_frags = pml_comm->c_unexpected_frags+proc;
    mca_ptl_base_recv_frag_t* frag;
    int tag = request->super.req_tag;

    /* lock for thread safety */
    THREAD_LOCK(pml_comm->c_matching_lock+proc);
    for (frag =  (mca_ptl_base_recv_frag_t*)lam_list_get_first(unexpected_frags);
         frag != (mca_ptl_base_recv_frag_t*)lam_list_get_end(unexpected_frags);
         frag =  (mca_ptl_base_recv_frag_t*)lam_list_get_next(frag)) {

        /* check first frag - we assume that process matching has been done already */
        if (((tag == LAM_ANY_TAG) || (tag == frag->super.frag_header.hdr_user_tag))) {

            mca_ptl_t* ptl = frag->super.frag_owner;
            if (tag == LAM_ANY_TAG && frag->super.frag_header.hdr_user_tag < 0) {
                continue;
            }

            frag->frag_request = request;
            request->req_sequence = frag->super.frag_header.hdr_msg_seq;
            request->super.req_tag = tag = frag->super.frag_header.hdr_user_tag;
            request->super.req_peer = frag->super.frag_header.hdr_src_rank;

            /* notify ptl fragment has been matched - send cts to peer */
            THREAD_UNLOCK(pml_comm->c_matching_lock+proc);
            ptl->ptl_cts(ptl, frag);
            return true;
        } 
    }

    THREAD_UNLOCK(pml_comm->c_matching_lock+proc);
    return false;
}


void mca_ptl_base_recv_request_progress(
    mca_ptl_base_recv_request_t* request, 
    mca_ptl_base_recv_frag_t* frag)
{

}

