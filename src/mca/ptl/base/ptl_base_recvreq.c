/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "include/types.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "mca/ptl/base/ptl_base_recvreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"


static void mca_ptl_base_recv_request_construct(mca_ptl_base_recv_request_t*);
static void mca_ptl_base_recv_request_destruct(mca_ptl_base_recv_request_t*);
static mca_ptl_base_recv_frag_t* mca_ptl_base_recv_request_match_specific_proc(mca_ptl_base_recv_request_t*, int);


lam_class_t mca_ptl_base_recv_request_t_class = { 
    "mca_ptl_base_recv_request_t", 
    OBJ_CLASS(mca_pml_base_request_t),
    (lam_construct_t) mca_ptl_base_recv_request_construct, 
    (lam_destruct_t) mca_ptl_base_recv_request_destruct 
};
                                                                                                 

static void mca_ptl_base_recv_request_construct(mca_ptl_base_recv_request_t* request)
{
}


static void mca_ptl_base_recv_request_destruct(mca_ptl_base_recv_request_t* request)
{
}

                                                                                                                    
/*
 * This routine is used to match a posted receive when the source process 
 * is specified.
*/

void mca_ptl_base_recv_request_match_specific(mca_ptl_base_recv_request_t* request)
{
    lam_communicator_t *comm = request->super.req_comm;
    mca_pml_ptl_comm_t* pml_comm = comm->c_pml_comm;
    int req_peer = request->super.req_peer;
    mca_ptl_base_recv_frag_t* frag;
   
    /* check for a specific match */
    THREAD_LOCK(pml_comm->c_matching_lock+req_peer);
    if (lam_list_get_size(&pml_comm->c_unexpected_frags[req_peer]) > 0 &&
        (frag = mca_ptl_base_recv_request_match_specific_proc(request, req_peer)) != NULL) {
        mca_ptl_t* ptl = frag->super.frag_owner;
        THREAD_UNLOCK(pml_comm->c_matching_lock+req_peer);
        mca_ptl_base_recv_frag_init(frag);
        ptl->ptl_recv(ptl, frag);
        return; /* match found */
    }

    /* We didn't find any matches.  Record this irecv so we can match 
     * it when the message comes in.
    */
    lam_list_append(pml_comm->c_specific_receives+req_peer, (lam_list_item_t*)request);
    THREAD_UNLOCK(pml_comm->c_matching_lock+req_peer);
}


/*
 * this routine is used to try and match a wild posted receive - where
 * wild is determined by the value assigned to the source process
*/

void mca_ptl_base_recv_request_match_wild(mca_ptl_base_recv_request_t* request)
{
    lam_communicator_t *comm = request->super.req_comm;
    mca_pml_ptl_comm_t* pml_comm = comm->c_pml_comm;
    int proc_count = comm->c_remote_group->grp_proc_count;
    int proc;

    /*
     * Loop over all the outstanding messages to find one that matches.
     * There is an outer loop over lists of messages from each
     * process, then an inner loop over the messages from the
     * process.
    */
    for (proc = 0; proc < proc_count; proc++) {
        mca_ptl_base_recv_frag_t* frag;

        /* continue if no frags to match */
        THREAD_LOCK(pml_comm->c_matching_lock+proc);
        if (lam_list_get_size(&pml_comm->c_unexpected_frags[proc]) == 0) {
            THREAD_UNLOCK(pml_comm->c_matching_lock+proc);
            continue;
        }

        /* loop over messages from the current proc */
        if ((frag = mca_ptl_base_recv_request_match_specific_proc(request, proc)) != NULL) {
            mca_ptl_t* ptl = frag->super.frag_owner;
            THREAD_UNLOCK(pml_comm->c_matching_lock+proc);
            mca_ptl_base_recv_frag_init(frag);
            ptl->ptl_recv(ptl, frag);
            return; /* match found */
        }
        THREAD_UNLOCK(pml_comm->c_matching_lock+proc);
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

static mca_ptl_base_recv_frag_t* mca_ptl_base_recv_request_match_specific_proc(
    mca_ptl_base_recv_request_t* request, int proc)
{
    mca_pml_ptl_comm_t *pml_comm = request->super.req_comm->c_pml_comm;
    lam_list_t* unexpected_frags = pml_comm->c_unexpected_frags+proc;
    mca_ptl_base_recv_frag_t* frag;
    int tag = request->super.req_tag;

    for (frag =  (mca_ptl_base_recv_frag_t*)lam_list_get_first(unexpected_frags);
         frag != (mca_ptl_base_recv_frag_t*)lam_list_get_end(unexpected_frags);
         frag =  (mca_ptl_base_recv_frag_t*)lam_list_get_next(frag)) {
        mca_ptl_base_match_header_t* header = &frag->super.frag_header.hdr_match;

        /* check first frag - we assume that process matching has been done already */
        if (((tag == LAM_ANY_TAG) || (tag == header->hdr_tag))) {

            if (tag == LAM_ANY_TAG && header->hdr_tag < 0) {
                continue;
            }
            lam_list_remove_item(unexpected_frags, (lam_list_item_t*)frag);
            request->req_sequence = header->hdr_msg_seq;
            request->super.req_tag = tag = header->hdr_tag;
            request->super.req_peer = header->hdr_src;
            frag->frag_request = request;
            return frag;
        } 
    }
    return NULL;
}

