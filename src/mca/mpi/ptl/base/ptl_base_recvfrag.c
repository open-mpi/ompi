/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_recvfrag.h"
#include "mca/mpi/ptl/base/ptl_base_match.h"


lam_class_info_t mca_ptl_base_recv_frag_cls = { 
    "mca_ptl_base_recv_frag_t", 
    &mca_ptl_base_frag_cls,
    (class_init_t) mca_ptl_base_recv_frag_init, 
    (class_destroy_t) mca_ptl_base_recv_frag_destroy 
};


void mca_ptl_base_recv_frag_init(mca_ptl_base_recv_frag_t* frag)
{
    SUPER_INIT(frag, &mca_ptl_base_frag_cls);
}

void mca_ptl_base_recv_frag_destroy(mca_ptl_base_recv_frag_t* frag)
{
    SUPER_DESTROY(frag, &mca_ptl_base_frag_cls);
}

int mca_ptl_base_recv_frag_match(mca_ptl_base_recv_frag_t* frag, mca_ptl_base_header_t* header)
{
    lam_list_t matched_frags;
    bool matched;
    int rc = mca_ptl_base_match(header, frag, &matched, &matched_frags);
    if(rc != LAM_SUCCESS)
        return rc;

    if(matched) {
        do {
            mca_ptl_base_recv_request_t* request = frag->frag_request;
            mca_ptl_t* ptl = frag->super.frag_owner;

            /* determine the offset and size of posted buffer */
            if (request->super.req_length < frag->super.frag_header.hdr_msg_offset) {

                /* user buffer is to small - discard entire fragment */
                frag->super.frag_addr = 0;
                frag->super.frag_size = 0;

            } else if (request->super.req_length < frag->super.frag_header.hdr_msg_offset +
                frag->super.frag_header.hdr_frag_length) {
    
                /* user buffer is to small - discard part of fragment */
                frag->super.frag_addr = ((unsigned char*)request->super.req_addr +  
                    frag->super.frag_header.hdr_msg_offset);
                frag->super.frag_size = request->super.req_length - frag->super.frag_header.hdr_msg_offset;

            } else {

                /* user buffer is large enough for this fragment */
                frag->super.frag_addr = ((unsigned char*)request->super.req_addr +  
                    frag->super.frag_header.hdr_msg_offset);
                frag->super.frag_size = frag->super.frag_header.hdr_frag_length;

            }

            /* send cts acknowledgment back to peer */
            ptl->ptl_cts(ptl, frag);
 
            /* process any fragments that arrived out of order */
            frag = (mca_ptl_base_recv_frag_t*)lam_list_remove_first(&matched_frags);
        } while(NULL != frag);
    }
    return LAM_SUCCESS;
}


