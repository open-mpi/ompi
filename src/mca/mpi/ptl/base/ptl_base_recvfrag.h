/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef MCA_PML_BASE_RECVFRAG_H
#define MCA_PML_BASE_RECVFRAG_H

#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_fragment.h"
#include "mca/mpi/ptl/base/ptl_base_recvreq.h"
#include "mca/mpi/ptl/base/ptl_base_match.h"

extern lam_class_info_t mca_ptl_base_recv_frag_cls;


struct mca_ptl_base_recv_frag_t {
    mca_ptl_base_frag_t super;
    mca_ptl_base_recv_request_t *frag_request; /* matched posted receive */
    struct mca_ptl_base_peer_t* frag_peer; /* peer received from */
};
typedef struct mca_ptl_base_recv_frag_t mca_ptl_base_recv_frag_t;



static inline void mca_ptl_base_recv_frag_process(mca_ptl_base_recv_frag_t* frag)
{
    mca_ptl_base_recv_request_t* request = frag->frag_request;
    mca_ptl_base_frag_header_t* header = &frag->super.frag_header.hdr_frag;
    mca_ptl_t* ptl = frag->super.frag_owner;
                                                                                                                   
    /* determine the offset and size of posted buffer */
    if (request->super.req_length < header->hdr_frag_offset) {
                                                                                                                   
        /* user buffer is to small - discard entire fragment */
        frag->super.frag_addr = 0;
        frag->super.frag_size = 0;
                                                                                                                   
    } else if (request->super.req_length < header->hdr_frag_offset + header->hdr_frag_length) {
                                                                                                                   
        /* user buffer is to small - discard part of fragment */
        frag->super.frag_addr = ((unsigned char*)request->super.req_addr + header->hdr_frag_offset);
        frag->super.frag_size = request->super.req_length - header->hdr_frag_offset;
                                                                                                                   
    } else {
                                                                                                                   
        /* user buffer is large enough for this fragment */
        frag->super.frag_addr = ((unsigned char*)request->super.req_addr + header->hdr_frag_offset);
        frag->super.frag_size = header->hdr_frag_length;
                                                                                                                   
    }

    /* indicate to the ptl that the fragment can be delivered */
    ptl->ptl_recv(ptl, frag);
}
                                                                                                                   

static inline int mca_ptl_base_recv_frag_match(mca_ptl_base_recv_frag_t* frag, mca_ptl_base_match_header_t* header)
{
    bool matched;
    lam_list_t matched_frags;
    int rc = mca_ptl_base_match(header, frag, &matched, &matched_frags);
    if(rc != LAM_SUCCESS)
        return rc;
                                                                                                                   
    if(matched) {
        do {
            /* process current fragment */
            mca_ptl_base_recv_frag_process(frag);
                                                                                                                   
            /* process any additional fragments that arrived out of order */
            frag = (mca_ptl_base_recv_frag_t*)lam_list_remove_first(&matched_frags);
        } while(NULL != frag);
    }
    return LAM_SUCCESS;
}


#endif

