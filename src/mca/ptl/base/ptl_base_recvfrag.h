/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef MCA_PTL_BASE_RECVFRAG_H
#define MCA_PTL_BASE_RECVFRAG_H

#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_fragment.h"
#include "mca/mpi/ptl/base/ptl_base_recvreq.h"
#include "mca/mpi/ptl/base/ptl_base_match.h"

extern lam_class_t mca_ptl_base_recv_frag_t_class;


struct mca_ptl_base_recv_frag_t {
    mca_ptl_base_frag_t super;
    mca_ptl_base_recv_request_t *frag_request; /* matched posted receive */
};
typedef struct mca_ptl_base_recv_frag_t mca_ptl_base_recv_frag_t;



static inline void mca_ptl_base_recv_frag_init(mca_ptl_base_recv_frag_t* frag)
{
    mca_ptl_base_recv_request_t* request = frag->frag_request;
    mca_ptl_base_frag_header_t* header = &frag->super.frag_header.hdr_frag;

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
}
                                                                                                                   

static inline int mca_ptl_base_recv_frag_match(
    mca_ptl_base_recv_frag_t* frag, 
    mca_ptl_base_match_header_t* header)
{
    bool matched;
    lam_list_t matched_frags;
    int rc;

    OBJ_CONSTRUCT(&matched_frags, lam_list_t);
    if((rc = mca_ptl_base_match(header, frag, &matched, &matched_frags)) != LAM_SUCCESS)
        return rc;

    if(matched) {
        do {
            mca_ptl_t* ptl = frag->super.frag_owner;

            /* initialize current fragment */
            mca_ptl_base_recv_frag_init(frag);
            
            /* notify ptl of match */
            ptl->ptl_recv(ptl, frag, &frag->frag_request->super.req_status);

            /* process any additional fragments that arrived out of order */
            frag = (mca_ptl_base_recv_frag_t*)lam_list_remove_first(&matched_frags);
        } while(NULL != frag);
    }
    return LAM_SUCCESS;
}


#endif

