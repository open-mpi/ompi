/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_BASE_RECVFRAG_H
#define MCA_PTL_BASE_RECVFRAG_H

#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_fragment.h"
#include "mca/ptl/base/ptl_base_recvreq.h"
#include "mca/ptl/base/ptl_base_match.h"

extern lam_class_t mca_ptl_base_recv_frag_t_class;

/**
 * Base type for receive fragment descriptors.
 */
struct mca_ptl_base_recv_frag_t {
    mca_ptl_base_frag_t super; /**< base fragment descriptor */
    mca_ptl_base_recv_request_t *frag_request; /**< matched posted receive */
    bool frag_is_buffered; /**< does fragment need to be unpacked into users buffer */
};
typedef struct mca_ptl_base_recv_frag_t mca_ptl_base_recv_frag_t;

                                                                                                                   
/**
 * Called by the PTL to match attempt a match for new fragments.
 * 
 * @param frag (IN)     Receive fragment descriptor.
 * @param header (IN)   Header corresponding to the receive fragment.
 * @return              LAM_SUCCESS or error status on failure.
 */
static inline bool mca_ptl_base_recv_frag_match(
    mca_ptl_base_recv_frag_t* frag, 
    mca_ptl_base_match_header_t* header)
{
    bool matched;
    lam_list_t matched_frags;
    OBJ_CONSTRUCT(&matched_frags, lam_list_t);
    if((matched = mca_ptl_base_match(header, frag, &matched_frags)) == false)
        frag = (mca_ptl_base_recv_frag_t*)lam_list_remove_first(&matched_frags);

    while(NULL != frag) {
        mca_ptl_t* ptl = frag->super.frag_owner;
        mca_ptl_base_recv_request_t *request = frag->frag_request;
        mca_ptl_base_match_header_t *header = &frag->super.frag_header.hdr_match;

        /*
         * Initialize request status.
         */
        request->req_bytes_msg = header->hdr_msg_length;
        request->super.req_peer = header->hdr_src;
        request->super.req_tag = header->hdr_tag;

        /* notify ptl of match */
        ptl->ptl_recv(ptl, frag);

        /* process any additional fragments that arrived out of order */
        frag = (mca_ptl_base_recv_frag_t*)lam_list_remove_first(&matched_frags);
    };
    return matched;
}


#endif

