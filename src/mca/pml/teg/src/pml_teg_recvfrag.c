/*
 *
 */
/**
 * @file
 */
#include "pml_teg_recvfrag.h"
#include "pml_teg_proc.h"


extern ompi_class_t mca_ptl_base_recv_frag_t_class;


/**
 * Called by the PTL to match attempt a match for new fragments.
 * 
 * @param ptl (IN)      The PTL pointer
 * @param frag (IN)     Receive fragment descriptor.
 * @param header (IN)   Header corresponding to the receive fragment.
 * @return              OMPI_SUCCESS or error status on failure.
 */
bool mca_pml_teg_recv_frag_match(
    mca_ptl_base_module_t* ptl,
    mca_ptl_base_recv_frag_t* frag, 
    mca_ptl_base_match_header_t* header)
{
    bool matched;
    ompi_list_t matched_frags;
    OBJ_CONSTRUCT(&matched_frags, ompi_list_t);
    if((matched = mca_ptl_base_match(header, frag, &matched_frags)) == false) {
        frag = (mca_ptl_base_recv_frag_t*)ompi_list_remove_first(&matched_frags);
    }

    while(NULL != frag) {
        mca_ptl_base_module_t* ptl = frag->frag_base.frag_owner;
        mca_pml_base_recv_request_t *request = frag->frag_request;
        mca_ptl_base_match_header_t *header = &frag->frag_base.frag_header.hdr_match;

        /*
         * Initialize request status.
         */
        request->req_bytes_packed = header->hdr_msg_length;
        request->req_base.req_peer = header->hdr_src;
        request->req_base.req_tag = header->hdr_tag;

        /*
         * If probe - signal request is complete - but don't notify PTL
         */
        if(request->req_base.req_type == MCA_PML_REQUEST_PROBE) {

             ptl->ptl_recv_progress(
                ptl, 
                request, 
                frag->frag_base.frag_header.hdr_frag.hdr_frag_length,
                frag->frag_base.frag_size);
             matched = mca_pml_teg_recv_frag_match( ptl, frag, header );

        } else {

            /* if required - setup pointer to ptls peer */
            if (NULL == frag->frag_base.frag_peer) {
                frag->frag_base.frag_peer = mca_pml_teg_proc_lookup_remote_peer(request->req_base.req_comm,header->hdr_src,ptl);
            }

            /* notify ptl of match */
            ptl->ptl_matched(ptl, frag);

            /* process any additional fragments that arrived out of order */
            frag = (mca_ptl_base_recv_frag_t*)ompi_list_remove_first(&matched_frags);
        };
    };
    return matched;
}


