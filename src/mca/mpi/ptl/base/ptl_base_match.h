/*
 * $HEADER$
 */

#ifndef MCA_PTL_BASE_MATCH_H
#define MCA_PTL_BASE_MATCH_H

int mca_ptl_base_match(mca_ptl_base_reliable_hdr_t *frag_header,
        mca_ptl_base_recv_frag_t *frag_desc, int *match_made,
        lam_list_t *additional_matches);

mca_ptl_base_recv_request_t *mca_ptl_base_check_recieves_for_match(
        mca_ptl_base_reliable_hdr_t *frag_header,
        mca_pml_comm_t *ptl_comm);

mca_ptl_base_recv_request_t *check_wild_receives_for_match(
        mca_ptl_base_reliable_hdr_t *frag_header, 
        mca_pml_comm_t *ptl_comm);

mca_ptl_base_recv_request_t *check_specific_receives_for_match(
        mca_ptl_base_reliable_hdr_t *frag_header, 
        mca_pml_comm_t *ptl_comm);

mca_ptl_base_recv_request_t *check_specific_and_wild_receives_for_match(
        mca_ptl_base_reliable_hdr_t *frag_header, 
        mca_pml_comm_t *ptl_comm);

void lam_check_cantmatch_for_match(lam_list_t *additional_matches,
                mca_pml_comm_t *pml_comm, int frag_src);

#endif /* MCA_PTL_BASE_MATCH_H */

