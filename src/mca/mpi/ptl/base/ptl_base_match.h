/*
 * $HEADER$
 */

#ifndef MCA_PTL_BASE_MATCH_H
#define MCA_PTL_BASE_MATCH_H

int mca_ptl_base_match(mca_ptl_base_header_t *frag_header,
    mca_ptl_base_recv_frag_t *frag_desc, bool *match_made,
    lam_list_t *additional_matches);

#endif /* MCA_PTL_BASE_MATCH_H */

