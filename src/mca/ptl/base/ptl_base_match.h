/*
 * $HEADER$
 */

#ifndef MCA_PTL_BASE_MATCH_H
#define MCA_PTL_BASE_MATCH_H

struct mca_ptl_base_recv_frag_t;


int mca_ptl_base_match(mca_ptl_base_match_header_t *frag_header,
    struct mca_ptl_base_recv_frag_t *frag_desc, bool *match_made,
    lam_list_t *additional_matches);

#endif /* MCA_PTL_BASE_MATCH_H */

