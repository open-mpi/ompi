/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_BASE_MATCH_H
#define MCA_PTL_BASE_MATCH_H

struct mca_ptl_base_recv_frag_t;

/**
 * Match incoming fragments against posted receives.
 * 
 * @param frag_header (IN)          Header of received fragment.
 * @param frag_desc (IN)            Received fragment descriptor.
 * @param match_made (OUT)          Flag indicating wether a match was made.
 * @param additional_matches (OUT)  List of additional matches 
 * @return                          OMPI_SUCCESS or error status on failure.
 */
bool mca_ptl_base_match(mca_ptl_base_match_header_t *frag_header,
    struct mca_ptl_base_recv_frag_t *frag_desc, ompi_list_t *additional_matches);

#endif /* MCA_PTL_BASE_MATCH_H */

