/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_BASE_MATCH_H
#define MCA_PTL_BASE_MATCH_H
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_ptl_base_recv_frag_t;

/**
 * RCS/CTS receive side matching
 * Match incoming fragments against posted receives.  Out of order
 * delivery.
 * 
 * @param frag_header (IN)          Header of received fragment.
 * @param frag_desc (IN)            Received fragment descriptor.
 * @param match_made (OUT)          Flag indicating wether a match was made.
 * @param additional_matches (OUT)  List of additional matches 
 * @return                          OMPI_SUCCESS or error status on failure.
 */
OMPI_DECLSPEC bool mca_ptl_base_match(
    mca_ptl_base_match_header_t *frag_header,
    struct mca_ptl_base_recv_frag_t *frag_desc, 
    ompi_list_t *additional_matches, 
    bool* additional_matched);

/**
 * RCS/CTS receive side matching
 *
 * @param frag_header list of parameters needed for matching
 *                    This list is also embeded in frag_desc,
 *                    but this allows to save a memory copy when
 *                    a match is made in this routine. (IN)
 * @param frag_desc   pointer to receive fragment which we want
 *                    to match (IN/OUT).  If a match is not made,
 *                    frag_header is copied to frag_desc.
 * @param match_made  parameter indicating if we matched frag_desc/
 *                    frag_header (OUT)
 * @return indication if match was made or not.
 *
 * This routine is used to try and match a newly arrived message fragment
 *   to pre-posted receives.  The following assumptions are made
 *   - fragments are received in order, so no explicit sequence
 *     tracking is needed.
 *   - for long messages, e.g. more than one fragment, a RTS/CTS algorithm
 *       is used.
 *   - 2nd and greater fragments include a receive descriptor pointer
 *   - this routine may be called simoultaneously by more than one thread
 */
OMPI_DECLSPEC bool mca_ptl_base_match_in_order_network_delivery(
        mca_ptl_base_match_header_t *frag_header,
        struct mca_ptl_base_recv_frag_t *frag_desc);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PTL_BASE_MATCH_H */

