/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PML_OB1_MATCH_H
#define MCA_PML_OB1_MATCH_H
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_pml_ob1_recv_frag_t;


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
OMPI_DECLSPEC int mca_pml_ob1_match(
    mca_bmi_base_module_t* bmi,
    mca_pml_ob1_match_hdr_t *hdr,
    mca_bmi_base_segment_t* segments,
    size_t num_segments);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PML_OB1_MATCH_H */

