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
 *  @file
 */
                                                                                                                                                 
#ifndef MCA_PML_OB1_RECVFRAG_H
#define MCA_PML_OB1_RECVFRAG_H

#include "mca/bmi/bmi.h"
#include "pml_ob1_hdr.h"

struct mca_pml_ob1_recv_frag_t {
    ompi_list_item_t super;
    mca_bmi_base_module_t* bmi;
    mca_pml_ob1_hdr_t hdr;
    mca_bmi_base_segment_t* segments;
    size_t num_segments;
    struct mca_pml_ob1_recv_request_t* request;
};
typedef struct mca_pml_ob1_recv_frag_t mca_pml_ob1_recv_frag_t;


#define MCA_PML_OB1_RECV_FRAG_ALLOC(frag,rc)  \
{ \
 \
} 

#define MCA_PML_OB1_RECV_FRAG_INIT(frag,bmi,hdr,segs,cnt)  \
{ \
 \
} 

#define MCA_PML_OB1_RECV_FRAG_RETURN(frag)  \
{ \
 \
} 


/**
 *  Callback from BMI on receipt of a fragment.
 */

OMPI_DECLSPEC void mca_pml_ob1_recv_frag_callback(
    mca_bmi_base_module_t* bmi,
    mca_bmi_base_tag_t tag,
    mca_bmi_base_descriptor_t* descriptor,
    void* cbdata
);
                                                                                                               
/**
 * Match incoming fragments against posted receives.  
 * Supports out of order delivery.
 * 
 * @param frag_header (IN)          Header of received fragment.
 * @param frag_desc (IN)            Received fragment descriptor.
 * @param match_made (OUT)          Flag indicating wether a match was made.
 * @param additional_matches (OUT)  List of additional matches 
 * @return                          OMPI_SUCCESS or error status on failure.
 */
OMPI_DECLSPEC int mca_pml_ob1_recv_frag_match(
    mca_bmi_base_module_t* bmi,
    mca_pml_ob1_match_hdr_t *hdr,
    mca_bmi_base_segment_t* segments,
    size_t num_segments);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

