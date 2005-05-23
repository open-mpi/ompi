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
 * Called to attempt a match for new fragments.
 *
 * @param bmi (IN)    The PTL pointer
 * @param frag (IN)   Receive fragment descriptor.
 * @param hdr (IN)    Header corresponding to the receive fragment.
 * @return            OMPI_SUCCESS or error status on failure.
 */
bool mca_pml_ob1_recv_frag_match( 
    struct mca_pml_ob1_recv_frag_t* frag
);

int mca_pml_ob1_recv_frag_matched(
    struct mca_pml_ob1_recv_frag_t* frag
);

int mca_pml_ob1_recv_frag_complete(
    struct mca_bmi_base_module_t* bmi,
    struct mca_pml_ob1_recv_request_t* req,
    struct mca_pml_ob1_recv_frag_t* frag
);

#endif

