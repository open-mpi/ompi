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
                                                                                                                                                 
#ifndef MCA_PML_OB1_RDMAFRAG_H
#define MCA_PML_OB1_RDMAFRAG_H

#include "mca/bmi/bmi.h"
#include "pml_ob1_hdr.h"

typedef enum {
    MCA_PML_OB1_RMDA_INIT,
    MCA_PML_OB1_RDMA_PREPARE,
    MCA_PML_OB1_RDMA_PUT,
    MCA_PML_OB1_RDMA_FIN
} mca_pml_ob1_rdma_state_t;

struct mca_pml_ob1_rdma_frag_t {
    ompi_list_item_t super;
    mca_bmi_base_module_t* rdma_bmi;
    mca_pml_ob1_hdr_t rdma_hdr;
    mca_pml_ob1_rdma_state_t rdma_state;
    size_t rdma_length;
    mca_bmi_base_segment_t rdma_segs[MCA_BMI_DES_MAX_SEGMENTS];
    struct mca_pml_ob1_send_request_t* rdma_req;
};
typedef struct mca_pml_ob1_rdma_frag_t mca_pml_ob1_rdma_frag_t;

OBJ_CLASS_DECLARATION(mca_pml_ob1_rdma_frag_t);


#define MCA_PML_OB1_RDMA_FRAG_ALLOC(frag,rc)                    \
do {                                                            \
    ompi_list_item_t* item;                                     \
    OMPI_FREE_LIST_WAIT(&mca_pml_ob1.rdma_frags, item, rc);     \
    frag = (mca_pml_ob1_rdma_frag_t*)item;                      \
} while(0)

#define MCA_PML_OB1_RDMA_FRAG_RETURN(frag)                      \
do {                                                            \
    /* return fragment */                                       \
    OMPI_FREE_LIST_RETURN(&mca_pml_ob1.rdma_frags,              \
        (ompi_list_item_t*)frag);                               \
} while(0)


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

