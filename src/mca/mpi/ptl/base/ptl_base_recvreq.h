/*
 * $HEADER$
 */

#ifndef MCA_PML_BASE_RECV_REQUEST_H
#define MCA_PML_BASE_RECV_REQUEST_H

#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/pml/base/pml_base_request.h"

extern lam_class_info_t mca_ptl_base_recv_request_cls;;


typedef struct {
   mca_pml_base_request_t super;
   mca_ptl_base_sequence_t req_sequence;
} mca_ptl_base_recv_request_t;


void mca_ptl_base_recv_request_init(mca_ptl_base_recv_request_t*);
void mca_ptl_base_recv_request_destroy(mca_ptl_base_recv_request_t*);


#endif

