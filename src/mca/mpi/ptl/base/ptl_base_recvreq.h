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

static inline void mca_ptl_base_recv_request_reinit(
    mca_ptl_base_recv_request_t *request,
    void *addr,
    size_t length,
    lam_datatype_t* datatype,
    int src,
    int tag,
    lam_communicator_t* comm,
    bool persistent)
{
    request->req_sequence = 0;
    request->super.req_addr = addr;
    request->super.req_length = length;
    request->super.req_datatype = datatype;
    request->super.req_peer = src;
    request->super.req_tag = tag;
    request->super.req_communicator = comm;
    request->super.req_type = MCA_PML_REQUEST_RECV;
    request->super.req_persistent = persistent;
    request->super.req_mpi_done = false;
    request->super.req_pml_layer_done = false;
}

#endif

