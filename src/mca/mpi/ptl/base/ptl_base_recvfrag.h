/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef MCA_PML_BASE_RECVFRAG_H
#define MCA_PML_BASE_RECVFRAG_H

#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_fragment.h"
#include "mca/mpi/ptl/base/ptl_base_recvreq.h"

extern lam_class_info_t mca_ptl_base_recv_frag_cls;


typedef struct {
    mca_ptl_base_frag_t super;
    /* matched receve request corresponding to this fragment */
    mca_ptl_base_recv_request_t *matched_recv;
} mca_ptl_base_recv_frag_t;


void mca_ptl_base_recv_frag_init(mca_ptl_base_recv_frag_t*);
void mca_ptl_base_recv_frag_destroy(mca_ptl_base_recv_frag_t*);


#endif

