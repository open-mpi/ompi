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


struct mca_ptl_base_recv_frag_t {
    mca_ptl_base_frag_t super;
    mca_ptl_base_recv_request_t *frag_request; /* matched posted receive */
    struct mca_ptl_peer_t* frag_peer; /* peer received from */
};
typedef struct mca_ptl_base_recv_frag_t mca_ptl_base_recv_frag_t;


void mca_ptl_base_recv_frag_init(mca_ptl_base_recv_frag_t*);
void mca_ptl_base_recv_frag_destroy(mca_ptl_base_recv_frag_t*);
int  mca_ptl_base_recv_frag_match(mca_ptl_base_recv_frag_t*, mca_ptl_base_header_t*);


#endif

