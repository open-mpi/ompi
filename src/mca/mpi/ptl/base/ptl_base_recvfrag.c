/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "mca/mpi/ptl/base/ptl_base_recvfrag.h"

lam_class_info_t mca_ptl_base_recv_frag_cls = { 
    "mca_ptl_base_recv_frag_t", 
    &mca_ptl_base_frag_cls,
    (class_init_t) mca_ptl_base_recv_frag_init, 
    (class_destroy_t) mca_ptl_base_recv_frag_destroy 
};


void mca_ptl_base_recv_frag_init(mca_ptl_base_recv_frag_t* frag)
{
    SUPER_INIT(frag, &mca_ptl_base_frag_cls);
}

void mca_ptl_base_recv_frag_destroy(mca_ptl_base_recv_frag_t* frag)
{
    SUPER_DESTROY(frag, &mca_ptl_base_frag_cls);
}

