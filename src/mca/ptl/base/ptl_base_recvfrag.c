/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_recvfrag.h"
#include "mca/mpi/ptl/base/ptl_base_match.h"

static void mca_ptl_base_recv_frag_construct(mca_ptl_base_recv_frag_t* frag);
static void mca_ptl_base_recv_frag_destruct(mca_ptl_base_recv_frag_t* frag);


lam_class_t mca_ptl_base_recv_frag_t_class = { 
    "mca_ptl_base_recv_frag_t", 
    OBJ_CLASS(mca_ptl_base_frag_t),
    (lam_construct_t) mca_ptl_base_recv_frag_construct, 
    (lam_destruct_t) mca_ptl_base_recv_frag_destruct 
};


void mca_ptl_base_recv_frag_construct(mca_ptl_base_recv_frag_t* frag)
{
}

void mca_ptl_base_recv_frag_destruct(mca_ptl_base_recv_frag_t* frag)
{
}

