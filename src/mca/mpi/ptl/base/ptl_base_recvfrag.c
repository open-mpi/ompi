/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_recvfrag.h"
#include "mca/mpi/ptl/base/ptl_base_match.h"

static void mca_ptl_base_recv_frag_construct(mca_ptl_base_recv_frag_t* frag);
static void mca_ptl_base_recv_frag_destruct(mca_ptl_base_recv_frag_t* frag);


lam_class_info_t mca_ptl_base_recv_frag_t_class_info = { 
    "mca_ptl_base_recv_frag_t", 
    CLASS_INFO(mca_ptl_base_frag_t),
    (lam_construct_t) mca_ptl_base_recv_frag_construct, 
    (lam_destruct_t) mca_ptl_base_recv_frag_destruct 
};


void mca_ptl_base_recv_frag_construct(mca_ptl_base_recv_frag_t* frag)
{
    OBJ_CONSTRUCT_SUPER(frag, mca_ptl_base_frag_t);
}

void mca_ptl_base_recv_frag_destruct(mca_ptl_base_recv_frag_t* frag)
{
    OBJ_DESTRUCT_SUPER(frag, mca_ptl_base_frag_t);
}

