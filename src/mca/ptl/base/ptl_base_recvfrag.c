/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "ompi_config.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/ptl/base/ptl_base_match.h"

static void mca_ptl_base_recv_frag_construct(mca_ptl_base_recv_frag_t* frag);
static void mca_ptl_base_recv_frag_destruct(mca_ptl_base_recv_frag_t* frag);


OBJ_CLASS_INSTANCE(
    mca_ptl_base_recv_frag_t, 
    mca_ptl_base_frag_t,
    mca_ptl_base_recv_frag_construct, 
    mca_ptl_base_recv_frag_destruct 
);


void mca_ptl_base_recv_frag_construct(mca_ptl_base_recv_frag_t* frag)
{
    frag->frag_base.frag_type = MCA_PTL_FRAGMENT_RECV;
}

void mca_ptl_base_recv_frag_destruct(mca_ptl_base_recv_frag_t* frag)
{
}

