/*
 * $HEADER$
 */
#include "ompi_config.h"
#include "ptl_mx.h"
#include "ptl_mx_recvfrag.h"


static void mca_ptl_mx_recv_frag_construct(mca_ptl_mx_recv_frag_t* frag);
static void mca_ptl_mx_recv_frag_destruct(mca_ptl_mx_recv_frag_t* frag);


OBJ_CLASS_INSTANCE(
    mca_ptl_mx_recv_frag_t,
    mca_ptl_base_recv_frag_t,
    mca_ptl_mx_recv_frag_construct,
    mca_ptl_mx_recv_frag_destruct);
                                                                                                           
/*
 * MX recv fragment constructor
 */

static void mca_ptl_mx_recv_frag_construct(mca_ptl_mx_recv_frag_t* frag)
{
}


/*
 * MX recv fragment destructor
 */

static void mca_ptl_mx_recv_frag_destruct(mca_ptl_mx_recv_frag_t* frag)
{
}

