/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "mca/mpi/ptl/base/ptl_base_recvreq.h"

lam_class_info_t mca_ptl_base_recv_request_cls = { 
    "mca_ptl_base_recv_request_t", 
    &mca_pml_base_request_cls,
    (class_init_t) mca_ptl_base_recv_request_init, 
    (class_destroy_t) mca_ptl_base_recv_request_destroy 
};
                                                                                                 

void mca_ptl_base_recv_request_init(mca_ptl_base_recv_request_t* req)
{
    SUPER_INIT(req, &mca_pml_base_request_cls);
}

void mca_ptl_base_recv_request_destroy(mca_ptl_base_recv_request_t* req)
{
    SUPER_DESTROY(req, &mca_pml_base_request_cls);
}

