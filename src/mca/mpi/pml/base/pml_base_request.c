/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "mca/mpi/pml/base/pml_base_request.h"

lam_class_t mca_pml_base_request_t_class = { 
    "mca_pml_base_request_t", 
    OBJ_CLASS(lam_request_t),
    (lam_construct_t) mca_pml_base_request_construct, 
    (lam_destruct_t) mca_pml_base_request_destruct 
};
                                                                                                 
void mca_pml_base_request_construct(mca_pml_base_request_t* req)
{
    lam_mutex_init(&req->req_lock);
}

void mca_pml_base_request_destruct(mca_pml_base_request_t* req)
{
    lam_mutex_destroy(&req->req_lock);
}

