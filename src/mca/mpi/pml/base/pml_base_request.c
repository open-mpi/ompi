/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "mca/mpi/pml/base/pml_base_request.h"

lam_class_info_t mca_pml_base_request_cls = { 
    "mca_pml_base_request_t", 
    &lam_request_cls,
    (class_init_t) mca_pml_base_request_init, 
    (class_destroy_t) mca_pml_base_request_destroy 
};
                                                                                                 
void mca_pml_base_request_init(mca_pml_base_request_t* req)
{
    SUPER_INIT(req, &lam_request_cls);
}

void mca_pml_base_request_destroy(mca_pml_base_request_t* req)
{
    SUPER_DESTROY(req, &lam_request_cls);
}

