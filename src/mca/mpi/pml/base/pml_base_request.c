/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "mca/mpi/pml/base/pml_base_request.h"

lam_class_info_t mca_pml_base_request_t_class_info = { 
    "mca_pml_base_request_t", 
    CLASS_INFO(lam_request_t),
    (lam_construct_t) mca_pml_base_request_construct, 
    (lam_destruct_t) mca_pml_base_request_destruct 
};
                                                                                                 
void mca_pml_base_request_construct(mca_pml_base_request_t* req)
{
    OBJ_CONSTRUCT_SUPER(req, lam_request_t);
}

void mca_pml_base_request_destruct(mca_pml_base_request_t* req)
{
    OBJ_DESTRUCT_SUPER(req, lam_request_t);
}

