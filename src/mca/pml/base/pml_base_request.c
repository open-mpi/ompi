/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "mca/pml/base/pml_base_request.h"

static void mca_pml_base_request_construct(mca_pml_base_request_t* req)
{
    req->req_ompi.req_type = OMPI_REQUEST_PML;
}

static void mca_pml_base_request_destruct(mca_pml_base_request_t* req)
{
}

ompi_class_t mca_pml_base_request_t_class = { 
    "mca_pml_base_request_t", 
    OBJ_CLASS(ompi_request_t),
    (ompi_construct_t) mca_pml_base_request_construct, 
    (ompi_destruct_t) mca_pml_base_request_destruct 
};
                                                                                                 

