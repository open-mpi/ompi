#include "pml_teg.h"

                                                                                                                              
int mca_pml_teg_test(
    lam_request_t** request,
    int *completed,
    lam_status_public_t* status)
{
    mca_pml_base_request_t* pml_request = *(mca_pml_base_request_t**)request;
    if(pml_request->req_mpi_done) {
        *completed = true;
        mca_pml_teg_request_return(pml_request);
        *request = NULL;
        if (status != NULL)
            *status = pml_request->req_status;
    } else {
        *completed = false;
    }
    return LAM_SUCCESS;
}

