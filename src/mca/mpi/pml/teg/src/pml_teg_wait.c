#include "pml_teg.h"


int mca_pml_teg_wait(
    lam_request_t* request,
    lam_status_public_t* status)
{
#if 0
    mca_pml_base_request_t* pml_request = (mca_pml_base_request_t*)request;
    if(pml_request->req_mpi_done == false) {
        lam_mutex_lock(&mca_pml_teg.teg_lock);
        mca_pml_teg.teg_req_waiting++;
        while(request->req_mpi_done == false)
            lam_condition_wait(&mca_pml_teg.teg_condition);
        mca_pml_teg.teg_req_waiting--;
        lam_mutex_unlock(&mca_pml_teg.teg_lock);
    }
    *status = request->req_status;
#endif
    return LAM_SUCCESS;
}

