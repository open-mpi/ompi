#include "pml_teg.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "mca/pml/base/pml_base_request.h"



int mca_pml_teg_wait(
    int count,
    lam_request_t** request,
    int *index,
    lam_status_public_t* status)
{
#if 0
    mca_pml_base_request_t* pml_request = *(mca_pml_base_request_t**)request;
    if(pml_request->req_mpi_done == false) {

        /* poll for completion - primarily for benchmarks */
        int i;
        for(i=0; i<mca_pml_teg.teg_poll_iterations && pml_request->req_mpi_done == false; i++) 
            ; /* do nothing */

        /* if not complete - sleep on condition variable until a request completes */
        if(pml_request->req_mpi_done == false) {
            lam_mutex_lock(&mca_pml_teg.teg_request_lock);
            mca_pml_teg.teg_request_waiting++;
            while(pml_request->req_mpi_done == false)
                lam_condition_wait(&mca_pml_teg.teg_request_cond, &mca_pml_teg.teg_request_lock);
            mca_pml_teg.teg_request_waiting--;
            lam_mutex_unlock(&mca_pml_teg.teg_request_lock);
        }
    }

    /* return request to pool */
    if(pml_request->req_persistent == false) {
        mca_pml_teg_free(request);
    }

    if (status != NULL) {
       *status = pml_request->req_status;
    }
#endif
    return LAM_SUCCESS;
}


int mca_pml_teg_waitall(
    int count,
    lam_request_t** request,
    lam_status_public_t* status)
{
    return LAM_SUCCESS;
}

