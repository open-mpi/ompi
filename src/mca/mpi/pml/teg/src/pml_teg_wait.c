#include "pml_teg.h"
#include "mca/mpi/ptl/base/ptl_base_comm.h"
#include "mca/mpi/pml/base/pml_base_request.h"



int mca_pml_teg_wait(
    lam_request_t** request,
    lam_status_public_t* status)
{
    mca_pml_base_request_t* pml_request = *(mca_pml_base_request_t**)request;
    if(pml_request->req_mpi_done == false) {

        /* poll status - primarily for benchmarks */
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
    if (status != NULL)
       *status = pml_request->req_status;
    return LAM_SUCCESS;
}

