#include "pml_teg.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "mca/pml/base/pml_base_request.h"



int mca_pml_teg_wait(
    int count,
    lam_request_t** request,
    int *index,
    lam_status_public_t* status)
{
    int c, i;
    int completed = -1;
    mca_pml_base_request_t* pml_request;

    /* poll for completion */
    for(c=0; completed < 0 && c < mca_pml_teg.teg_poll_iterations; c++) {
        for(i=0; i<count; i++) {
            pml_request = (mca_pml_base_request_t*)request[i];
            if(pml_request == NULL)
                continue; 
            if(pml_request->req_mpi_done == true) {
                completed = i;
                break;
            }
        }
    }

    if(completed < 0) {
        /* give up and sleep until completion */
        lam_mutex_lock(&mca_pml_teg.teg_request_lock);
        mca_pml_teg.teg_request_waiting++;
        while(completed < 0) {
            for(i=0; i<count; i++) {
                pml_request = (mca_pml_base_request_t*)request[i];
                if(pml_request->req_mpi_done == true) {
                    completed = i;
                    break;
                }
                lam_condition_wait(&mca_pml_teg.teg_request_cond, &mca_pml_teg.teg_request_lock);
            }
        }
        mca_pml_teg.teg_request_waiting--;
        lam_mutex_unlock(&mca_pml_teg.teg_request_lock);
    }

    /* return request to pool */
    if(pml_request->req_persistent == false) {
        mca_pml_teg_free(request);
    }
    if (status != NULL) {
       *status = pml_request->req_status;
    }
    if (index != NULL) {
       *index = completed;
    }
    return LAM_SUCCESS;
}


int mca_pml_teg_waitall(
    int count,
    lam_request_t** request,
    lam_status_public_t* status)
{
    return LAM_SUCCESS;
}

