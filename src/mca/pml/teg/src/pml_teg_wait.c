#include "pml_teg.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "mca/pml/base/pml_base_request.h"



int mca_pml_teg_wait(
    size_t count,
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
    if(false == pml_request->req_persistent) {
        mca_pml_teg_free(request);
    }
    if (NULL != status) {
       *status = pml_request->req_status;
    }
    *index = completed;
    return LAM_SUCCESS;
}


int mca_pml_teg_wait_all(
    size_t count,
    lam_request_t** requests,
    lam_status_public_t* statuses)
{
    int completed = 0, i;
    for(i=0; i<count; i++) {
        mca_pml_base_request_t* pml_request = (mca_pml_base_request_t*)requests[i];
        if(pml_request == NULL || pml_request->req_mpi_done == true) {
            completed++;
        } 
    }

    /* if all requests have not completed -- defer requiring lock unless required */
    if(completed != count) {
        /*
         * acquire lock and test for completion - if all requests are not completed
         * pend on condition variable until a request completes 
         */
        lam_mutex_lock(&mca_pml_teg.teg_request_lock);
        mca_pml_teg.teg_request_waiting++;
        do {
            completed = 0;
            for(i=0; i<count; i++) {
                mca_pml_base_request_t* pml_request = (mca_pml_base_request_t*)requests[i];
                if(pml_request == NULL || pml_request->req_mpi_done == true) {
                    completed++;
                    continue;
                }
            } 
            if(completed != count)
                lam_condition_wait(&mca_pml_teg.teg_request_cond, &mca_pml_teg.teg_request_lock);
        } while (completed != count);
        mca_pml_teg.teg_request_waiting--;
        lam_mutex_unlock(&mca_pml_teg.teg_request_lock);
    }

    if(NULL != statuses) {
        /* fill out status and free request if required */
        for(i=0; i<count; i++) {
            mca_pml_base_request_t* pml_request = (mca_pml_base_request_t*)requests[i];
            if (NULL == pml_request) {
                statuses[i] = mca_pml_teg.teg_request_null.req_status;
            } else {
                statuses[i] = pml_request->req_status;
                if (false == pml_request->req_persistent) {
                    mca_pml_teg_free(&requests[i]);
                }
            }
        }
    } else {
        /* free request if required */
        for(i=0; i<count; i++) {
            mca_pml_base_request_t* pml_request = (mca_pml_base_request_t*)requests[i];
            if (NULL != pml_request && false == pml_request->req_persistent) {
                    mca_pml_teg_free(&requests[i]);
            }
        }
    }
    return LAM_SUCCESS;
}

