/*
 * $HEADER$
 */

#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_teg_sendreq.h"


int mca_pml_teg_isend_init(
    void *buf,
    size_t count,
    lam_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t sendmode,
    lam_communicator_t* comm,
    lam_request_t **request)
{
    int rc;

    mca_ptl_base_send_request_t* sendreq;
    MCA_PML_TEG_SEND_REQUEST_ALLOC(comm,dst,sendreq,rc);
    if(rc != LAM_SUCCESS)
        return rc;
 
    MCA_PTL_BASE_SEND_REQUEST_INIT(
        sendreq,
        buf,
        count,
        datatype,
        dst,
        tag,
        comm,
        sendmode,
        true
        );
         
    *request = (lam_request_t*)sendreq;
    return LAM_SUCCESS;
}
         

int mca_pml_teg_isend(
    void *buf,
    size_t count,
    lam_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t sendmode,
    lam_communicator_t* comm,
    lam_request_t **request)
{
    int rc;
    mca_ptl_base_send_request_t* sendreq;
    MCA_PML_TEG_SEND_REQUEST_ALLOC(comm,dst,sendreq,rc);
#if MCA_PML_TEG_STATISTICS
    mca_pml_teg.teg_isends++;
#endif
    if(rc != LAM_SUCCESS)
        return rc;
    MCA_PTL_BASE_SEND_REQUEST_INIT(
        sendreq,
        buf,
        count,
        datatype,
        dst,
        tag,
        comm,
        sendmode,
        false
        );
         
    if((rc = mca_pml_teg_send_request_start(sendreq)) != LAM_SUCCESS)
        return rc;
    *request = (lam_request_t*)sendreq;
    return LAM_SUCCESS;
}


int mca_pml_teg_send(
    void *buf,
    size_t count,
    lam_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t sendmode,
    lam_communicator_t* comm)
{
    int rc, index;
    mca_ptl_base_send_request_t* sendreq;
    MCA_PML_TEG_SEND_REQUEST_ALLOC(comm,dst,sendreq,rc);
#if MCA_PML_TEG_STATISTICS
    mca_pml_teg.teg_sends++;
#endif
    if(rc != LAM_SUCCESS)
        return rc;

    MCA_PTL_BASE_SEND_REQUEST_INIT(
        sendreq,
        buf,
        count,
        datatype,
        dst,
        tag,
        comm,
        sendmode,
        false
        );
         
    if((rc = mca_pml_teg_send_request_start(sendreq)) != LAM_SUCCESS) {
        MCA_PML_TEG_FREE((lam_request_t**)&sendreq);
        return rc;
    }

    if(sendreq->super.req_mpi_done == false) {
        /* give up and sleep until completion */
        if(lam_using_threads()) {
            lam_mutex_lock(&mca_pml_teg.teg_request_lock);
            mca_pml_teg.teg_request_waiting++;
            while(sendreq->super.req_mpi_done == false)
                lam_condition_wait(&mca_pml_teg.teg_request_cond, &mca_pml_teg.teg_request_lock);
            mca_pml_teg.teg_request_waiting--;
            lam_mutex_unlock(&mca_pml_teg.teg_request_lock);
        } else {
            mca_pml_teg.teg_request_waiting++;
            while(sendreq->super.req_mpi_done == false)
                lam_condition_wait(&mca_pml_teg.teg_request_cond, &mca_pml_teg.teg_request_lock);
            mca_pml_teg.teg_request_waiting--;
        }
    }

    /* return request to pool */
    MCA_PML_TEG_FREE((lam_request_t**)&sendreq);
    return LAM_SUCCESS;
}

