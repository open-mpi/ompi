/*
 * $HEADER$
 */

#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_teg_sendreq.h"


int mca_pml_teg_isend_init(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t sendmode,
    ompi_communicator_t* comm,
    ompi_request_t **request)
{
    int rc;

    mca_ptl_base_send_request_t* sendreq;
    MCA_PML_TEG_SEND_REQUEST_ALLOC(comm,dst,sendreq,rc);
    if(rc != OMPI_SUCCESS)
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
         
    *request = (ompi_request_t*)sendreq;
    return OMPI_SUCCESS;
}
         

int mca_pml_teg_isend(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t sendmode,
    ompi_communicator_t* comm,
    ompi_request_t **request)
{
    int rc;
    mca_ptl_base_send_request_t* sendreq;
    MCA_PML_TEG_SEND_REQUEST_ALLOC(comm,dst,sendreq,rc);
#if MCA_PML_TEG_STATISTICS
    mca_pml_teg.teg_isends++;
#endif
    if(rc != OMPI_SUCCESS)
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
         
    if((rc = mca_pml_teg_send_request_start(sendreq)) != OMPI_SUCCESS)
        return rc;
    *request = (ompi_request_t*)sendreq;
    return OMPI_SUCCESS;
}


int mca_pml_teg_send(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t sendmode,
    ompi_communicator_t* comm)
{
    int rc;
    mca_ptl_base_send_request_t* sendreq;
    MCA_PML_TEG_SEND_REQUEST_ALLOC(comm,dst,sendreq,rc);
#if MCA_PML_TEG_STATISTICS
    mca_pml_teg.teg_sends++;
#endif
    if(rc != OMPI_SUCCESS)
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
         
    if((rc = mca_pml_teg_send_request_start(sendreq)) != OMPI_SUCCESS) {
        MCA_PML_TEG_FREE((ompi_request_t**)&sendreq);
        return rc;
    }

    if(sendreq->super.req_mpi_done == false) {
        /* give up and sleep until completion */
        if(ompi_using_threads()) {
            ompi_mutex_lock(&mca_pml_teg.teg_request_lock);
            mca_pml_teg.teg_request_waiting++;
            while(sendreq->super.req_mpi_done == false)
                ompi_condition_wait(&mca_pml_teg.teg_request_cond, &mca_pml_teg.teg_request_lock);
            mca_pml_teg.teg_request_waiting--;
            ompi_mutex_unlock(&mca_pml_teg.teg_request_lock);
        } else {
            mca_pml_teg.teg_request_waiting++;
            while(sendreq->super.req_mpi_done == false)
                ompi_condition_wait(&mca_pml_teg.teg_request_cond, &mca_pml_teg.teg_request_lock);
            mca_pml_teg.teg_request_waiting--;
        }
    }

    /* return request to pool */
    MCA_PML_TEG_FREE((ompi_request_t**)&sendreq);
    return OMPI_SUCCESS;
}

