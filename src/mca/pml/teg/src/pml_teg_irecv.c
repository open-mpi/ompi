#include "pml_teg_recvreq.h"

                                                                                                             
int mca_pml_teg_irecv_init(
    void *addr,
    size_t count,
    ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request)
{
    int rc;
    mca_ptl_base_recv_request_t *recvreq;
    MCA_PML_TEG_RECV_REQUEST_ALLOC(recvreq, rc);
    if(NULL == recvreq)
        return rc;
    
    MCA_PTL_BASE_RECV_REQUEST_INIT(
        recvreq,
        addr,
        count,
        datatype,
        src,
        tag,
        comm,
        true);

    *request = (ompi_request_t*)recvreq;
    return OMPI_SUCCESS;
}
                                                                                                                              
int mca_pml_teg_irecv(
    void *addr,
    size_t count,
    ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request)
{
    int rc;

    mca_ptl_base_recv_request_t *recvreq;
    MCA_PML_TEG_RECV_REQUEST_ALLOC(recvreq, rc);
#if MCA_PML_TEG_STATISTICS
    mca_pml_teg.teg_irecvs++;
#endif
    if(NULL == recvreq)
        return rc;

    MCA_PTL_BASE_RECV_REQUEST_INIT(
        recvreq,
        addr,
        count,
        datatype,
        src,
        tag,
        comm,
        false);

    if((rc = mca_pml_teg_recv_request_start(recvreq)) != OMPI_SUCCESS) {
        MCA_PML_TEG_RECV_REQUEST_RETURN(recvreq);
        return rc;
    }
    *request = (ompi_request_t*)recvreq;
    return OMPI_SUCCESS;
}


int mca_pml_teg_recv(
    void *addr,
    size_t count,
    ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    ompi_status_public_t* status)
{
    int rc;
    mca_ptl_base_recv_request_t *recvreq;
    MCA_PML_TEG_RECV_REQUEST_ALLOC(recvreq, rc);
 #if MCA_PML_TEG_STATISTICS
    mca_pml_teg.teg_recvs++;
 #endif
    if(NULL == recvreq)
        return rc;

    MCA_PTL_BASE_RECV_REQUEST_INIT(
        recvreq,
        addr,
        count,
        datatype,
        src,
        tag,
        comm,
        false);

    if((rc = mca_pml_teg_recv_request_start(recvreq)) != OMPI_SUCCESS) {
        MCA_PML_TEG_RECV_REQUEST_RETURN(recvreq);
        return rc;
    }

    if(recvreq->super.req_mpi_done == false) {
        /* give up and sleep until completion */
        if(ompi_using_threads()) {
            ompi_mutex_lock(&mca_pml_teg.teg_request_lock);
            mca_pml_teg.teg_request_waiting++;
            while(recvreq->super.req_mpi_done == false)
                ompi_condition_wait(&mca_pml_teg.teg_request_cond, &mca_pml_teg.teg_request_lock);
            mca_pml_teg.teg_request_waiting--;
            ompi_mutex_unlock(&mca_pml_teg.teg_request_lock);
        } else {
            mca_pml_teg.teg_request_waiting++;
            while(recvreq->super.req_mpi_done == false)
                ompi_condition_wait(&mca_pml_teg.teg_request_cond, &mca_pml_teg.teg_request_lock);
            mca_pml_teg.teg_request_waiting--;
        }
    }
    MCA_PML_TEG_RECV_REQUEST_RETURN(recvreq);
    return OMPI_SUCCESS;
}

