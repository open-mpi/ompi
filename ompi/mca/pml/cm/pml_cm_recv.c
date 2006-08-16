/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"

#include "ompi/request/request.h"
#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/convertor.h"

#include "pml_cm.h"
#include "pml_cm_recvreq.h"

int
mca_pml_cm_irecv_init(void *addr,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int src,
                      int tag,
                      struct ompi_communicator_t *comm,
                      struct ompi_request_t **request)
{
    int ret;
    mca_pml_cm_hvy_recv_request_t *recvreq;
    ompi_proc_t* ompi_proc;
    
    MCA_PML_CM_HVY_RECV_REQUEST_ALLOC(recvreq, ret);
    if (NULL == recvreq || OMPI_SUCCESS != ret) return ret;
    
    MCA_PML_CM_HVY_RECV_REQUEST_INIT(recvreq, ompi_proc, comm, tag, src, 
                                     datatype, addr, count, true); 
    
    *request = (ompi_request_t*) recvreq;

    return OMPI_SUCCESS;
}


int
mca_pml_cm_irecv(void *addr,
                 size_t count,
                 ompi_datatype_t * datatype,
                 int src,
                 int tag,
                 struct ompi_communicator_t *comm,
                 struct ompi_request_t **request)
{
    int ret;
    mca_pml_cm_thin_recv_request_t *recvreq;
    ompi_proc_t* ompi_proc;
    
    MCA_PML_CM_THIN_RECV_REQUEST_ALLOC(recvreq, ret);
    if (NULL == recvreq || OMPI_SUCCESS != ret) return ret;
    
    MCA_PML_CM_THIN_RECV_REQUEST_INIT(recvreq,
                                      ompi_proc,
                                      comm,
                                      tag,
                                      src,
                                      datatype,
                                      addr,
                                      count);
    
    MCA_PML_CM_THIN_RECV_REQUEST_START(recvreq, comm, tag, src, ret);

    if (OMPI_SUCCESS == ret) *request = (ompi_request_t*) recvreq;

    return ret;
}


int
mca_pml_cm_recv(void *addr,
                size_t count,
                ompi_datatype_t * datatype,
                int src,
                int tag,
                struct ompi_communicator_t *comm,
                ompi_status_public_t * status)
{
    int ret;
    mca_pml_cm_thin_recv_request_t *recvreq;
    ompi_proc_t* ompi_proc;
    
    MCA_PML_CM_THIN_RECV_REQUEST_ALLOC(recvreq, ret);
    if (NULL == recvreq || OMPI_SUCCESS != ret) return ret;

    MCA_PML_CM_THIN_RECV_REQUEST_INIT(recvreq,
                                      ompi_proc,
                                      comm, 
                                      tag,
                                      src,
                                      datatype,
                                      addr,
                                      count);
    
    
    MCA_PML_CM_THIN_RECV_REQUEST_START(recvreq, comm, tag, src, ret);
    if (OMPI_SUCCESS != ret) {
        /* BWB - XXX - need cleanup of request here */
        MCA_PML_CM_THIN_RECV_REQUEST_RETURN(recvreq);
    }

    if (recvreq->req_base.req_ompi.req_complete == false) {
        /* give up and sleep until completion */
        if (opal_using_threads()) {
            opal_mutex_lock(&ompi_request_lock);
            ompi_request_waiting++;
            while (recvreq->req_base.req_ompi.req_complete == false)
                opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
            ompi_request_waiting--;
            opal_mutex_unlock(&ompi_request_lock);
        } else {
            ompi_request_waiting++;
            while (recvreq->req_base.req_ompi.req_complete == false)
                opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
            ompi_request_waiting--;
        }
    }

    if (NULL != status) {  /* return status */
        *status = recvreq->req_base.req_ompi.req_status;
    }
    ret = recvreq->req_base.req_ompi.req_status.MPI_ERROR;
    ompi_request_free( (ompi_request_t**)&recvreq );

    return ret;
}

