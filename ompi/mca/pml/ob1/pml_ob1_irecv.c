/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "pml_ob1_recvreq.h"
#include "ompi/peruse/peruse-internal.h"

int mca_pml_ob1_irecv_init(void *addr,
                           size_t count,
                           ompi_datatype_t * datatype,
                           int src,
                           int tag,
                           struct ompi_communicator_t *comm,
                           struct ompi_request_t **request)
{
    int rc;
    mca_pml_ob1_recv_request_t *recvreq;
    MCA_PML_OB1_RECV_REQUEST_ALLOC(recvreq, rc);
    if (NULL == recvreq)
        return rc;

    MCA_PML_OB1_RECV_REQUEST_INIT(recvreq,
                                   addr,
                                   count, datatype, src, tag, comm, true);

    *request = (ompi_request_t *) recvreq;
    return OMPI_SUCCESS;
}

int mca_pml_ob1_irecv(void *addr,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int src,
                      int tag,
                      struct ompi_communicator_t *comm,
                      struct ompi_request_t **request)
{
    int rc;

    mca_pml_ob1_recv_request_t *recvreq;
    MCA_PML_OB1_RECV_REQUEST_ALLOC(recvreq, rc);
    if (NULL == recvreq)
        return rc;

    MCA_PML_OB1_RECV_REQUEST_INIT(recvreq,
                                   addr,
                                   count, datatype, src, tag, comm, false);

    MCA_PML_OB1_RECV_REQUEST_START(recvreq);
    *request = (ompi_request_t *) recvreq;
    return OMPI_SUCCESS;
}


int mca_pml_ob1_recv(void *addr,
                     size_t count,
                     ompi_datatype_t * datatype,
                     int src,
                     int tag,
                     struct ompi_communicator_t *comm,
                     ompi_status_public_t * status)
{
    int rc;
    mca_pml_ob1_recv_request_t *recvreq;
    MCA_PML_OB1_RECV_REQUEST_ALLOC(recvreq, rc);
    if (NULL == recvreq)
        return rc;

    MCA_PML_OB1_RECV_REQUEST_INIT(recvreq,
                                   addr,
                                   count, datatype, src, tag, comm, false);

    MCA_PML_OB1_RECV_REQUEST_START(recvreq);
    if (recvreq->req_recv.req_base.req_ompi.req_complete == false) {
#if OMPI_ENABLE_PROGRESS_THREADS
        if(opal_progress_spin(&recvreq->req_recv.req_base.req_ompi.req_complete)) {
            goto finished;
        }
#endif
        /* give up and sleep until completion */
        if (opal_using_threads()) {
            opal_mutex_lock(&ompi_request_lock);
            ompi_request_waiting++;
            while (recvreq->req_recv.req_base.req_ompi.req_complete == false)
                opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
            ompi_request_waiting--;
            opal_mutex_unlock(&ompi_request_lock);
        } else {
            ompi_request_waiting++;
            while (recvreq->req_recv.req_base.req_ompi.req_complete == false)
                opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
            ompi_request_waiting--;
        }
    }

#if OMPI_ENABLE_PROGRESS_THREADS
finished:
#endif

    if (NULL != status) {  /* return status */
        *status = recvreq->req_recv.req_base.req_ompi.req_status;
    }
    rc = recvreq->req_recv.req_base.req_ompi.req_status.MPI_ERROR;
    ompi_request_free( (ompi_request_t**)&recvreq );
    return rc;
}
