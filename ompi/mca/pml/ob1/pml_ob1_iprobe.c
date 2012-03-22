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
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2011-2012 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "ompi/message/message.h"
#include "pml_ob1_recvreq.h"


int mca_pml_ob1_iprobe(int src,
                       int tag,
                       struct ompi_communicator_t *comm,
                       int *matched, ompi_status_public_t * status)
{
    int rc = OMPI_SUCCESS;
    mca_pml_ob1_recv_request_t recvreq;

    OBJ_CONSTRUCT( &recvreq, mca_pml_ob1_recv_request_t );
    recvreq.req_recv.req_base.req_ompi.req_type = OMPI_REQUEST_PML;
    recvreq.req_recv.req_base.req_type = MCA_PML_REQUEST_IPROBE;

    MCA_PML_OB1_RECV_REQUEST_INIT(&recvreq, NULL, 0, &ompi_mpi_char.dt, src, tag, comm, true);
    MCA_PML_OB1_RECV_REQUEST_START(&recvreq);

    if( recvreq.req_recv.req_base.req_ompi.req_complete == true ) {
        if( NULL != status ) {
            OMPI_STATUS_SET(status, &recvreq.req_recv.req_base.req_ompi.req_status);
        }
        rc = recvreq.req_recv.req_base.req_ompi.req_status.MPI_ERROR;
        *matched = 1;
    } else {
        *matched = 0;
        opal_progress();
    }
    MCA_PML_BASE_RECV_REQUEST_FINI( &recvreq.req_recv );
    return rc;
}


int mca_pml_ob1_probe(int src,
                      int tag,
                      struct ompi_communicator_t *comm,
                      ompi_status_public_t * status)
{
    int rc = OMPI_SUCCESS;
    mca_pml_ob1_recv_request_t recvreq;

    OBJ_CONSTRUCT( &recvreq, mca_pml_ob1_recv_request_t );
    recvreq.req_recv.req_base.req_ompi.req_type = OMPI_REQUEST_PML;
    recvreq.req_recv.req_base.req_type = MCA_PML_REQUEST_PROBE;

    MCA_PML_OB1_RECV_REQUEST_INIT(&recvreq, NULL, 0, &ompi_mpi_char.dt, src, tag, comm, true);
    MCA_PML_OB1_RECV_REQUEST_START(&recvreq);

    ompi_request_wait_completion(&recvreq.req_recv.req_base.req_ompi);
    rc = recvreq.req_recv.req_base.req_ompi.req_status.MPI_ERROR;
    if (NULL != status) {
        OMPI_STATUS_SET(status, &recvreq.req_recv.req_base.req_ompi.req_status);
    }

    MCA_PML_BASE_RECV_REQUEST_FINI( &recvreq.req_recv );
    return rc;
}


int
mca_pml_ob1_improbe(int src,
                    int tag,
                    struct ompi_communicator_t *comm,
                    int *matched, 
                    struct ompi_message_t **message,
                    ompi_status_public_t * status)
{
    int rc = OMPI_SUCCESS;
    mca_pml_ob1_recv_request_t *recvreq;

    MCA_PML_OB1_RECV_REQUEST_ALLOC(recvreq, rc);
    if (NULL == recvreq)
        return rc;
    recvreq->req_recv.req_base.req_type = MCA_PML_REQUEST_IMPROBE;

    /* initialize the request enough to probe and get the status */
    MCA_PML_OB1_RECV_REQUEST_INIT(recvreq, NULL, 0, &ompi_mpi_char.dt, 
                                  src, tag, comm, false);
    MCA_PML_OB1_RECV_REQUEST_START(recvreq);

    if( recvreq->req_recv.req_base.req_ompi.req_complete == true ) {
        if( NULL != status ) {
            *status = recvreq->req_recv.req_base.req_ompi.req_status;
        }
        *matched = 1;

        *message = ompi_message_alloc();
        (*message)->comm = comm;
        (*message)->req_ptr = recvreq;
        (*message)->peer = recvreq->req_recv.req_base.req_ompi.req_status.MPI_STATUS;
        (*message)->count = recvreq->req_recv.req_base.req_ompi.req_status._ucount;

        rc = OMPI_SUCCESS;
    } else {
        *matched = 0;

        /* we only free if we didn't match, because we're going to
           translate the request into a receive request later on if it
           was matched */
        ompi_request_free((ompi_request_t**)&recvreq);
        
        opal_progress();
    }

    return rc;
}


int
mca_pml_ob1_mprobe(int src,
                   int tag,
                   struct ompi_communicator_t *comm,
                   struct ompi_message_t **message,
                   ompi_status_public_t * status)
{
    int rc = OMPI_SUCCESS;
    mca_pml_ob1_recv_request_t *recvreq;

    MCA_PML_OB1_RECV_REQUEST_ALLOC(recvreq, rc);
    if (NULL == recvreq)
        return rc;
    recvreq->req_recv.req_base.req_type = MCA_PML_REQUEST_MPROBE;

    /* initialize the request enough to probe and get the status */
    MCA_PML_OB1_RECV_REQUEST_INIT(recvreq, NULL, 0, &ompi_mpi_char.dt, 
                                  src, tag, comm, false);
    MCA_PML_OB1_RECV_REQUEST_START(recvreq);

    ompi_request_wait_completion(&recvreq->req_recv.req_base.req_ompi);

    if( NULL != status ) {
        *status = recvreq->req_recv.req_base.req_ompi.req_status;
    }

    *message = ompi_message_alloc();
    (*message)->comm = comm;
    (*message)->req_ptr = recvreq;
    (*message)->count = recvreq->req_recv.req_base.req_ompi.req_status._ucount;

    return OMPI_SUCCESS;
}
