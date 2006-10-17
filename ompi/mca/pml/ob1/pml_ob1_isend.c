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

#include "pml_ob1.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_recvreq.h"
#include "ompi/peruse/peruse-internal.h"

int mca_pml_ob1_isend_init(void *buf,
                           size_t count,
                           ompi_datatype_t * datatype,
                           int dst,
                           int tag,
                           mca_pml_base_send_mode_t sendmode,
                           ompi_communicator_t * comm,
                           ompi_request_t ** request)
{
    int rc;
    
    mca_pml_ob1_send_request_t *sendreq = NULL;
    MCA_PML_OB1_SEND_REQUEST_ALLOC(comm, dst, sendreq, rc);
    if (rc != OMPI_SUCCESS)
        return rc;

    MCA_PML_OB1_SEND_REQUEST_INIT(sendreq,
                                   buf,
                                   count,
                                   datatype,
                                   dst, tag,
                                   comm, sendmode, true);

    *request = (ompi_request_t *) sendreq;
    return OMPI_SUCCESS;
}


int mca_pml_ob1_isend(void *buf,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int dst,
                      int tag,
                      mca_pml_base_send_mode_t sendmode,
                      ompi_communicator_t * comm,
                      ompi_request_t ** request)
{
    int rc;
    mca_pml_ob1_send_request_t *sendreq = NULL;
    MCA_PML_OB1_SEND_REQUEST_ALLOC(comm, dst, sendreq, rc);
    if (rc != OMPI_SUCCESS)
        return rc;
    
    MCA_PML_OB1_SEND_REQUEST_INIT(sendreq,
                                   buf,
                                   count,
                                   datatype,
                                   dst, tag,
                                   comm, sendmode, false);

    MCA_PML_OB1_SEND_REQUEST_START(sendreq, rc);
    *request = (ompi_request_t *) sendreq;
    return rc;
}


int mca_pml_ob1_send(void *buf,
                     size_t count,
                     ompi_datatype_t * datatype,
                     int dst,
                     int tag,
                     mca_pml_base_send_mode_t sendmode,
                     ompi_communicator_t * comm)
{
    int rc;
    mca_pml_ob1_send_request_t *sendreq;
    MCA_PML_OB1_SEND_REQUEST_ALLOC(comm, dst, sendreq, rc);
    if (rc != OMPI_SUCCESS)
        return rc;
    
    MCA_PML_OB1_SEND_REQUEST_INIT(sendreq,
                                  buf,
                                  count,
                                  datatype,
                                  dst, tag,
                                  comm, sendmode, false);
    
    MCA_PML_OB1_SEND_REQUEST_START(sendreq, rc);
    if (rc != OMPI_SUCCESS) {
        MCA_PML_OB1_SEND_REQUEST_RETURN( sendreq );
        return rc;
    }

    if (sendreq->req_send.req_base.req_ompi.req_complete == false) {
#if OMPI_ENABLE_PROGRESS_THREADS
        if(opal_progress_spin(&sendreq->req_send.req_base.req_ompi.req_complete)) {
            ompi_request_free( (ompi_request_t**)&sendreq );
            return OMPI_SUCCESS;
        }
#endif

        /* give up and sleep until completion */
        if (opal_using_threads()) {
            opal_mutex_lock(&ompi_request_lock);
            ompi_request_waiting++;
            while (sendreq->req_send.req_base.req_ompi.req_complete == false)
                opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
            ompi_request_waiting--;
            opal_mutex_unlock(&ompi_request_lock);
        } else {
            ompi_request_waiting++;
            while (sendreq->req_send.req_base.req_ompi.req_complete == false)
                opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
            ompi_request_waiting--;
        }
    }

    rc = sendreq->req_send.req_base.req_ompi.req_status.MPI_ERROR;
    ompi_request_free( (ompi_request_t**)&sendreq );
    return rc;
}

