/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_teg_sendreq.h"
#include "pml_teg_recvreq.h"


int mca_pml_teg_isend_init(void *buf,
                           size_t count,
                           ompi_datatype_t * datatype,
                           int dst,
                           int tag,
                           mca_pml_base_send_mode_t sendmode,
                           ompi_communicator_t * comm,
                           ompi_request_t ** request)
{
    int rc;

    mca_pml_base_send_request_t *sendreq;
    MCA_PML_TEG_SEND_REQUEST_ALLOC(comm, dst, sendreq, rc);
    if (rc != OMPI_SUCCESS)
        return rc;

    MCA_PML_BASE_SEND_REQUEST_INIT(sendreq,
                                   buf,
                                   count,
                                   datatype,
                                   dst, tag,
                                   comm, sendmode, true);

    *request = (ompi_request_t *) sendreq;
    return OMPI_SUCCESS;
}


int mca_pml_teg_isend(void *buf,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int dst,
                      int tag,
                      mca_pml_base_send_mode_t sendmode,
                      ompi_communicator_t * comm,
                      ompi_request_t ** request)
{
    int rc;
    mca_pml_base_send_request_t *sendreq;
    MCA_PML_TEG_SEND_REQUEST_ALLOC(comm, dst, sendreq, rc);
    if (rc != OMPI_SUCCESS)
        return rc;
    MCA_PML_BASE_SEND_REQUEST_INIT(sendreq,
                                   buf,
                                   count,
                                   datatype,
                                   dst, tag,
                                   comm, sendmode, false);

    MCA_PML_TEG_SEND_REQUEST_START(sendreq, rc);
    if (rc != OMPI_SUCCESS)
        return rc;
    *request = (ompi_request_t *) sendreq;
    return OMPI_SUCCESS;
}


int mca_pml_teg_send(void *buf,
                     size_t count,
                     ompi_datatype_t * datatype,
                     int dst,
                     int tag,
                     mca_pml_base_send_mode_t sendmode,
                     ompi_communicator_t * comm)
{
    int rc;
    mca_pml_base_send_request_t *sendreq;
    MCA_PML_TEG_SEND_REQUEST_ALLOC(comm, dst, sendreq, rc);
    if (rc != OMPI_SUCCESS)
        return rc;

    MCA_PML_BASE_SEND_REQUEST_INIT(sendreq,
                                   buf,
                                   count,
                                   datatype,
                                   dst, tag,
                                   comm, sendmode, false);

    MCA_PML_TEG_SEND_REQUEST_START(sendreq, rc);
    if (rc != OMPI_SUCCESS) {
        MCA_PML_TEG_FREE((ompi_request_t **) & sendreq);
        return rc;
    }

    if (sendreq->req_base.req_ompi.req_complete == false) {
        /* give up and sleep until completion */
        if (ompi_using_threads()) {
            ompi_mutex_lock(&ompi_request_lock);
            ompi_request_waiting++;
            while (sendreq->req_base.req_ompi.req_complete == false)
                ompi_condition_wait(&ompi_request_cond, &ompi_request_lock);
            ompi_request_waiting--;
            ompi_mutex_unlock(&ompi_request_lock);
        } else {
            ompi_request_waiting++;
            while (sendreq->req_base.req_ompi.req_complete == false)
                ompi_condition_wait(&ompi_request_cond, &ompi_request_lock);
            ompi_request_waiting--;
        }
    }

    /* return request to pool */
    MCA_PML_TEG_FREE((ompi_request_t **) & sendreq);
    return OMPI_SUCCESS;
}

