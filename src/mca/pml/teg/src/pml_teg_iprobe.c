/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "request/request.h"
#include "pml_teg_recvreq.h"


int mca_pml_teg_iprobe(int src,
                       int tag,
                       struct ompi_communicator_t *comm,
                       int *matched, ompi_status_public_t * status)
{
    int rc;
    mca_pml_base_recv_request_t recvreq;

    OBJ_CONSTRUCT( &(recvreq), mca_pml_base_recv_request_t );
    recvreq.req_base.req_ompi.req_type = OMPI_REQUEST_PML;
    recvreq.req_base.req_type = MCA_PML_REQUEST_IPROBE;
    MCA_PML_BASE_RECV_REQUEST_INIT(&recvreq, NULL, 0, &ompi_mpi_char, src, tag, comm, true);

    *matched = 0;
    if ((rc = mca_pml_teg_recv_request_start(&recvreq)) == OMPI_SUCCESS) {
        if( recvreq.req_base.req_ompi.req_complete == true ) {
            if( NULL != status ) {
                *status = recvreq.req_base.req_ompi.req_status;
            }
            *matched = 1;
        } else {
            /* we are supposed to progress ... */
            ompi_progress();
        }
    }
    MCA_PML_BASE_RECV_REQUEST_RETURN( &recvreq );
    return rc;
}


int mca_pml_teg_probe(int src,
                      int tag,
                      struct ompi_communicator_t *comm,
                      ompi_status_public_t * status)
{
    int rc;
    mca_pml_base_recv_request_t recvreq;

    OBJ_CONSTRUCT( &(recvreq), mca_pml_base_recv_request_t );
    recvreq.req_base.req_ompi.req_type = OMPI_REQUEST_PML;
    recvreq.req_base.req_type = MCA_PML_REQUEST_PROBE;
    MCA_PML_BASE_RECV_REQUEST_INIT(&recvreq, NULL, 0, &ompi_mpi_char, src, tag, comm, true);

    if ((rc = mca_pml_teg_recv_request_start(&recvreq)) != OMPI_SUCCESS) {
        MCA_PML_BASE_RECV_REQUEST_RETURN( &recvreq );
        return rc;
    }

    if (recvreq.req_base.req_ompi.req_complete == false) {
        /* give up and sleep until completion */
        if (ompi_using_threads()) {
            ompi_mutex_lock(&ompi_request_lock);
            ompi_request_waiting++;
            while (recvreq.req_base.req_ompi.req_complete == false)
                ompi_condition_wait(&ompi_request_cond, &ompi_request_lock);
            ompi_request_waiting--;
            ompi_mutex_unlock(&ompi_request_lock);
        } else {
            ompi_request_waiting++;
            while (recvreq.req_base.req_ompi.req_complete == false)
                ompi_condition_wait(&ompi_request_cond, &ompi_request_lock);
            ompi_request_waiting--;
        }
    }

    if (NULL != status) {
        *status = recvreq.req_base.req_ompi.req_status;
    }
    MCA_PML_BASE_RECV_REQUEST_RETURN( &recvreq );
    return OMPI_SUCCESS;
}

