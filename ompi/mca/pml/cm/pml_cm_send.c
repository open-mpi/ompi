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

#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/convertor.h"

#include "pml_cm.h"
#include "pml_cm_sendreq.h"

int
mca_pml_cm_isend_init(void* buf,
                        size_t count,
                        ompi_datatype_t* datatype,
                        int dst,
                        int tag,
                        mca_pml_base_send_mode_t sendmode,
                        ompi_communicator_t* comm,
                        ompi_request_t** request)
{
    int ret;
    mca_pml_cm_send_request_t *sendreq;

    MCA_PML_CM_SEND_REQUEST_ALLOC(comm, dst, sendreq, ret);
    if (NULL == sendreq || OMPI_SUCCESS != ret) return ret;
    
    MCA_PML_CM_SEND_REQUEST_INIT(sendreq, buf, count,
                                 datatype, dst, tag, comm, 
                                 sendmode, false, true);
    *request = (ompi_request_t*) sendreq;

    return OMPI_SUCCESS;
}


int
mca_pml_cm_isend(void* buf,
                   size_t count,
                   ompi_datatype_t* datatype,
                   int dst,
                   int tag,
                   mca_pml_base_send_mode_t sendmode,
                   ompi_communicator_t* comm,
                   ompi_request_t** request)
{
    int ret;
    mca_pml_cm_send_request_t *sendreq;

    MCA_PML_CM_SEND_REQUEST_ALLOC(comm, dst, sendreq, ret);
    if (NULL == sendreq || OMPI_SUCCESS != ret) return ret;
    
    MCA_PML_CM_SEND_REQUEST_INIT(sendreq, buf, count,
                                 datatype, dst, tag, comm, 
                                 sendmode, false, false);

    MCA_PML_CM_SEND_REQUEST_START(sendreq, ret);

    if (OMPI_SUCCESS == ret) *request = (ompi_request_t*) sendreq;

    return ret;
}


int
mca_pml_cm_send(void *buf,
                size_t count,
                ompi_datatype_t* datatype,
                int dst,
                int tag,
                mca_pml_base_send_mode_t sendmode,
                ompi_communicator_t* comm)
{
    int ret;
    mca_pml_cm_send_request_t *sendreq;

    MCA_PML_CM_SEND_REQUEST_ALLOC(comm, dst, sendreq, ret);
    if (NULL == sendreq || OMPI_SUCCESS != ret) return ret;
    
    MCA_PML_CM_SEND_REQUEST_INIT(sendreq, buf, count,
                                 datatype, dst, tag, comm, 
                                 sendmode, true, false);

    MCA_PML_CM_SEND_REQUEST_START(sendreq, ret);
    if (OMPI_SUCCESS != ret) {
        MCA_PML_CM_SEND_REQUEST_RETURN(sendreq);
        return ret;
    }

    if (sendreq->req_send.req_base.req_ompi.req_complete == false) {
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

    ompi_request_free( (ompi_request_t**)&sendreq );

    return ret;
}

