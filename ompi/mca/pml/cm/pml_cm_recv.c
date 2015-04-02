/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"

#include "opal/prefetch.h"

#include "ompi/request/request.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/message/message.h"

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
    mca_pml_cm_hvy_recv_request_t *recvreq;
    ompi_proc_t* ompi_proc;
    
    MCA_PML_CM_HVY_RECV_REQUEST_ALLOC(recvreq);
    if( OPAL_UNLIKELY(NULL == recvreq) ) return OMPI_ERR_OUT_OF_RESOURCE;
    
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
    
    MCA_PML_CM_THIN_RECV_REQUEST_ALLOC(recvreq);
    if( OPAL_UNLIKELY(NULL == recvreq) ) return OMPI_ERR_OUT_OF_RESOURCE;
    
    MCA_PML_CM_THIN_RECV_REQUEST_INIT(recvreq,
                                      ompi_proc,
                                      comm,
                                      src,
                                      datatype,
                                      addr,
                                      count);
    
    MCA_PML_CM_THIN_RECV_REQUEST_START(recvreq, comm, tag, src, ret);

    if( OPAL_LIKELY(OMPI_SUCCESS == ret) ) *request = (ompi_request_t*) recvreq;

    return ret;
}


void mca_pml_cm_recv_fast_completion(struct mca_mtl_request_t *mtl_request)
{
    // Do nothing!
    ompi_request_complete(mtl_request->ompi_req, true);
    return;
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
    ompi_proc_t *ompi_proc;
    opal_convertor_t convertor;
    mca_pml_cm_request_t req;
    mca_mtl_request_t *req_mtl =
            alloca(sizeof(mca_mtl_request_t) + ompi_mtl->mtl_request_size);

    req_mtl->ompi_req = &req.req_ompi;
    req_mtl->completion_callback = mca_pml_cm_recv_fast_completion;

    req.req_pml_type = MCA_PML_CM_REQUEST_RECV_THIN;
    req.req_free_called = false;
    req.req_ompi.req_complete = false;
    req.req_ompi.req_complete_cb = NULL;
    req.req_ompi.req_state = OMPI_REQUEST_ACTIVE;
    req.req_ompi.req_status.MPI_TAG = OMPI_ANY_TAG;
    req.req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;
    req.req_ompi.req_status._cancelled = 0;

    if( MPI_ANY_SOURCE == src ) {
        ompi_proc = ompi_proc_local_proc;
    } else {
        ompi_proc = ompi_comm_peer_lookup( comm, src );
    }

    opal_convertor_copy_and_prepare_for_recv(
                                  ompi_proc->super.proc_convertor,
                                  &(datatype->super),
                                  count,
                                  addr,
                                  0,
                                  &convertor );
    ret = OMPI_MTL_CALL(irecv(ompi_mtl,
                              comm,
                              src,
                              tag,
                              &convertor,
                              req_mtl));
    if( OPAL_UNLIKELY(OMPI_SUCCESS != ret) ) {
        return ret;
    }

    ompi_request_wait_completion(&req.req_ompi);

    if (NULL != status) {  /* return status */
        *status = req.req_ompi.req_status;
    }
    ret = req.req_ompi.req_status.MPI_ERROR;

    return ret;
}


int
mca_pml_cm_imrecv(void *buf,
                  size_t count,
                  ompi_datatype_t *datatype,
                  struct ompi_message_t **message,
                  struct ompi_request_t **request)
{
    int ret;
    mca_pml_cm_thin_recv_request_t *recvreq;
    ompi_proc_t* ompi_proc;
    ompi_communicator_t *comm = (*message)->comm;
    int peer = (*message)->peer;

    MCA_PML_CM_THIN_RECV_REQUEST_ALLOC(recvreq);
    if( OPAL_UNLIKELY(NULL == recvreq) ) return OMPI_ERR_OUT_OF_RESOURCE;
    
    MCA_PML_CM_THIN_RECV_REQUEST_INIT(recvreq,
                                      ompi_proc,
                                      comm,
                                      peer,
                                      datatype,
                                      buf,
                                      count);
    
    MCA_PML_CM_THIN_RECV_REQUEST_MATCHED_START(recvreq, message, ret);

    if( OPAL_LIKELY(OMPI_SUCCESS == ret) ) *request = (ompi_request_t*) recvreq;

    return ret;
}


int
mca_pml_cm_mrecv(void *buf,
                 size_t count,
                 ompi_datatype_t *datatype,
                 struct ompi_message_t **message,
                 ompi_status_public_t* status)
{
    int ret;
    mca_pml_cm_thin_recv_request_t *recvreq;
    ompi_proc_t* ompi_proc;
    ompi_communicator_t *comm = (*message)->comm;
    int peer = (*message)->peer;

    MCA_PML_CM_THIN_RECV_REQUEST_ALLOC(recvreq);
    if( OPAL_UNLIKELY(NULL == recvreq) ) return OMPI_ERR_OUT_OF_RESOURCE;

    MCA_PML_CM_THIN_RECV_REQUEST_INIT(recvreq,
                                      ompi_proc,
                                      comm, 
                                      peer,
                                      datatype,
                                      buf,
                                      count);
    
    MCA_PML_CM_THIN_RECV_REQUEST_MATCHED_START(recvreq, 
                                               message, ret);
    if( OPAL_UNLIKELY(OMPI_SUCCESS != ret) ) {
        MCA_PML_CM_THIN_RECV_REQUEST_RETURN(recvreq);
        return ret;
    }

    ompi_request_wait_completion(&recvreq->req_base.req_ompi);

    if (NULL != status) {  /* return status */
        *status = recvreq->req_base.req_ompi.req_status;
    }
    ret = recvreq->req_base.req_ompi.req_status.MPI_ERROR;
    ompi_request_free( (ompi_request_t**)&recvreq );

    return ret;
}
