/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_CM_RECVREQ_H
#define PML_CM_RECVREQ_H

#include "ompi/mca/pml/base/pml_base_recvreq.h"
#include "ompi/mca/mtl/mtl.h"

struct mca_pml_cm_recv_request_t {
    mca_pml_base_recv_request_t req_recv;
    mca_mtl_request_t req_mtl;
};
typedef struct mca_pml_cm_recv_request_t mca_pml_cm_recv_request_t;
OBJ_CLASS_DECLARATION(mca_pml_cm_recv_request_t);


/**
 *  Allocate a recv request from the modules free list.
 *
 *  @param rc (OUT)  OMPI_SUCCESS or error status on failure.
 *  @return          Receive request.
 */
#define MCA_PML_CM_RECV_REQUEST_ALLOC(recvreq, rc)                   \
do {                                                                 \
    ompi_free_list_item_t*item;                                      \
    OMPI_FREE_LIST_GET(&ompi_pml_cm.cm_recv_requests, item, rc);     \
    recvreq = (mca_pml_cm_recv_request_t*) item;                     \
 } while (0)


/**
 * Initialize a receive request with call parameters.
 *
 * @param request (IN)       Receive request.
 * @param addr (IN)          User buffer.
 * @param count (IN)         Number of elements of indicated datatype.
 * @param datatype (IN)      User defined datatype.
 * @param src (IN)           Source rank w/in the communicator.
 * @param tag (IN)           User defined tag.
 * @param comm (IN)          Communicator.
 * @param persistent (IN)    Is this a ersistent request.
 */
#define MCA_PML_CM_RECV_REQUEST_INIT( request,                          \
                                      addr,                             \
                                      count,                            \
                                      datatype,                         \
                                      src,                              \
                                      tag,                              \
                                      comm,                             \
                                      persistent)                       \
do {                                                                    \
    MCA_PML_BASE_RECV_REQUEST_INIT( &(request)->req_recv,               \
                                    addr,                               \
                                    count,                              \
                                    datatype,                           \
                                    src,                                \
                                    tag,                                \
                                    comm,                               \
                                    persistent);                        \
    /* BWB - fix me - need real remote proc */                          \
    if (MPI_ANY_SOURCE == src) {                                        \
        (request)->req_recv.req_base.req_proc =                         \
            comm->c_pml_procs[comm->c_my_rank]->proc_ompi;              \
    } else {                                                            \
        (request)->req_recv.req_base.req_proc =                         \
            comm->c_pml_procs[src]->proc_ompi;                          \
    }                                                                   \
                                                                        \
    ompi_convertor_copy_and_prepare_for_recv(                           \
             (request)->req_recv.req_base.req_proc->proc_convertor,     \
             (request)->req_recv.req_base.req_datatype,                 \
             (request)->req_recv.req_base.req_count,                    \
             (request)->req_recv.req_base.req_addr,                     \
             0,                                                         \
             &(request)->req_recv.req_convertor );                      \
} while(0)


/**
 * Start an initialized request.
 *
 * @param request  Receive request.
 * @return         OMPI_SUCESS or error status on failure.
 */
#define MCA_PML_CM_RECV_REQUEST_START(request, ret)                     \
do {                                                                    \
    /* init/re-init the request */                                      \
    MCA_PML_BASE_RECV_START( &(request)->req_recv.req_base );           \
    ret = OMPI_MTL_CALL(irecv(ompi_mtl,                                 \
                             recvreq->req_recv.req_base.req_comm,       \
                             recvreq->req_recv.req_base.req_peer,       \
                             recvreq->req_recv.req_base.req_tag,        \
                             &recvreq->req_recv.req_convertor,          \
                             &recvreq->req_mtl));                       \
} while (0)


/**
 * Mark the request as completed at MPI level for internal purposes.
 *
 *  @param recvreq (IN)  Receive request.
 */
#define MCA_PML_CM_RECV_REQUEST_MPI_COMPLETE( recvreq )                 \
do {                                                                    \
    MCA_PML_BASE_REQUEST_MPI_COMPLETE( &(recvreq->req_recv.req_base.req_ompi) ); \
 } while (0)


/**
 *  Return a recv request to the modules free list.
 *
 *  @param recvreq (IN)  Receive request.
 */
#define MCA_PML_CM_RECV_REQUEST_PML_COMPLETE(recvreq)                   \
do {                                                                    \
    assert( false == recvreq->req_recv.req_base.req_pml_complete );     \
                                                                        \
    OPAL_THREAD_LOCK(&ompi_request_lock);                               \
                                                                        \
    if( true == recvreq->req_recv.req_base.req_free_called ) {          \
        MCA_PML_CM_RECV_REQUEST_RETURN( recvreq );                      \
    } else {                                                            \
        /* initialize request status */                                 \
        if(recvreq->req_recv.req_base.req_ompi.req_persistent) {        \
            /* rewind convertor */                                      \
            size_t offset = 0;                                          \
            ompi_convertor_set_position(&recvreq->req_recv.req_convertor, &offset); \
        }                                                               \
        recvreq->req_recv.req_base.req_pml_complete = true;             \
        MCA_PML_CM_RECV_REQUEST_MPI_COMPLETE( recvreq );                \
    }                                                                   \
    OPAL_THREAD_UNLOCK(&ompi_request_lock);                             \
 } while(0)


/**
 *  Free the PML receive request
 */
#define MCA_PML_CM_RECV_REQUEST_RETURN(recvreq)                \
{                                                              \
    MCA_PML_BASE_RECV_REQUEST_FINI(&(recvreq)->req_recv);      \
    OMPI_FREE_LIST_RETURN( &ompi_pml_cm.cm_recv_requests,      \
                           (ompi_free_list_item_t*)(recvreq)); \
}


#endif
