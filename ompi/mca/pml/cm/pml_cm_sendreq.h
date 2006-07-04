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

#ifndef PML_CM_SENDREQ_H
#define PML_CM_SENDREQ_H

#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/mtl/mtl.h"

struct mca_pml_cm_send_request_t {
    mca_pml_base_send_request_t req_send;
    mca_mtl_request_t req_mtl;
    bool req_blocking;
};
typedef struct mca_pml_cm_send_request_t mca_pml_cm_send_request_t;
OBJ_CLASS_DECLARATION(mca_pml_cm_send_request_t);


#define MCA_PML_CM_SEND_REQUEST_ALLOC(comm, dst, sendreq, rc)           \
{                                                                       \
    ompi_proc_t *proc =                                                 \
        comm->c_pml_procs[dst]->proc_ompi;                              \
    ompi_free_list_item_t* item;                                        \
                                                                        \
    if(NULL == proc) {                                                  \
        rc = OMPI_ERR_OUT_OF_RESOURCE;                                  \
    } else {                                                            \
        rc = OMPI_SUCCESS;                                              \
        OMPI_FREE_LIST_WAIT(&ompi_pml_cm.cm_send_requests, item, rc);   \
        sendreq = (mca_pml_cm_send_request_t*)item;                     \
        sendreq->req_send.req_base.req_proc = proc;                     \
    }                                                                   \
}


#define MCA_PML_CM_SEND_REQUEST_INIT( sendreq,                          \
                                       buf,                             \
                                       count,                           \
                                       datatype,                        \
                                       dst,                             \
                                       tag,                             \
                                       comm,                            \
                                       sendmode,                        \
                                       blocking,                        \
                                       persistent)                      \
{                                                                       \
    MCA_PML_BASE_SEND_REQUEST_INIT(&sendreq->req_send,                  \
                                   buf,                                 \
                                   count,                               \
                                   datatype,                            \
                                   dst,                                 \
                                   tag,                                 \
                                   comm,                                \
                                   sendmode,                            \
                                   persistent);                         \
    /* BWB - XXX - fix me later */                                      \
    if (count == 0) {                                                   \
        ompi_convertor_copy_and_prepare_for_send(                       \
                          (sendreq)->req_send.req_base.req_proc->proc_convertor, \
                          (sendreq)->req_send.req_base.req_datatype,    \
                          (sendreq)->req_send.req_base.req_count,       \
                          (sendreq)->req_send.req_base.req_addr,        \
                          0,                                            \
                          &(sendreq)->req_send.req_convertor );         \
        ompi_convertor_get_packed_size( &(sendreq)->req_send.req_convertor, \
                          &((sendreq)->req_send.req_bytes_packed) );    \
    }                                                                   \
                                                                        \
    sendreq->req_blocking = blocking;                                   \
}


#define MCA_PML_CM_SEND_REQUEST_START(sendreq, ret)                     \
do {                                                                    \
    MCA_PML_BASE_SEND_START( &sendreq->req_send.req_base );             \
    ret = OMPI_SUCCESS;                                                 \
    if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) { \
        ret = mca_pml_base_bsend_request_alloc(&sendreq->req_send.req_base.req_ompi); \
        if (OMPI_SUCCESS == ret) {                                      \
            ret =mca_pml_base_bsend_request_start(&sendreq->req_send.req_base.req_ompi); \
        }                                                               \
    }                                                                   \
    if (OMPI_SUCCESS == ret) {                                          \
        ret = OMPI_MTL_CALL(isend(ompi_mtl,                             \
                                  sendreq->req_send.req_base.req_comm,  \
                                  sendreq->req_send.req_base.req_peer,  \
                                  sendreq->req_send.req_base.req_tag,   \
                                  &sendreq->req_send.req_convertor,     \
                                  sendreq->req_send.req_send_mode,      \
                                  sendreq->req_blocking,                \
                                  &sendreq->req_mtl));                  \
    }                                                                   \
 } while (0)


/*
 * Mark a send request as completed at the MPI level.
 */
#define MCA_PML_CM_SEND_REQUEST_MPI_COMPLETE(sendreq)                   \
do {                                                                    \
    (sendreq)->req_send.req_base.req_ompi.req_status.MPI_SOURCE =       \
        (sendreq)->req_send.req_base.req_comm->c_my_rank;               \
    (sendreq)->req_send.req_base.req_ompi.req_status.MPI_TAG =          \
        (sendreq)->req_send.req_base.req_tag;                           \
    (sendreq)->req_send.req_base.req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS; \
    (sendreq)->req_send.req_base.req_ompi.req_status._count =           \
        (sendreq)->req_send.req_bytes_packed;                           \
    MCA_PML_BASE_REQUEST_MPI_COMPLETE( &((sendreq)->req_send.req_base.req_ompi) ); \
 } while(0)


/*
 * The PML has completed a send request. Note that this request
 * may have been orphaned by the user or have already completed
 * at the MPI level. 
 * This macro will never be called directly from the upper level, as it should
 * only be an internal call to the PML.
 */
#define MCA_PML_CM_SEND_REQUEST_PML_COMPLETE(sendreq)                   \
do {                                                                    \
        assert( false == sendreq->req_send.req_base.req_pml_complete ); \
                                                                        \
        if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) { \
            mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq);  \
        }                                                               \
                                                                        \
        OPAL_THREAD_LOCK(&ompi_request_lock);                           \
        if( false == sendreq->req_send.req_base.req_ompi.req_complete ) { \
            /* Should only be called for long messages (maybe synchronous) */ \
            MCA_PML_CM_SEND_REQUEST_MPI_COMPLETE(sendreq);              \
        }                                                               \
        sendreq->req_send.req_base.req_pml_complete = true;             \
                                                                        \
        if( sendreq->req_send.req_base.req_free_called ) {              \
            MCA_PML_CM_SEND_REQUEST_RETURN( sendreq );                  \
        } else {                                                        \
            if(sendreq->req_send.req_base.req_ompi.req_persistent) {    \
                /* rewind convertor */                                  \
                size_t offset = 0;                                      \
                ompi_convertor_set_position(&sendreq->req_send.req_convertor, &offset); \
            }                                                           \
        }                                                               \
        OPAL_THREAD_UNLOCK(&ompi_request_lock);                         \
    } while (0)


/*
 * Release resources associated with a request
 */
#define MCA_PML_CM_SEND_REQUEST_RETURN(sendreq)                          \
{                                                                        \
    /*  Let the base handle the reference counts */                      \
    MCA_PML_BASE_SEND_REQUEST_FINI((&(sendreq)->req_send));              \
    OMPI_FREE_LIST_RETURN(                                               \
        &ompi_pml_cm.cm_send_requests, (ompi_free_list_item_t*)sendreq); \
}

#endif
