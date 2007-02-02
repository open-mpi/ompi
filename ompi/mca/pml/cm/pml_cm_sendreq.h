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

#include "pml_cm_request.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/mtl/mtl.h"

struct mca_pml_cm_send_request_t { 
    mca_pml_cm_request_t req_base;
    mca_pml_base_send_mode_t req_send_mode;
    struct ompi_communicator_t *req_comm; /**< communicator pointer */
    struct ompi_datatype_t *req_datatype; /**< pointer to data type */
};
typedef struct mca_pml_cm_send_request_t mca_pml_cm_send_request_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_pml_cm_send_request_t);


struct mca_pml_cm_thin_send_request_t { 
    mca_pml_cm_send_request_t req_send;
    mca_mtl_request_t req_mtl;            /**< the mtl specific memory */
    
};
typedef struct mca_pml_cm_thin_send_request_t mca_pml_cm_thin_send_request_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_pml_cm_thin_send_request_t);


struct mca_pml_cm_hvy_send_request_t {
    mca_pml_cm_send_request_t req_send;
    void *req_addr;                       /**< pointer to application buffer */
    size_t req_count;                     /**< count of user datatype elements */
    int32_t req_peer;                     /**< peer process - rank w/in this communicator */
    int32_t req_tag;                      /**< user defined tag */
    void *req_buff;                  /**< pointer to send buffer - may not be application buffer */
    bool req_blocking;
    mca_mtl_request_t req_mtl;            /**< the mtl specific memory */
};
typedef struct mca_pml_cm_hvy_send_request_t mca_pml_cm_hvy_send_request_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_pml_cm_hvy_send_request_t);


#define MCA_PML_CM_THIN_SEND_REQUEST_ALLOC(sendreq, comm, dst,          \
                                           ompi_proc, rc)               \
{                                                                       \
    do{                                                                 \
        ompi_free_list_item_t* item;                                    \
        ompi_proc = ompi_comm_peer_lookup( comm, dst );                 \
                                                                        \
        if(NULL == ompi_proc) {                                         \
            rc = OMPI_ERR_OUT_OF_RESOURCE;                              \
            sendreq = NULL;                                             \
        } else {                                                        \
            rc = OMPI_SUCCESS;                                          \
        OMPI_FREE_LIST_WAIT(&ompi_pml_cm.cm_thin_send_requests,         \
                            item, rc);                                  \
        sendreq = (mca_pml_cm_thin_send_request_t*)item;                \
        }                                                               \
    }while(0);                                                          \
}


#define MCA_PML_CM_HVY_SEND_REQUEST_ALLOC(sendreq, comm, dst,           \
                                          ompi_proc, rc)                \
{                                                                       \
    ompi_free_list_item_t* item;                                        \
    ompi_proc = ompi_comm_peer_lookup( comm, dst );                     \
    if(NULL == ompi_proc) {                                             \
        rc = OMPI_ERR_OUT_OF_RESOURCE;                                  \
        sendreq = NULL;                                                 \
    } else {                                                            \
        rc = OMPI_SUCCESS;                                              \
        OMPI_FREE_LIST_WAIT(&ompi_pml_cm.cm_hvy_send_requests,          \
                            item, rc);                                  \
        sendreq = (mca_pml_cm_hvy_send_request_t*)item;                 \
    }                                                                   \
}


#define MCA_PML_CM_SEND_REQUEST_INIT_COMMON(req_send,                   \
                                            ompi_proc,                  \
                                            comm,                       \
                                            tag,                        \
                                            datatype,                   \
                                            sendmode,                   \
                                            buf,                        \
                                            count)                      \
{                                                                       \
    OBJ_RETAIN(comm);                                                   \
    OBJ_RETAIN(datatype);                                               \
    req_send->req_comm = comm;                                          \
    req_send->req_datatype = datatype;                                  \
    ompi_convertor_copy_and_prepare_for_send(                           \
                                             ompi_proc->proc_convertor, \
                                             datatype,                  \
                                             count,                     \
                                             buf,                       \
                                             0,                         \
                                             &req_send->req_base.req_convertor ); \
    req_send->req_base.req_ompi.req_mpi_object.comm = comm;             \
    req_send->req_base.req_ompi.req_status.MPI_SOURCE =                  \
        comm->c_my_rank;                                                \
    req_send->req_base.req_ompi.req_status.MPI_TAG = tag;                \
    req_send->req_base.req_ompi.req_status._count = count;               \
    req_send->req_send_mode = sendmode;                                  \
    req_send->req_base.req_free_called = false;                          \
}

#define MCA_PML_CM_HVY_SEND_REQUEST_INIT( sendreq,                      \
                                          ompi_proc,                    \
                                          comm,                         \
                                          tag,                          \
                                          dst,                          \
                                          datatype,                     \
                                          sendmode,                     \
                                          persistent,                   \
                                          blocking,                     \
                                          buf,                          \
                                          count)                        \
{                                                                       \
    do {                                                                \
        OMPI_REQUEST_INIT(&(sendreq->req_send.req_base.req_ompi),       \
                          persistent);                                  \
        sendreq->req_tag = tag;                                         \
        sendreq->req_peer = dst;                                        \
        sendreq->req_addr = buf;                                        \
        sendreq->req_count = count;                                     \
        MCA_PML_CM_SEND_REQUEST_INIT_COMMON(                            \
                                            (&sendreq->req_send),       \
                                            ompi_proc,                  \
                                            comm,                       \
                                            tag,                        \
                                            datatype,                   \
                                            sendmode,                   \
                                            buf,                        \
                                            count);                     \
        ompi_convertor_get_packed_size(                                 \
                                       &sendreq->req_send.req_base.req_convertor, \
                                       &sendreq->req_count  );          \
                                                                        \
                                                                        \
        sendreq->req_blocking = blocking;                               \
        sendreq->req_send.req_base.req_pml_complete =                   \
            (persistent ? true:false);                                  \
    }while(0);                                                          \
}



#define MCA_PML_CM_THIN_SEND_REQUEST_INIT( sendreq,                     \
                                           ompi_proc,                   \
                                           comm,                        \
                                           tag,                         \
                                           dst,                         \
                                           datatype,                    \
                                           sendmode,                    \
                                           buf,                         \
                                           count)                       \
{                                                                       \
    do {                                                                \
        OMPI_REQUEST_INIT(&(sendreq->req_send.req_base.req_ompi),       \
                          false);                                       \
        MCA_PML_CM_SEND_REQUEST_INIT_COMMON(                       \
                                            (&sendreq->req_send),       \
                                            ompi_proc,                  \
                                            comm,                       \
                                            tag,                        \
                                            datatype,                   \
                                            sendmode,                   \
                                            buf,                        \
                                            count);                     \
        sendreq->req_send.req_base.req_pml_complete = false;            \
    }while(0);                                                          \
}


#define MCA_PML_CM_SEND_REQUEST_START_SETUP(req_send)                   \
    do {                                                                \
                                                                        \
    req_send->req_base.req_pml_complete = false;                        \
    req_send->req_base.req_ompi.req_complete = false;                   \
    req_send->req_base.req_ompi.req_state =                             \
        OMPI_REQUEST_ACTIVE;                                            \
    req_send->req_base.req_ompi.req_status._cancelled = 0;              \
                                                                        \
} while (0)


#define MCA_PML_CM_THIN_SEND_REQUEST_START(sendreq,                     \
                                           comm,                        \
                                           tag,                         \
                                           dst,                         \
                                           sendmode,                    \
                                           blocking,                    \
                                           ret)                         \
do {                                                                    \
    MCA_PML_CM_SEND_REQUEST_START_SETUP((&sendreq->req_send));          \
    ret = OMPI_MTL_CALL(isend(ompi_mtl,                                 \
                              comm,                                     \
                              dst,                                      \
                              tag,                                      \
                              &sendreq->req_send.req_base.req_convertor, \
                              sendmode,                                 \
                              blocking,                                 \
                              &sendreq->req_mtl));                      \
 } while (0)

#define MCA_PML_CM_HVY_SEND_REQUEST_BSEND_ALLOC(sendreq, ret)           \
do {                                                                    \
    struct iovec iov;                                                   \
    unsigned int iov_count;                                             \
    size_t max_data;                                                    \
                                                                        \
    if(sendreq->req_count > 0) {                                        \
        sendreq->req_buff =                                             \
            mca_pml_base_bsend_request_alloc_buf(sendreq->req_count);   \
        if (NULL == sendreq->req_buff) {                                \
            ret = MPI_ERR_BUFFER;                                       \
        } else {                                                        \
            iov.iov_base = (IOVBASE_TYPE*)sendreq->req_buff;            \
            max_data = iov.iov_len = sendreq->req_count;                \
            iov_count = 1;                                              \
            ompi_convertor_pack( &sendreq->req_send.req_base.req_convertor, \
                                 &iov,                                  \
                                 &iov_count,                            \
                                 &max_data );                           \
            ompi_convertor_prepare_for_send( &sendreq->req_send.req_base.req_convertor, MPI_PACKED, \
                                             max_data, sendreq->req_buff ); \
        }                                                               \
    }                                                                   \
 } while(0);
        

#define MCA_PML_CM_HVY_SEND_REQUEST_START(sendreq, ret)                 \
do {                                                                    \
    ret = OMPI_SUCCESS;                                                 \
    MCA_PML_CM_SEND_REQUEST_START_SETUP((&sendreq->req_send));          \
    if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) {  \
        MCA_PML_CM_HVY_SEND_REQUEST_BSEND_ALLOC(sendreq, ret);          \
    }                                                                   \
    if (OMPI_SUCCESS == ret) {                                          \
        ret = OMPI_MTL_CALL(isend(ompi_mtl,                             \
                                  sendreq->req_send.req_comm,           \
                                  sendreq->req_peer,                    \
                                  sendreq->req_tag,                     \
                                  &sendreq->req_send.req_base.req_convertor, \
                                  sendreq->req_send.req_send_mode,      \
                                  sendreq->req_blocking,                \
                                  &sendreq->req_mtl));                  \
        if(OMPI_SUCCESS == ret &&                                       \
           sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) { \
            sendreq->req_send.req_base.req_ompi.req_status.MPI_ERROR = 0; \
            MCA_PML_BASE_REQUEST_MPI_COMPLETE(&(sendreq->req_send.req_base.req_ompi)); \
        }                                                               \
    }                                                                   \
 } while (0)

/*
 * The PML has completed a send request. Note that this request
 * may have been orphaned by the user or have already completed
 * at the MPI level. 
 * This macro will never be called directly from the upper level, as it should
 * only be an internal call to the PML.
 */
#define MCA_PML_CM_HVY_SEND_REQUEST_PML_COMPLETE(sendreq)               \
do {                                                                    \
    assert( false == sendreq->req_send.req_base.req_pml_complete );     \
                                                                        \
    if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED && \
        sendreq->req_count > 0 ) {                                      \
        mca_pml_base_bsend_request_free(sendreq->req_buff);             \
    }                                                                   \
                                                                        \
    OPAL_THREAD_LOCK(&ompi_request_lock);                               \
    if( false == sendreq->req_send.req_base.req_ompi.req_complete ) {   \
        /* Should only be called for long messages (maybe synchronous) */ \
        MCA_PML_BASE_REQUEST_MPI_COMPLETE(&(sendreq->req_send.req_base.req_ompi)); \
    }                                                                   \
    sendreq->req_send.req_base.req_pml_complete = true;                 \
                                                                        \
    if( sendreq->req_send.req_base.req_free_called ) {                           \
        MCA_PML_CM_HVY_SEND_REQUEST_RETURN( sendreq );                  \
    } else {                                                            \
        if(sendreq->req_send.req_base.req_ompi.req_persistent) {                 \
            /* rewind convertor */                                      \
            size_t offset = 0;                                          \
            ompi_convertor_set_position(&sendreq->req_send.req_base.req_convertor, \
                                        &offset);                       \
        }                                                               \
    }                                                                   \
    OPAL_THREAD_UNLOCK(&ompi_request_lock);                             \
 } while (0)
    
    
/*
 * Release resources associated with a request
 */
#define MCA_PML_CM_HVY_SEND_REQUEST_RETURN(sendreq)                     \
    {                                                                   \
        /*  Let the base handle the reference counts */                 \
        OBJ_RELEASE(sendreq->req_send.req_datatype);                    \
        OBJ_RELEASE(sendreq->req_send.req_comm);                        \
        OMPI_REQUEST_FINI(&sendreq->req_send.req_base.req_ompi);        \
        ompi_convertor_cleanup( &(sendreq->req_send.req_base.req_convertor) ); \
        OMPI_FREE_LIST_RETURN(                                          \
                              &ompi_pml_cm.cm_hvy_send_requests,        \
                              (ompi_free_list_item_t*)sendreq);         \
    }

/*
 * The PML has completed a send request. Note that this request
 * may have been orphaned by the user or have already completed
 * at the MPI level. 
 * This macro will never be called directly from the upper level, as it should
 * only be an internal call to the PML.
 */
#define MCA_PML_CM_THIN_SEND_REQUEST_PML_COMPLETE(sendreq)              \
do {                                                                    \
    assert( false == sendreq->req_send.req_base.req_pml_complete );              \
                                                                        \
    OPAL_THREAD_LOCK(&ompi_request_lock);                               \
    if( false == sendreq->req_send.req_base.req_ompi.req_complete ) {   \
        /* Should only be called for long messages (maybe synchronous) */ \
        MCA_PML_BASE_REQUEST_MPI_COMPLETE(&(sendreq->req_send.req_base.req_ompi)); \
    }                                                                   \
    sendreq->req_send.req_base.req_pml_complete = true;                 \
                                                                        \
    if( sendreq->req_send.req_base.req_free_called ) {                  \
        MCA_PML_CM_THIN_SEND_REQUEST_RETURN( sendreq );                 \
    }                                                                   \
    OPAL_THREAD_UNLOCK(&ompi_request_lock);                             \
 } while (0)
    
    
/*
 * Release resources associated with a request
 */
#define MCA_PML_CM_THIN_SEND_REQUEST_RETURN(sendreq)                    \
    {                                                                   \
        /*  Let the base handle the reference counts */                 \
        OBJ_RELEASE(sendreq->req_send.req_datatype);                    \
        OBJ_RELEASE(sendreq->req_send.req_comm);                        \
        OMPI_REQUEST_FINI(&sendreq->req_send.req_base.req_ompi);        \
        ompi_convertor_cleanup( &(sendreq->req_send.req_base.req_convertor) ); \
        OMPI_FREE_LIST_RETURN(                                          \
                              &ompi_pml_cm.cm_thin_send_requests,       \
                              (ompi_free_list_item_t*)sendreq);         \
    }

#endif
