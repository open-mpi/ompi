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
/**
 * @file
 */
#ifndef OMPI_PML_OB1_SEND_REQUEST_H
#define OMPI_PML_OB1_SEND_REQUEST_H

#include "mca/bmi/bmi.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "pml_ob1_proc.h"
#include "pml_ob1_comm.h"
#include "pml_ob1_hdr.h"
#include "datatype/convertor.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

typedef enum {
    MCA_PML_OB1_SR_INIT,
    MCA_PML_OB1_SR_START,
    MCA_PML_OB1_SR_SEND,
    MCA_PML_OB1_SR_PUT,
    MCA_PML_OB1_SR_COMPLETE
} mca_pml_ob1_send_request_state_t;


struct mca_pml_ob1_send_request_t {
    mca_pml_base_send_request_t req_send;
    mca_pml_ob1_proc_t* req_proc;
    mca_pml_ob1_endpoint_t* req_endpoint;
    mca_pml_ob1_send_request_state_t req_state;
    ompi_ptr_t req_recv;
    int32_t req_lock;
    size_t req_pending;
    size_t req_offset;
};
typedef struct mca_pml_ob1_send_request_t mca_pml_ob1_send_request_t;


OBJ_CLASS_DECLARATION(mca_pml_ob1_send_request_t);


#define MCA_PML_OB1_SEND_REQUEST_ALLOC(                                    \
    comm,                                                                  \
    dst,                                                                   \
    sendreq,                                                               \
    rc)                                                                    \
{                                                                          \
    mca_pml_ob1_proc_t *proc = comm->c_pml_procs[dst];                     \
    ompi_list_item_t* item;                                                \
                                                                           \
    if(NULL == proc) {                                                     \
        rc = OMPI_ERR_OUT_OF_RESOURCE;                                     \
    } else {                                                               \
        rc = OMPI_SUCCESS;                                                 \
        OMPI_FREE_LIST_WAIT(&mca_pml_ob1.send_requests, item, rc);         \
        sendreq = (mca_pml_ob1_send_request_t*)item;                       \
        sendreq->req_proc = proc;                                          \
    }                                                                      \
}


#define MCA_PML_OB1_SEND_REQUEST_INIT(                                     \
    sendreq,                                                               \
    buf,                                                                   \
    count,                                                                 \
    datatype,                                                              \
    dst,                                                                   \
    tag,                                                                   \
    comm,                                                                  \
    sendmode,                                                              \
    persistent)                                                            \
{                                                                          \
    sendreq->req_state = MCA_PML_OB1_SR_INIT;                              \
    MCA_PML_BASE_SEND_REQUEST_INIT(&sendreq->req_send,                     \
        buf,                                                               \
        count,                                                             \
        datatype,                                                          \
        dst,                                                               \
        tag,                                                               \
        comm,                                                              \
        sendmode,                                                          \
        persistent);                                                       \
}

/**
 * Start a send request. 
 */

#define MCA_PML_OB1_SEND_REQUEST_START(sendreq, rc)                                       \
{                                                                                         \
    mca_pml_ob1_endpoint_t* endpoint;                                                     \
    mca_pml_ob1_proc_t* proc = sendreq->req_proc;                                         \
                                                                                          \
    /* select next endpoint */                                                            \
    endpoint = mca_pml_ob1_ep_array_get_next(&proc->bmi_first);                           \
    sendreq->req_lock = 0;                                                                \
    sendreq->req_offset = 0;                                                              \
    sendreq->req_pending = 0;                                                             \
    sendreq->req_state = MCA_PML_OB1_SR_START;                                            \
    sendreq->req_send.req_base.req_ompi.req_complete = false;                             \
    sendreq->req_send.req_base.req_ompi.req_state = OMPI_REQUEST_ACTIVE;                  \
    sendreq->req_send.req_base.req_sequence = OMPI_THREAD_ADD32(&proc->proc_sequence,1);  \
    sendreq->req_endpoint = endpoint;                                                     \
                                                                                          \
    /* handle buffered send */                                                            \
    if(sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) {                   \
        mca_pml_base_bsend_request_start(&sendreq->req_send.req_base.req_ompi);           \
    }                                                                                     \
                                                                                          \
    if(NULL != endpoint->bmi_alloc) {                                                     \
        rc = mca_pml_ob1_send_request_start_copy(sendreq, endpoint);                      \
    } else {                                                                              \
        rc = mca_pml_ob1_send_request_start_user(sendreq, endpoint);                      \
    }                                                                                     \
}

/*
 * Advance a request
 */


#define MCA_PML_OB1_SEND_REQUEST_RETURN(sendreq)                     \
{                                                                    \
    /*  Let the base handle the reference counts */                  \
    MCA_PML_BASE_SEND_REQUEST_FINI((&sendreq->req_send));            \
    OMPI_FREE_LIST_RETURN(                                           \
        &mca_pml_ob1.send_requests, (ompi_list_item_t*)sendreq);     \
}
                                                                                                                      

/**
 *  BMI doesn't require pre-pinned or "specially" allocated memory.
 *  Can try to directly send from the users buffer if contigous.
 */

int mca_pml_ob1_send_request_start_user(
    mca_pml_ob1_send_request_t* sendreq,
    mca_pml_ob1_endpoint_t* endpoint);


/**
 *  BMI requires "specially" allocated memory. Request a segment that
 *  is used for initial hdr and any eager data.
 */

int mca_pml_ob1_send_request_start_copy(
    mca_pml_ob1_send_request_t* sendreq,
    mca_pml_ob1_endpoint_t* endpoint);

/**
 *
 */
int mca_pml_ob1_send_request_schedule(
    mca_pml_ob1_send_request_t* sendreq);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

