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

#include "mca/btl/btl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/mpool/base/base.h"
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
    MCA_PML_OB1_SR_ACKED,
    MCA_PML_OB1_SR_COMPLETE
} mca_pml_ob1_send_request_state_t;


struct mca_pml_ob1_send_request_t {
    mca_pml_base_send_request_t req_send;
    mca_pml_ob1_proc_t* req_proc;
    mca_pml_ob1_endpoint_t* req_endpoint;
    mca_pml_ob1_send_request_state_t req_state;
    struct mca_mpool_base_chunk_t* req_chunk;
    ompi_ptr_t req_recv;
    int32_t req_lock;
    size_t req_pipeline_depth;
    size_t req_bytes_delivered;
    size_t req_send_offset;
    size_t req_rdma_offset;

#if MCA_PML_OB1_TIMESTAMPS
    unsigned long long t_start;
    unsigned long long t_send1;
    unsigned long long t_send2;
    unsigned long long t_scheduled;
    unsigned long long t_pin[MCA_PML_OB1_NUM_TSTAMPS];
    unsigned long long t_put[MCA_PML_OB1_NUM_TSTAMPS];
    unsigned long long t_fin[MCA_PML_OB1_NUM_TSTAMPS];
    int t_pin_index;
    int t_put_index;
    int t_fin_index;
#endif
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
    opal_list_item_t* item;                                                \
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
 *  Diagnostic output to trace rdma protocol timing
 */

#if MCA_PML_OB1_TIMESTAMPS
#define MCA_PML_OB1_SEND_REQUEST_TSTAMPS_DUMP(sendreq) \
{ \
 int i; \
 ompi_output(0, "[%d,%d,%d] src start, %llu\n",  \
    ORTE_NAME_ARGS(orte_process_info.my_name), (sendreq)->t_start); \
\
 ompi_output(0, "[%d,%d,%d] src send start, %llu\n",  \
    ORTE_NAME_ARGS(orte_process_info.my_name), (sendreq)->t_send1); \
\
 ompi_output(0, "[%d,%d,%d] src scheduled, %llu\n",  \
    ORTE_NAME_ARGS(orte_process_info.my_name), (sendreq)->t_scheduled); \
\
 ompi_output(0, "[%d,%d,%d] src send complete, %llu\n",  \
    ORTE_NAME_ARGS(orte_process_info.my_name), (sendreq)->t_send2); \
\
 for(i=0; i<(sendreq)->t_pin_index; i++) \
     ompi_output(0, "[%d,%d,%d] src pin, %llu %llu\n",  \
        ORTE_NAME_ARGS(orte_process_info.my_name), (sendreq)->t_pin[i], \
        (sendreq)->t_put[i] - (sendreq)->t_pin[i]); \
 for(i=0; i<(sendreq)->t_put_index; i++) \
     ompi_output(0, "[%d,%d,%d] src put, %llu %llu\n",  \
        ORTE_NAME_ARGS(orte_process_info.my_name), (sendreq)->t_put[i], \
        (sendreq)->t_fin[i] - (sendreq)->t_put[i]); \
 for(i=0; i<(sendreq)->t_fin_index; i++) \
     ompi_output(0, "[%d,%d,%d] src fin, %llu\n",  \
        ORTE_NAME_ARGS(orte_process_info.my_name), (sendreq)->t_fin[i]); \
}

#define MCA_PML_OB1_SEND_REQUEST_TSTAMPS_INIT(sendreq) \
{                                                      \
    sendreq->t_pin_index = 0;                          \
    sendreq->t_put_index = 0;                          \
    sendreq->t_fin_index = 0;                          \
}

#else
#define MCA_PML_OB1_SEND_REQUEST_TSTAMPS_DUMP(sendreq)
#define MCA_PML_OB1_SEND_REQUEST_TSTAMPS_INIT(sendreq)
#endif


/**
 * Start a send request. 
 */

#define MCA_PML_OB1_SEND_REQUEST_START(sendreq, rc)                                       \
{                                                                                         \
    mca_pml_ob1_endpoint_t* endpoint;                                                     \
    mca_pml_ob1_proc_t* proc = sendreq->req_proc;                                         \
                                                                                          \
    /* select next endpoint */                                                            \
    endpoint = mca_pml_ob1_ep_array_get_next(&proc->btl_eager);                           \
    sendreq->req_lock = 0;                                                                \
    MCA_PML_OB1_SEND_REQUEST_TSTAMPS_INIT(sendreq);                                       \
    sendreq->req_pipeline_depth = 0;                                                      \
    sendreq->req_bytes_delivered = 0;                                                     \
    sendreq->req_chunk = NULL;                                                            \
    sendreq->req_send_offset = 0;                                                         \
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
    rc = mca_pml_ob1_send_request_start(sendreq, endpoint);                               \
}


/*
 * Complete a send request
 */
#define MCA_PML_OB1_SEND_REQUEST_COMPLETE(sendreq)                                        \
{                                                                                         \
    (sendreq)->req_send.req_base.req_pml_complete = true;                                 \
    if ((sendreq)->req_send.req_base.req_ompi.req_complete == false) {                    \
        (sendreq)->req_send.req_base.req_ompi.req_status.MPI_SOURCE =                     \
            (sendreq)->req_send.req_base.req_comm->c_my_rank;                             \
        (sendreq)->req_send.req_base.req_ompi.req_status.MPI_TAG =                        \
            (sendreq)->req_send.req_base.req_tag;                                         \
        (sendreq)->req_send.req_base.req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;        \
        (sendreq)->req_send.req_base.req_ompi.req_status._count =                         \
            (sendreq)->req_send.req_bytes_packed;                                         \
        (sendreq)->req_send.req_base.req_ompi.req_complete = true;                        \
        (sendreq)->req_state = MCA_PML_OB1_SR_COMPLETE;                                   \
        MCA_PML_OB1_SEND_REQUEST_TSTAMPS_DUMP(sendreq);                                   \
        if(ompi_request_waiting) {                                                        \
            ompi_condition_broadcast(&ompi_request_cond);                                 \
        }                                                                                 \
    } else if((sendreq)->req_send.req_base.req_free_called) {                             \
        MCA_PML_OB1_FREE((ompi_request_t**)&sendreq);                                     \
    } else if ((sendreq)->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) {         \
        mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq);                        \
        sendreq->req_state = MCA_PML_OB1_SR_COMPLETE;                                     \
    }                                                                                     \
}

/*
 * Advance a request
 */


#define MCA_PML_OB1_SEND_REQUEST_RETURN(sendreq)                            \
{                                                                           \
    if(NULL != (sendreq)->req_chunk) {                                      \
        mca_mpool_base_reg_mpool_t* reg = (sendreq)->req_chunk->mpools;     \
        while(NULL != reg->mpool) {                                         \
            OBJ_RELEASE(reg->mpool_registration);                           \
        }                                                                   \
        OBJ_RELEASE((sendreq)->req_chunk);                                  \
    }                                                                       \
                                                                            \
    /*  Let the base handle the reference counts */                         \
    MCA_PML_BASE_SEND_REQUEST_FINI((&(sendreq)->req_send));                 \
    OMPI_FREE_LIST_RETURN(                                                  \
        &mca_pml_ob1.send_requests, (opal_list_item_t*)sendreq);            \
}
                                                                                                                      
/**
 *  
 */

int mca_pml_ob1_send_request_start(
    mca_pml_ob1_send_request_t* sendreq,
    mca_pml_ob1_endpoint_t* endpoint);

/**
 *
 */
int mca_pml_ob1_send_request_schedule(
    mca_pml_ob1_send_request_t* sendreq);

/**
 *
 */

void mca_pml_ob1_send_request_put(
     mca_pml_ob1_send_request_t* sendreq,
     mca_btl_base_module_t* btl,
     mca_pml_ob1_rdma_hdr_t* hdr);

 
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

