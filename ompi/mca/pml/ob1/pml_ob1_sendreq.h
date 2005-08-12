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
#include "mca/bml/bml.h" 

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_pml_ob1_send_request_t {
    mca_pml_base_send_request_t req_send;
    ompi_proc_t* req_proc; 
    mca_bml_base_endpoint_t* bml_endpoint;
    volatile int32_t req_state;
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
    ompi_proc_t *proc =                                                    \
         comm->c_pml_procs[dst]->proc_ompi;                                \
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
 opal_output(0, "[%d,%d,%d] src start, %llu\n",  \
    ORTE_NAME_ARGS(orte_process_info.my_name), (sendreq)->t_start); \
\
 opal_output(0, "[%d,%d,%d] src send start, %llu\n",  \
    ORTE_NAME_ARGS(orte_process_info.my_name), (sendreq)->t_send1); \
\
 opal_output(0, "[%d,%d,%d] src scheduled, %llu\n",  \
    ORTE_NAME_ARGS(orte_process_info.my_name), (sendreq)->t_scheduled); \
\
 opal_output(0, "[%d,%d,%d] src send complete, %llu\n",  \
    ORTE_NAME_ARGS(orte_process_info.my_name), (sendreq)->t_send2); \
\
 for(i=0; i<(sendreq)->t_pin_index; i++) \
     opal_output(0, "[%d,%d,%d] src pin, %llu %llu\n",  \
        ORTE_NAME_ARGS(orte_process_info.my_name), (sendreq)->t_pin[i], \
        (sendreq)->t_put[i] - (sendreq)->t_pin[i]); \
 for(i=0; i<(sendreq)->t_put_index; i++) \
     opal_output(0, "[%d,%d,%d] src put, %llu %llu\n",  \
        ORTE_NAME_ARGS(orte_process_info.my_name), (sendreq)->t_put[i], \
        (sendreq)->t_fin[i] - (sendreq)->t_put[i]); \
 for(i=0; i<(sendreq)->t_fin_index; i++) \
     opal_output(0, "[%d,%d,%d] src fin, %llu\n",  \
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
do {                                                                                      \
    mca_pml_ob1_comm_t* comm = sendreq->req_send.req_base.req_comm->c_pml_comm;           \
    mca_bml_base_endpoint_t* endpoint = (mca_bml_base_endpoint_t*)sendreq->req_proc->proc_pml; \
                                                                                          \
    MCA_PML_OB1_SEND_REQUEST_TSTAMPS_INIT(sendreq);                                       \
    sendreq->req_lock = 0;                                                                \
    sendreq->req_pipeline_depth = 0;                                                      \
    sendreq->req_bytes_delivered = 0;                                                     \
    sendreq->req_chunk = NULL;                                                            \
    sendreq->req_state = 0;                                                               \
    sendreq->req_send_offset = 0;                                                         \
    sendreq->req_send.req_base.req_pml_complete = false;                                  \
    sendreq->req_send.req_base.req_ompi.req_complete = false;                             \
    sendreq->req_send.req_base.req_ompi.req_state = OMPI_REQUEST_ACTIVE;                  \
    sendreq->req_send.req_base.req_sequence = OPAL_THREAD_ADD32(                          \
        &comm->procs[sendreq->req_send.req_base.req_peer].send_sequence,1);               \
    sendreq->bml_endpoint = endpoint;                                                     \
                                                                                          \
    /* shortcut for zero byte */                                                          \
    if(sendreq->req_send.req_bytes_packed == 0 &&                                         \
       sendreq->req_send.req_send_mode != MCA_PML_BASE_SEND_SYNCHRONOUS) {                \
        mca_btl_base_descriptor_t* descriptor;                                            \
        mca_btl_base_segment_t* segment;                                                  \
        mca_bml_base_btl_t* bml_btl;                                                      \
        mca_pml_ob1_hdr_t* hdr;                                                           \
                                                                                          \
        /* select a btl */                                                                \
        bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);                  \
        if(NULL == bml_btl) {                                                             \
            rc = OMPI_ERR_UNREACH;                                                        \
            break;                                                                        \
        }                                                                                 \
                                                                                          \
        /* allocate a descriptor */                                                       \
        MCA_BML_BASE_BTL_DES_ALLOC(bml_btl, descriptor, sizeof(mca_pml_ob1_match_hdr_t)); \
        if(NULL == descriptor) {                                                          \
            return OMPI_ERR_OUT_OF_RESOURCE;                                              \
        }                                                                                 \
        segment = descriptor->des_src;                                                    \
                                                                                          \
        /* build hdr */                                                                   \
        hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;                                 \
        hdr->hdr_common.hdr_flags = 0;                                                    \
        hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_MATCH;                            \
        hdr->hdr_match.hdr_contextid = sendreq->req_send.req_base.req_comm->c_contextid;  \
        hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;          \
        hdr->hdr_match.hdr_dst = sendreq->req_send.req_base.req_peer;                     \
        hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;                      \
        hdr->hdr_match.hdr_msg_length = 0;                                                \
        hdr->hdr_match.hdr_msg_seq = sendreq->req_send.req_base.req_sequence;             \
                                                                                          \
        /* short message */                                                               \
        descriptor->des_cbfunc = mca_pml_ob1_match_completion;                            \
        descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;                              \
        descriptor->des_cbdata = sendreq;                                                 \
                                                                                          \
        /* request is complete at mpi level */                                            \
        ompi_request_complete((ompi_request_t*)sendreq);                                  \
                                                                                          \
        /* send */                                                                        \
        rc = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_PML);                     \
        if(OMPI_SUCCESS != rc) {                                                          \
            mca_bml_base_free(bml_btl, descriptor );                                      \
        }                                                                                 \
                                                                                          \
    } else {                                                                              \
        /* handle buffered send */                                                        \
        if(sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) {               \
            mca_pml_base_bsend_request_start(&sendreq->req_send.req_base.req_ompi);       \
        }                                                                                 \
                                                                                          \
        /* start request */                                                               \
        rc = mca_pml_ob1_send_request_start( sendreq );                                   \
    }                                                                                     \
} while (0)


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
        MCA_PML_OB1_SEND_REQUEST_TSTAMPS_DUMP(sendreq);                                   \
        if(ompi_request_waiting) {                                                        \
            opal_condition_broadcast(&ompi_request_cond);                                 \
        }                                                                                 \
    } else if((sendreq)->req_send.req_base.req_free_called) {                             \
        MCA_PML_OB1_FREE((ompi_request_t**)&sendreq);                                     \
    } else if ((sendreq)->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) {         \
        mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq);                        \
    }                                                                                     \
}


/*
 * Advance a pending send request. Note that the initial descriptor must complete
 * and the acknowledment received before the request can complete or be scheduled.
 * However, these events may occur in either order.
 */

#define MCA_PML_OB1_SEND_REQUEST_ADVANCE(sendreq)                                         \
do {                                                                                      \
    bool schedule = false;                                                                \
                                                                                          \
    /* has an acknowledgment been received */                                             \
    if(OPAL_THREAD_ADD32(&sendreq->req_state, 1) == 2) {                                  \
        OPAL_THREAD_LOCK(&ompi_request_lock);                                             \
        if(sendreq->req_bytes_delivered == sendreq->req_send.req_bytes_packed) {          \
            MCA_PML_OB1_SEND_REQUEST_COMPLETE(sendreq);                                   \
        } else {                                                                          \
            schedule = true;                                                              \
        }                                                                                 \
        OPAL_THREAD_UNLOCK(&ompi_request_lock);                                           \
    }                                                                                     \
                                                                                          \
    /* additional data to schedule */                                                     \
    if(schedule == true) {                                                                \
        mca_pml_ob1_send_request_schedule(sendreq);                                       \
    }                                                                                     \
} while (0)

/*
 * Release resources associated with a request
 */

#define MCA_PML_OB1_SEND_REQUEST_RETURN(sendreq)                            \
{                                                                           \
    if(NULL != (sendreq)->req_chunk) {                                      \
        mca_mpool_base_reg_mpool_t* reg = (sendreq)->req_chunk->mpools;     \
        while(NULL != reg->mpool) {                                         \
            if(NULL != reg->mpool_registration) {                           \
                OBJ_RELEASE(reg->mpool_registration);                       \
            }                                                               \
            reg++;                                                          \
        }                                                                   \
        OBJ_RELEASE((sendreq)->req_chunk);                                  \
    }                                                                       \
                                                                            \
    /*  Let the base handle the reference counts */                         \
    MCA_PML_BASE_SEND_REQUEST_FINI((&(sendreq)->req_send));                 \
    OMPI_FREE_LIST_RETURN(                                                  \
        &mca_pml_ob1.send_requests, (opal_list_item_t*)sendreq);            \
}
                                                                                                                      

/*
 * Update bytes delivered on request based on supplied descriptor
 */

#define MCA_PML_OB1_SEND_REQUEST_SET_BYTES_DELIVERED(sendreq, descriptor, hdrlen)   \
do {                                                                                \
   size_t i;                                                                        \
   mca_btl_base_segment_t* segments = descriptor->des_src;                          \
                                                                                    \
   for(i=0; i<descriptor->des_src_cnt; i++) {                                       \
       sendreq->req_bytes_delivered += segments[i].seg_len;                         \
   }                                                                                \
   sendreq->req_bytes_delivered -= hdrlen;                                          \
                                                                                    \
} while(0)
                                                                                                                          
/*
 * Attempt to process any pending requests
 */

#define MCA_PML_OB1_SEND_REQUEST_PROCESS_PENDING()                    \
do {                                                                  \
    /* advance pending requests */                                    \
    while(opal_list_get_size(&mca_pml_ob1.send_pending)) {            \
        mca_pml_ob1_send_request_t* sendreq;                          \
        OPAL_THREAD_LOCK(&mca_pml_ob1.lock);                          \
        sendreq = (mca_pml_ob1_send_request_t*)                       \
            opal_list_remove_first(&mca_pml_ob1.send_pending);        \
        OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);                        \
        if(NULL == sendreq)                                           \
            break;                                                    \
        mca_pml_ob1_send_request_schedule(sendreq);                   \
    }                                                                 \
} while (0)


/**
 *  Start the specified request
 */

int mca_pml_ob1_send_request_start(
    mca_pml_ob1_send_request_t* sendreq);

/**
 *  Schedule additional fragments 
 */
int mca_pml_ob1_send_request_schedule(
    mca_pml_ob1_send_request_t* sendreq);

/**
 *  Completion callback on match header
 */
void mca_pml_ob1_match_completion(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status);

/**
 *  Initiate a put scheduled by the receiver.
 */

void mca_pml_ob1_send_request_put(
     mca_pml_ob1_send_request_t* sendreq,
     mca_btl_base_module_t* btl,
     mca_pml_ob1_rdma_hdr_t* hdr);

 
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

