/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "ompi_config.h"
#include "ompi/include/constants.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "mca/errmgr/errmgr.h"
#include "mca/mpool/mpool.h" 
#include "pml_ob1.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_proc.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_rdmafrag.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_endpoint.h"
#include "mca/bml/base/base.h"

                                                                                                         
static int mca_pml_ob1_send_request_fini(struct ompi_request_t** request)
{
    mca_pml_ob1_send_request_t* sendreq = *(mca_pml_ob1_send_request_t**)(request); 
    if(sendreq->req_send.req_base.req_persistent) {
       if(sendreq->req_send.req_base.req_free_called) {
           MCA_PML_OB1_SEND_REQUEST_FREE(sendreq);
       } else {
           sendreq->req_send.req_base.req_ompi.req_state = OMPI_REQUEST_INACTIVE; 
           /* rewind convertor */
           if(sendreq->req_send.req_bytes_packed) {
               size_t offset = 0;
               ompi_convertor_set_position(&sendreq->req_send.req_convertor, &offset);
           }
           /* if buffered send - release any resources */
           if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED &&
               sendreq->req_send.req_addr != sendreq->req_send.req_base.req_addr) {
               mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq);
           }
       }
    } else {
        MCA_PML_OB1_SEND_REQUEST_FREE(sendreq);
    }
    return OMPI_SUCCESS;
}

static int mca_pml_ob1_send_request_free(struct ompi_request_t** request)
{
    MCA_PML_OB1_SEND_REQUEST_FREE( *(mca_pml_ob1_send_request_t**)request );
    return OMPI_SUCCESS;
}

static int mca_pml_ob1_send_request_cancel(struct ompi_request_t* request, int complete)
{
    /* we dont cancel send requests by now */
    return OMPI_SUCCESS;
}

static void mca_pml_ob1_send_request_construct(mca_pml_ob1_send_request_t* req)
{
    req->req_send.req_base.req_type = MCA_PML_REQUEST_SEND;
    req->req_send.req_base.req_ompi.req_fini = mca_pml_ob1_send_request_fini;
    req->req_send.req_base.req_ompi.req_free = mca_pml_ob1_send_request_free;
    req->req_send.req_base.req_ompi.req_cancel = mca_pml_ob1_send_request_cancel;
}

static void mca_pml_ob1_send_request_destruct(mca_pml_ob1_send_request_t* req)
{
}


OBJ_CLASS_INSTANCE(
    mca_pml_ob1_send_request_t,
    mca_pml_base_send_request_t,
    mca_pml_ob1_send_request_construct,
    mca_pml_ob1_send_request_destruct);

/**
 * Completion of a short message - nothing left to schedule.
 */

void mca_pml_ob1_match_completion_cache(
    struct mca_btl_base_module_t* btl,  
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) descriptor->des_context; 

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    /* attempt to cache the descriptor */
    MCA_BML_BASE_BTL_DES_RETURN( bml_btl, descriptor ); 

    /* signal request completion */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
}

/**
 * Completion of a short message - nothing left to schedule.
 */

void mca_pml_ob1_match_completion_free(
    struct mca_btl_base_module_t* btl,  
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) descriptor->des_context; 

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    /* free the descriptor */
    mca_bml_base_free( bml_btl, descriptor ); 

    /* signal request completion */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
}

/*
 *  Completion of the first fragment of a long message that 
 *  requires an acknowledgement
 */
static void mca_pml_ob1_rndv_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)  descriptor->des_context; 

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    /* count bytes of user data actually delivered. As the rndv completion only
     * happens in one thread, the increase of the req_bytes_delivered does not
     * have to be atomic.
     */
    MCA_PML_OB1_COMPUTE_SEGMENT_LENGTH( descriptor->des_src,
                                        descriptor->des_src_cnt,
                                        sizeof(mca_pml_ob1_rendezvous_hdr_t),
                                        sendreq->req_bytes_delivered );

    /* return the descriptor */
    mca_bml_base_free(bml_btl, descriptor); 

    /* advance the request */
    MCA_PML_OB1_SEND_REQUEST_ADVANCE(sendreq);

    /* check for pending requests */
    MCA_PML_OB1_SEND_REQUEST_PROCESS_PENDING();
}


/**
 * Completion of a get request.
 */

static void mca_pml_ob1_rget_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)des->des_cbdata;
    size_t req_bytes_delivered = 0;

    /* count bytes of user data actually delivered and check for request completion */
    MCA_PML_OB1_COMPUTE_SEGMENT_LENGTH( des->des_src, des->des_src_cnt,
                                        0, req_bytes_delivered );
    if( OPAL_THREAD_ADD_SIZE_T( &sendreq->req_bytes_delivered, req_bytes_delivered )
        == sendreq->req_send.req_bytes_packed ) {
        OPAL_THREAD_LOCK(&ompi_request_lock);
        MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
    }

    /* release resources */
    btl->btl_free(btl,des);
}


/**
 * Completion of a control message - return resources.
 */

static void mca_pml_ob1_ctl_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    /* return the descriptor */
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) descriptor->des_context; 
    mca_bml_base_free(bml_btl, descriptor);
}

/**
 * Completion of additional fragments of a large message - may need
 * to schedule additional fragments.
 */

static void mca_pml_ob1_frag_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) descriptor->des_context; 
    size_t req_bytes_delivered = 0;

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    /* count bytes of user data actually delivered */
    MCA_PML_OB1_COMPUTE_SEGMENT_LENGTH( descriptor->des_src,
                                        descriptor->des_src_cnt,
                                        sizeof(mca_pml_ob1_frag_hdr_t),
                                        req_bytes_delivered );
    req_bytes_delivered = OPAL_THREAD_ADD_SIZE_T( &sendreq->req_bytes_delivered,
                                                  req_bytes_delivered );
    if (OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,-1) == 0 &&
        req_bytes_delivered == sendreq->req_send.req_bytes_packed) {
    /*if( OPAL_THREAD_ADD_SIZE_T( &sendreq->req_bytes_delivered, req_bytes_delivered )
        == sendreq->req_send.req_bytes_packed) {*/
        OPAL_THREAD_LOCK(&ompi_request_lock);
        MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq); 
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
    } else {
        mca_pml_ob1_send_request_schedule(sendreq);
    }

    /* return the descriptor */
    mca_bml_base_free(bml_btl, descriptor);

    /* check for pending requests */
    MCA_PML_OB1_SEND_REQUEST_PROCESS_PENDING();
}



/**
 *  Buffer the entire message and mark as complete.
 */

int mca_pml_ob1_send_request_start_buffered(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size)
{
    mca_btl_base_descriptor_t* descriptor;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    struct iovec iov;
    unsigned int iov_count;
    size_t max_data;
    int32_t free_after;
    int rc;

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &descriptor, sizeof(mca_pml_ob1_rendezvous_hdr_t) + size);
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    segment = descriptor->des_src;

    /* pack the data into the BTL supplied buffer */
    iov.iov_base = (void*)((unsigned char*)segment->seg_addr.pval + 
             sizeof(mca_pml_ob1_rendezvous_hdr_t));
    iov.iov_len = size;
    iov_count = 1;
    max_data = size;
    if((rc = ompi_convertor_pack(
        &sendreq->req_send.req_convertor,
        &iov,
        &iov_count,
        &max_data,
        &free_after)) < 0) {
        mca_bml_base_free(bml_btl, descriptor);
        return rc;
    }

    /* build rendezvous header */
    hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
    hdr->hdr_rndv.hdr_src_req.pval = sendreq;

    /* update lengths */
    segment->seg_len = sizeof(mca_pml_ob1_rendezvous_hdr_t) + max_data;
    sendreq->req_send_offset = max_data;

    descriptor->des_cbfunc = mca_pml_ob1_rndv_completion;
    descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    descriptor->des_cbdata = sendreq;

    /* buffer the remainder of the message */
    rc = mca_pml_base_bsend_request_alloc((ompi_request_t*)sendreq);
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, descriptor);
        return rc;
    }

    iov.iov_base = (void*)(((unsigned char*)sendreq->req_send.req_addr) + sendreq->req_send_offset);
    iov.iov_len = max_data = sendreq->req_send.req_bytes_packed - sendreq->req_send_offset;

    if((rc = ompi_convertor_pack(
            &sendreq->req_send.req_convertor,
            &iov,
            &iov_count,
            &max_data,
            &free_after)) < 0) {
            mca_bml_base_free(bml_btl, descriptor);
            return rc;
    }

    /* re-init convertor for packed data */
    ompi_convertor_prepare_for_send(
            &sendreq->req_send.req_convertor,
            sendreq->req_send.req_datatype,
            sendreq->req_send.req_count,
            sendreq->req_send.req_addr);

    /* request is complete at mpi level */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    MCA_PML_OB1_SEND_REQUEST_MPI_COMPLETE(sendreq);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    /* send */
    rc = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, descriptor );
    }
    return rc;
}


/**
 *  BTL requires "specially" allocated memory. Request a segment that
 *  is used for initial hdr and any eager data.
 */

int mca_pml_ob1_send_request_start_copy(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size)
{
    mca_btl_base_descriptor_t* descriptor;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    struct iovec iov;
    unsigned int iov_count;
    size_t max_data;
    int32_t free_after;
    int rc;

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &descriptor, sizeof(mca_pml_ob1_match_hdr_t) + size);
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    segment = descriptor->des_src;

    max_data = size;
    if(size > 0) { 
        /* pack the data into the supplied buffer */
        iov.iov_base = (void*)((unsigned char*)segment->seg_addr.pval + sizeof(mca_pml_ob1_match_hdr_t));
        iov.iov_len = size;
        iov_count = 1;
        if((rc = ompi_convertor_pack(
                                     &sendreq->req_send.req_convertor,
                                     &iov,
                                     &iov_count,
                                     &max_data,
                                     &free_after)) < 0) {
            mca_bml_base_free(bml_btl, descriptor);
            return rc;
        }
    }
    
    /* build match header */
    hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_MATCH;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;

    /* update lengths */
    segment->seg_len = sizeof(mca_pml_ob1_match_hdr_t) + max_data;
    sendreq->req_send_offset = max_data;
    sendreq->req_rdma_offset = max_data;

    /* short message */
    descriptor->des_cbfunc = mca_pml_ob1_match_completion_free;
    descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    descriptor->des_cbdata = sendreq;

    /* signal request completion */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    MCA_PML_OB1_SEND_REQUEST_MPI_COMPLETE(sendreq);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    /* send */
    rc = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, descriptor );
    } 
    return rc;
}

/**
 *  BTL can send directly from user buffer so allow the BTL
 *  to prepare the segment list.
 */

int mca_pml_ob1_send_request_start_prepare(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size)
{
    mca_btl_base_descriptor_t* descriptor;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    int rc;

    /* prepare descriptor */
    mca_bml_base_prepare_src(
            bml_btl, 
            NULL,
            &sendreq->req_send.req_convertor,
            sizeof(mca_pml_ob1_match_hdr_t),
            &size,
            &descriptor);
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    segment = descriptor->des_src;

    /* build match header */
    hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_MATCH;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;

    /* short message */
    descriptor->des_cbfunc = mca_pml_ob1_match_completion_free;
       
    /* update lengths */
    sendreq->req_send_offset = size;
    sendreq->req_rdma_offset = size;

    descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    descriptor->des_cbdata = sendreq;

    /* send */
    rc = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_PML); 
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, descriptor );
    }
    return rc;
}


/**
 *  We have contigous data that is registered - schedule across
 *  available nics.
 */

int mca_pml_ob1_send_request_start_rdma(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size)
{
    /*
     * FIX - to get the basics working - schedule on the
     * first BTL only
     */

    mca_mpool_base_registration_t* reg = sendreq->req_rdma[0].btl_reg;
    mca_btl_base_descriptor_t* src;
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    size_t i;
    int rc;


    bml_btl = sendreq->req_rdma[0].bml_btl;
    if(sendreq->req_rdma_cnt == 1 &&
       bml_btl->btl_flags & MCA_BTL_FLAGS_GET) {

         /* prepare source descriptor/segment(s) */
         size = sendreq->req_send.req_bytes_packed;
         mca_bml_base_prepare_src(
             bml_btl, 
             reg,
             &sendreq->req_send.req_convertor,
             0,
             &size,
             &src);
         if(NULL == src) {
             return OMPI_ERR_OUT_OF_RESOURCE;
         } 
         src->des_cbfunc = mca_pml_ob1_rget_completion;
         src->des_cbdata = sendreq;

         /* allocate space for get hdr + segment list */
         mca_bml_base_alloc(bml_btl, &des, 
            sizeof(mca_pml_ob1_rget_hdr_t) + (sizeof(mca_btl_base_segment_t)*(src->des_src_cnt-1)));
         if(NULL == des) {
             return OMPI_ERR_OUT_OF_RESOURCE;
         }
         segment = des->des_src;

         /* build match header */
         hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
         hdr->hdr_common.hdr_flags = MCA_PML_OB1_HDR_FLAGS_CONTIG|MCA_PML_OB1_HDR_FLAGS_PIN;
         hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RGET;
         hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
         hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
         hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
         hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
         hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
         hdr->hdr_rndv.hdr_src_req.pval = sendreq;
         hdr->hdr_rget.hdr_des.pval = src;
         hdr->hdr_rget.hdr_seg_cnt = src->des_src_cnt;
         for(i=0; i<src->des_src_cnt; i++)
             hdr->hdr_rget.hdr_segs[i] = src->des_src[i];
         des->des_cbfunc = mca_pml_ob1_ctl_completion;

      } else {

         /* allocate a rendezvous header - dont eager send any data 
          * receiver will schedule rdma put(s) of the entire message
          */

         mca_bml_base_alloc(bml_btl, &des, sizeof(mca_pml_ob1_rendezvous_hdr_t));
         if(NULL == des) {
             return OMPI_ERR_OUT_OF_RESOURCE;
         }
         segment = des->des_src;
            
         /* build hdr */
         hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
         hdr->hdr_common.hdr_flags = MCA_PML_OB1_HDR_FLAGS_CONTIG|MCA_PML_OB1_HDR_FLAGS_PIN;
         hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
         hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
         hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
         hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
         hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
         hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
         hdr->hdr_rndv.hdr_src_req.pval = sendreq;

         /* update lengths with number of bytes actually packed */
         segment->seg_len = sizeof(mca_pml_ob1_rendezvous_hdr_t);
         sendreq->req_send_offset = 0;
    
         /* first fragment of a long message */
         des->des_cbfunc = mca_pml_ob1_rndv_completion;
    }

    des->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    des->des_cbdata = sendreq;

    /* send */
    rc = mca_bml_base_send(bml_btl, des, MCA_BTL_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, des);
    }
    return rc;
}


/**
 *  Rendezvous is required. Not doing rdma so eager send up to
 *  the btls eager limit.
 */

int mca_pml_ob1_send_request_start_rndv(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size,
    int flags)
{
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    int rc;

    /* prepare descriptor */
    if(size == 0) {
        mca_bml_base_alloc(
                           bml_btl, 
                           &des, 
                           sizeof(mca_pml_ob1_rendezvous_hdr_t)
                           ); 
    } else {
        mca_bml_base_prepare_src(
                                 bml_btl, 
                                 NULL,
                                 &sendreq->req_send.req_convertor,
                                 sizeof(mca_pml_ob1_rendezvous_hdr_t),
                                 &size,
                                 &des);
    }

    if(NULL == des) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    segment = des->des_src;

    /* build hdr */
    hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = flags;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
    hdr->hdr_rndv.hdr_src_req.pval = sendreq;

    /* first fragment of a long message */
    des->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    des->des_cbdata = sendreq;
    des->des_cbfunc = mca_pml_ob1_rndv_completion;
    sendreq->req_send_offset = size;

    /* send */
    rc = mca_bml_base_send(bml_btl, des, MCA_BTL_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, des );
    }
    return rc;
}


/**
 *  Schedule pipeline of send descriptors for the given request.
 *  Up to the rdma threshold. If this is a send based protocol,
 *  the rdma threshold is the end of the message. Otherwise, schedule
 *  fragments up to the threshold to overlap initial registration/setup
 *  costs of the rdma.
 */

int mca_pml_ob1_send_request_schedule(mca_pml_ob1_send_request_t* sendreq)
{ 
    /*
     * Only allow one thread in this routine for a given request.
     * However, we cannot block callers on a mutex, so simply keep track
     * of the number of times the routine has been called and run through
     * the scheduling logic once for every call.
    */
    
    mca_bml_base_endpoint_t* bml_endpoint = sendreq->req_endpoint;
    
    if(OPAL_THREAD_ADD32(&sendreq->req_lock,1) == 1) {
        do {
            /* allocate remaining bytes to BTLs */
            size_t bytes_remaining = sendreq->req_rdma_offset - sendreq->req_send_offset;
            while(bytes_remaining > 0 && 
                  (sendreq->req_pipeline_depth < mca_pml_ob1.send_pipeline_depth ||
                   sendreq->req_rdma_offset < sendreq->req_send.req_bytes_packed)) {
                
                mca_pml_ob1_frag_hdr_t* hdr;
                mca_btl_base_descriptor_t* des;
                int rc;
                size_t size; 
                mca_bml_base_btl_t* bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_send); 
                size_t num_btl_avail = bml_endpoint->btl_send.arr_size; 
                
                if(num_btl_avail == 1 || bytes_remaining < bml_btl->btl_min_send_size) {
                    size = bytes_remaining;

                /* otherwise attempt to give the BTL a percentage of the message
                 * based on a weighting factor. for simplicity calculate this as
                 * a percentage of the overall message length (regardless of amount
                 * previously assigned)
                 */
                } else {
                    size = (size_t)(bml_btl->btl_weight * bytes_remaining);
                } 

                /* makes sure that we don't exceed BTL max send size */
                if (bml_btl->btl_max_send_size != 0 && 
                    size > bml_btl->btl_max_send_size - sizeof(mca_pml_ob1_frag_hdr_t)) {
                    size = bml_btl->btl_max_send_size - sizeof(mca_pml_ob1_frag_hdr_t);

                    /* very expensive - however for buffered sends we need to send on a 
                     * boundary that the receiver will be able to unpack completely
                     * using the native datatype
                     */
                    if(sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) {
                        ompi_convertor_t convertor;
                        size_t position = sendreq->req_send_offset + size;
                        /*
                         * We need this local convertor in order to correctly compute
                         * the correct position. Therefore we have to correctly construct and
                         * destruct it.
                         */
                        OBJ_CONSTRUCT( &convertor, ompi_convertor_t );
                        ompi_convertor_copy_and_prepare_for_send(
                            &sendreq->req_send.req_convertor,
                            sendreq->req_send.req_base.req_datatype,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_addr,
                            &convertor);
                        ompi_convertor_set_position(&convertor, &position);
                        OBJ_DESTRUCT( &convertor );
                        size = position - sendreq->req_send_offset;
                    }
                }
                
                
                /* pack into a descriptor */
                ompi_convertor_set_position(&sendreq->req_send.req_convertor, 
                                            &sendreq->req_send_offset);
                
                mca_bml_base_prepare_src(
                                         bml_btl, 
                                         NULL, 
                                         &sendreq->req_send.req_convertor,
                                         sizeof(mca_pml_ob1_frag_hdr_t),
                                         &size,
                                         &des
                                         );
                
                if(des == NULL) {
                    OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
                    opal_list_append(&mca_pml_ob1.send_pending, (opal_list_item_t*)sendreq);
                    OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    break;
                }
                des->des_cbfunc = mca_pml_ob1_frag_completion;
                des->des_cbdata = sendreq;

                /* setup header */
                hdr = (mca_pml_ob1_frag_hdr_t*)des->des_src->seg_addr.pval;
                hdr->hdr_common.hdr_flags = 0;
                hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_FRAG;
                hdr->hdr_frag_offset = sendreq->req_send_offset;
                hdr->hdr_src_req.pval = sendreq;
                hdr->hdr_dst_req = sendreq->req_recv;

                /* update state */
                sendreq->req_send_offset += size;
                OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,1);

                /* initiate send - note that this may complete before the call returns */
                rc = mca_bml_base_send( bml_btl, des, MCA_BTL_TAG_PML);
                
                if(rc == OMPI_SUCCESS) {
                    bytes_remaining -= size;
                } else {
                    sendreq->req_send_offset -= size;
                    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,-1);
                    mca_bml_base_free(bml_btl,des);
                    OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
                    opal_list_append(&mca_pml_ob1.send_pending, (opal_list_item_t*)sendreq);
                    OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    break;
                }
                mca_pml_ob1_progress(); 
            }
        } while (OPAL_THREAD_ADD32(&sendreq->req_lock,-1) > 0);
    }
    return OMPI_SUCCESS;
} 


/**
 * Return resources used by the RDMA
 */

static void mca_pml_ob1_fin_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    
    mca_pml_ob1_rdma_frag_t* frag = (mca_pml_ob1_rdma_frag_t*)des->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context; 
    
    MCA_PML_OB1_RDMA_FRAG_RETURN(frag);
    MCA_BML_BASE_BTL_DES_RETURN(bml_btl, des);
}

/**
 *  An RDMA put operation has completed:
 *  (1) Update request status and if required set completed
 *  (2) Send FIN control message to the destination 
 */

static void mca_pml_ob1_put_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    mca_pml_ob1_rdma_frag_t* frag = (mca_pml_ob1_rdma_frag_t*)des->des_cbdata;
    mca_pml_ob1_send_request_t* sendreq = frag->rdma_req;
    mca_btl_base_descriptor_t* fin;
    mca_pml_ob1_fin_hdr_t* hdr;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context; 
    int rc;

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        ORTE_ERROR_LOG(status);
        orte_errmgr.abort();
    }

    /* check for request completion */
    if( OPAL_THREAD_ADD_SIZE_T(&sendreq->req_bytes_delivered, frag->rdma_length)
        >= sendreq->req_send.req_bytes_packed) {
        OPAL_THREAD_LOCK(&ompi_request_lock);
        MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
    }

    /* allocate descriptor for fin control message - note that
     * the rdma descriptor cannot be reused as it points directly
     * at the user buffer
     */
    frag->rdma_state = MCA_PML_OB1_RDMA_FIN;

    MCA_PML_OB1_DES_ALLOC(bml_btl, fin, sizeof(mca_pml_ob1_fin_hdr_t));
    if(NULL == fin) {
        OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
        opal_list_append(&mca_pml_ob1.rdma_pending, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        goto cleanup;
    }
    fin->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    fin->des_cbfunc = mca_pml_ob1_fin_completion;
    fin->des_cbdata = frag;

    /* fill in header */
    hdr = (mca_pml_ob1_fin_hdr_t*)fin->des_src->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_FIN;
    hdr->hdr_des = frag->rdma_hdr.hdr_rdma.hdr_des;

    /* queue request */
    rc = mca_bml_base_send(
                           bml_btl,
                           fin,
                           MCA_BTL_TAG_PML
                           );
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, fin);
        if(rc == OMPI_ERR_OUT_OF_RESOURCE) {
            OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
            opal_list_append(&mca_pml_ob1.rdma_pending, (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        } else {
            /* TSW - FIX */
            ORTE_ERROR_LOG(rc);
            orte_errmgr.abort();
        }
    }

cleanup:
    /* return rdma descriptor - do this after queuing the fin message - as 
     * release rdma resources (unpin memory) can take some time.
     */
    des->des_dst = NULL; 
    des->des_dst_cnt = 0; 
    mca_bml_base_free(bml_btl,  des);

}


/**
 *  Receiver has scheduled an RDMA operation:
 *  (1) Allocate an RDMA fragment to maintain the state of the operation
 *  (2) Call BTL prepare_src to pin/prepare source buffers
 *  (3) Queue the RDMA put 
 */

void mca_pml_ob1_send_request_put(
    mca_pml_ob1_send_request_t* sendreq,
    mca_btl_base_module_t* btl, 
    mca_pml_ob1_rdma_hdr_t* hdr)
{ 
    mca_bml_base_endpoint_t *bml_endpoint = sendreq->req_endpoint;
    mca_mpool_base_registration_t* reg = NULL;
    mca_bml_base_btl_t* bml_btl;
    mca_btl_base_descriptor_t* des;
    mca_pml_ob1_rdma_frag_t* frag;
    size_t offset = hdr->hdr_rdma_offset;
    size_t i, size = 0;
    int rc;
    
    bml_btl = mca_bml_base_btl_array_find(&bml_endpoint->btl_rdma, btl);  
    MCA_PML_OB1_RDMA_FRAG_ALLOC(frag, rc); 
    if(NULL == frag) {
        /* TSW - FIX */
        ORTE_ERROR_LOG(rc);
        orte_errmgr.abort();
    }

    /* setup fragment */
    for(i=0; i<hdr->hdr_seg_cnt; i++) {
        size += hdr->hdr_segs[i].seg_len;
        frag->rdma_segs[i] = hdr->hdr_segs[i];
    }
    frag->rdma_hdr.hdr_rdma = *hdr;
    frag->rdma_req = sendreq; 
    frag->rdma_ep = bml_endpoint;
    frag->rdma_state = MCA_PML_OB1_RDMA_PREPARE;

    /* lookup the corresponding registration */
    for(i=0; i<sendreq->req_rdma_cnt; i++) {
       if(sendreq->req_rdma[i].bml_btl == bml_btl) {
           reg = sendreq->req_rdma[i].btl_reg;
           break;
       }
    } 
    
    /* setup descriptor */
    ompi_convertor_set_position(&sendreq->req_send.req_convertor, &offset);
    mca_bml_base_prepare_src(
        bml_btl, 
        reg,
        &sendreq->req_send.req_convertor, 
        0,
        &size, 
        &des
        );
    
    if(NULL == des) { 
        OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
        opal_list_append(&mca_pml_ob1.rdma_pending, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
    }
    frag->rdma_state = MCA_PML_OB1_RDMA_PUT;
    frag->rdma_length = size;

    des->des_dst = frag->rdma_segs;
    des->des_dst_cnt = hdr->hdr_seg_cnt;
    des->des_cbfunc = mca_pml_ob1_put_completion;
    des->des_cbdata = frag;

    if(OMPI_SUCCESS != (rc = mca_bml_base_put(bml_btl, des))) {
        if(rc == OMPI_ERR_OUT_OF_RESOURCE) {
            OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
            opal_list_append(&mca_pml_ob1.rdma_pending, (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        } else {
            /* TSW - FIX */
            ORTE_ERROR_LOG(rc);
            orte_errmgr.abort();
        }
    }
}


