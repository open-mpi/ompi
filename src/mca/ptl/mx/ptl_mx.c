/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <errno.h>
#include <unistd.h>
#include <string.h>

#include "include/constants.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "ptl_mx.h"
#include "ptl_mx_peer.h"
#include "ptl_mx_sendfrag.h"
#include "ptl_mx_recvfrag.h"


mca_ptl_mx_module_t mca_ptl_mx_module = {
    {
    &mca_ptl_mx_component.super,
    16, /* ptl_cache_size */
    sizeof(mca_ptl_mx_send_frag_t), /* ptl_cache_bytes */
    (32 * 1024) - sizeof(mca_ptl_base_header_t), /* ptl_frag_first_size */
    0, /* ptl_frag_min_size */
    -1, /* ptl_frag_max_size */
    0, /* ptl_exclusivity */
    0, /* ptl_latency */
    0, /* ptl_bandwidth */
    MCA_PTL_PUT, /* ptl flags */
    mca_ptl_mx_add_procs,
    mca_ptl_mx_del_procs,
    mca_ptl_mx_finalize,
    mca_ptl_mx_send,  
    mca_ptl_mx_send_continue,  
    NULL, /* get */
    mca_ptl_mx_matched, /* matched */
    mca_ptl_mx_request_init,
    mca_ptl_mx_request_fini,
    NULL, /* match */
    NULL,
    NULL
    }
};



/**
 * Allocate memory for use by the convert.
 */

static void *mca_ptl_mx_alloc(size_t *size)
{
    return malloc(*size);
}

/**
 * PML->PTL Initialize a send request for use by the PTL.
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 *
 * To reduce latency (number of required allocations), the PML allocates up
 * to ptl_cache_bytes of additional space contigous w/ the base send request.
 * This space may be used by the PTL for additional control information (e.g.
 * first fragment descriptor).
 *
 * The ptl_request_init() function is called by the PML when requests are
 * allocated to the PTLs cache. These requests will be cached by the PML
 * on completion and re-used by the same PTL w/out additional calls to
 * ptl_request_init().
 *
 * If the cache size is exceeded, the PML may pass requests to ptl_send/ptl_put
 * that have been taken from the global pool and have not been initialized by the
 * PTL. These requests will have the req_cached attribute set to false.
 *
 */

int mca_ptl_mx_request_init(struct mca_ptl_base_module_t* ptl, mca_pml_base_send_request_t* request)
{
    OBJ_CONSTRUCT(request+1, mca_ptl_mx_send_frag_t);
    return OMPI_SUCCESS;
}


/**
 * PML->PTL Cleanup any resources that may have been associated with the
 *          request by the PTL.
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 *
 * The ptl_request_fini function is called when the PML removes a request
 * from the PTLs cache (due to resource constraints).  This routine provides
 * the PTL the chance to cleanup/release any resources cached on the send
 * descriptor by the PTL.
 */

void mca_ptl_mx_request_fini(struct mca_ptl_base_module_t* ptl, mca_pml_base_send_request_t* request)
{
    OBJ_DESTRUCT(request+1);
}


/**
 * PML->PTL Initiate a send to the peer.
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param request (IN)           Send request
 * @param offset                 Current offset into packed/contiguous buffer.
 * @param size (IN)              Number of bytes PML is requesting PTL to deliver,
 * @param flags (IN)             Flags that should be passed to the peer via the message header.
 * @param request (OUT)          OMPI_SUCCESS if the PTL was able to queue one or more fragments
 *
 * The PML implements a rendevouz protocol, with up to the PTL threshold
 * (ptl_first_frag_size) bytes of the message sent in eager send mode. The ptl_send()
 * function is called by the PML to initiate the send of the first message fragment.
 *
 * The PTL is responsible for updating the current data offset (req_offset) in the
 * request to reflect the actual number of bytes fragmented.  This may be less than
 * the requested size, due to resource constraints or datatype alighnment/offset. If
 * an acknowledgment is required, the MCA_PTL_FLAGS_ACK bit will be set in the
 * flags parameter. In this case, the PTL should not call ptl_send_progress() function
 * to indicate completion of the fragment until the ack is received. For all other
 * fragments ptl_send_progress() may be called based on local completion semantics.
 */
                                                                                                   
int mca_ptl_mx_send(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_pml_base_send_request_t* sendreq,
    size_t offset,
    size_t size,
    int flags)
{
    mca_ptl_mx_module_t* mx_ptl = (mca_ptl_mx_module_t*)ptl;
    mca_ptl_mx_send_frag_t* sendfrag;
    mca_ptl_base_header_t* hdr;
    mx_segment_t *segments;
    mx_return_t mx_return;
    ompi_ptr_t match;
    int rc;

    if (sendreq->req_cached) {
        sendfrag = (mca_ptl_mx_send_frag_t*)(sendreq+1);
    } else {
        ompi_list_item_t* item;
        OMPI_FREE_LIST_GET(&mca_ptl_mx_component.mx_send_frags, item, rc);
        if(NULL == (sendfrag = (mca_ptl_mx_send_frag_t*)item))
            return rc;
    }

    /* setup iovec */
    sendfrag->frag_progress = 0;
    sendfrag->frag_free = 0;

    /* initialize convertor */
    if(size > 0) {
       ompi_convertor_t *convertor;
       struct iovec iov;
       unsigned int iov_count;
       unsigned int max_data;
       int rc;

       convertor = &sendfrag->frag_send.frag_base.frag_convertor;
       ompi_convertor_copy(&sendreq->req_convertor, convertor);
       ompi_convertor_init_for_send(
                    convertor,
                    0,
                    sendreq->req_datatype,
                    sendreq->req_count,
                    sendreq->req_addr,
                    offset,
                    mca_ptl_mx_alloc );
                                                                                                                      
        /* if data is contigous convertor will return an offset
         * into users buffer - otherwise will return an allocated buffer
         * that holds the packed data
         */
        iov.iov_base = NULL;
        iov.iov_len = size;
        iov_count = 1;
        max_data = size;
        if((rc = ompi_convertor_pack(
            convertor,
            &iov,
            &iov_count,
            &max_data,
            &(sendfrag->frag_free))) < 0) {
            return OMPI_ERROR;
        }
        sendfrag->frag_segments[1].segment_ptr = iov.iov_base;
        sendfrag->frag_segments[1].segment_length = iov.iov_len;
        sendfrag->frag_send.frag_base.frag_addr = iov.iov_base;
        sendfrag->frag_send.frag_base.frag_size = iov.iov_len;
    } else {
        sendfrag->frag_send.frag_base.frag_addr = NULL;
        sendfrag->frag_send.frag_base.frag_size = 0;
    }

    /* setup message header */
    hdr = &sendfrag->frag_send.frag_base.frag_header;

    /* first fragment - need to try and match at the receiver */
    if(offset == 0) {
        hdr->hdr_common.hdr_flags = flags;
        hdr->hdr_match.hdr_src = sendreq->req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst = sendreq->req_base.req_peer;
        hdr->hdr_match.hdr_tag = sendreq->req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_bytes_packed;
        hdr->hdr_match.hdr_msg_seq = sendreq->req_base.req_sequence;

        /* for the first 32K - send header for matching + data */
        segments = sendfrag->frag_segments;
        if(sendfrag->frag_send.frag_base.frag_size > 0) {
            sendfrag->frag_segment_count = 2;
        } else {
            sendfrag->frag_segment_count = 1;
        }

        /* if an acknoweldgment is not required - can get by with a shorter header */
        if((flags & MCA_PTL_FLAGS_ACK) == 0) {
            hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
            sendfrag->frag_segments[0].segment_length = sizeof(mca_ptl_base_match_header_t);
            match.lval = MCA_PTL_HDR_TYPE_MATCH;

            /* convert header to network byte order if required */
            if(ptl_peer->peer_nbo) {
                hdr->hdr_common.hdr_flags |= MCA_PTL_FLAGS_NBO;
                MCA_PTL_BASE_MATCH_HDR_HTON(hdr->hdr_match);
            }
        } else {
            hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_RNDV;
            hdr->hdr_rndv.hdr_frag_length = sendfrag->frag_send.frag_base.frag_size;
            hdr->hdr_rndv.hdr_src_ptr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
            hdr->hdr_rndv.hdr_src_ptr.pval = sendfrag;
            sendfrag->frag_segments[0].segment_length = sizeof(mca_ptl_base_rendezvous_header_t);
            match.lval = MCA_PTL_HDR_TYPE_RNDV;

            /* convert header to network byte order if required */
            if(ptl_peer->peer_nbo) {
                hdr->hdr_common.hdr_flags |= MCA_PTL_FLAGS_NBO;
                MCA_PTL_BASE_RNDV_HDR_HTON(hdr->hdr_rndv);
            }
        }

    /* non-zero offset - fragment of a previously started message */
    } else {
        hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FRAG;
        hdr->hdr_common.hdr_flags = flags;
        hdr->hdr_frag.hdr_frag_offset = offset;
        hdr->hdr_frag.hdr_frag_length = sendfrag->frag_send.frag_base.frag_size;
        hdr->hdr_frag.hdr_src_ptr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
        hdr->hdr_frag.hdr_src_ptr.pval = sendfrag;
        hdr->hdr_frag.hdr_dst_ptr = sendreq->req_peer_match;
        match.sval.uval = sendreq->req_peer_match.ival;
        match.sval.lval = offset;

        /* dont send a header for after the first 32K - MX currently doesn't
         * support DMA of more than one segment.
        */
        segments = sendfrag->frag_segments+1;
        sendfrag->frag_segment_count = 1;

        /* convert header to network byte order if required */
        if(ptl_peer->peer_nbo) {
            hdr->hdr_common.hdr_flags |= MCA_PTL_FLAGS_NBO;
            MCA_PTL_BASE_FRAG_HDR_HTON(hdr->hdr_frag);
        }
    }

    /* fragment state */
    sendfrag->frag_send.frag_base.frag_owner = &ptl_peer->peer_ptl->super;
    sendfrag->frag_send.frag_request = sendreq;
    sendfrag->frag_send.frag_base.frag_peer = ptl_peer;

    /* must update the offset after actual fragment size is determined 
     * before attempting to send the fragment
     */
    mca_pml_base_send_request_offset(sendreq,
        sendfrag->frag_send.frag_base.frag_size);

    /* start the fragment */
    mx_return = mx_isend(
        mx_ptl->mx_endpoint,
        segments,
        sendfrag->frag_segment_count,
        ptl_peer->peer_addr,
        match.lval,
        sendfrag,
        &sendfrag->frag_request);
    if(mx_return != MX_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_send: mx_isend() failed with return value=%d\n", mx_return);
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}
                                                                                                   

/**
 * PML->PTL Initiate a send to the peer.
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param request (IN)           Send request
 * @param offset                 Current offset into packed/contiguous buffer.
 * @param size (IN)              Number of bytes PML is requesting PTL to deliver,
 * @param flags (IN)             Flags that should be passed to the peer via the message header.
 * @param request (OUT)          OMPI_SUCCESS if the PTL was able to queue one or more fragments
 *
 * Continue sending fragments of a large message to the peer.
 */
                                                                                                   
int mca_ptl_mx_send_continue(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_pml_base_send_request_t* sendreq,
    size_t offset,
    size_t size,
    int flags)
{
    mca_ptl_mx_module_t* mx_ptl = (mca_ptl_mx_module_t*)ptl;
    mca_ptl_mx_send_frag_t* sendfrag;
    mx_return_t mx_return;
    ompi_ptr_t match;
    ompi_convertor_t *convertor;
    struct iovec iov;
    unsigned int iov_count;
    unsigned int max_data;
    int rc;

    /* allocate fragment */
    MCA_PTL_MX_SEND_FRAG_ALLOC(sendfrag, rc);
    if(rc != OMPI_SUCCESS) {
        return rc;
    }
    sendfrag->frag_free = 0;

    /* initialize convertor */
    convertor = &sendfrag->frag_send.frag_base.frag_convertor;
    ompi_convertor_copy(&sendreq->req_convertor, convertor);
    ompi_convertor_init_for_send(
        convertor,
        0,
        sendreq->req_datatype,
        sendreq->req_count,
        sendreq->req_addr,
        offset,
        mca_ptl_mx_alloc );
                                                                                                                      
    /* if data is contigous convertor will return an offset
     * into users buffer - otherwise will return an allocated buffer
     * that holds the packed data
     */
    iov.iov_base = NULL;
    iov.iov_len = size;
    iov_count = 1;
    max_data = size;
    if((rc = ompi_convertor_pack(
        convertor,
        &iov,
        &iov_count,
        &max_data,
        &(sendfrag->frag_free))) < 0) {
        return OMPI_ERROR;
    }

    sendfrag->frag_segments[0].segment_length = sizeof(mca_ptl_base_frag_header_t);
    sendfrag->frag_segments[1].segment_ptr = iov.iov_base;
    sendfrag->frag_segments[1].segment_length = iov.iov_len;
    sendfrag->frag_segment_count = 1;
    sendfrag->frag_send.frag_base.frag_addr = iov.iov_base;
    sendfrag->frag_send.frag_base.frag_header.hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FRAG;
    sendfrag->frag_send.frag_base.frag_header.hdr_common.hdr_flags = 0;
    sendfrag->frag_send.frag_base.frag_header.hdr_frag.hdr_frag_length = iov.iov_len;
    sendfrag->frag_send.frag_base.frag_header.hdr_frag.hdr_frag_offset = offset;

    /* fragment state */
    sendfrag->frag_send.frag_base.frag_owner = &ptl_peer->peer_ptl->super;
    sendfrag->frag_send.frag_request = sendreq;
    sendfrag->frag_send.frag_base.frag_size = size;
    sendfrag->frag_send.frag_base.frag_peer = ptl_peer;
    sendfrag->frag_progress = 0;

    /* must update the offset after actual fragment size is determined 
     * before attempting to send the fragment
     */
    mca_pml_base_send_request_offset(sendreq, size);

    /* start the fragment */
    match.sval.uval = sendreq->req_peer_match.ival;
    match.sval.lval = offset;
    mx_return = mx_isend(
        mx_ptl->mx_endpoint,
        sendfrag->frag_segments+1,
        1,
        sendfrag->frag_send.frag_base.frag_peer->peer_addr,
        match.lval,
        sendfrag,
        &sendfrag->frag_request);
    if(mx_return != MX_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_send: mx_isend() failed with return value=%d\n", mx_return);
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

/**
 * PML->PTL Notification from the PML to the PTL that a receive
 * has been posted and matched against the indicated fragment.
 *
 * @param ptl (IN)       PTL instance
 * @param recv_frag      Matched fragment
 *
 * The ptl_matched() function is called by the PML when a fragment
 * is matched to a posted receive. This may occur during a call to
 * ptl_match() if the receive is matched, or at a later point in time
 * when a matching receive is posted.
 *
 * When this routine is called, the PTL is responsible for generating
 * an acknowledgment to the peer if the MCA_PTL_FLAGS_ACK
 * bit is set in the original fragment header. Additionally, the PTL
 * is responsible for transferring any data associated with the fragment
 * into the users buffer utilizing the datatype engine, and notifying
 * the PML that the fragment has completed via the ptl_recv_progress()
 * function.
 */

void mca_ptl_mx_matched( 
    mca_ptl_base_module_t* ptl,
    mca_ptl_base_recv_frag_t* frag)
{
    mca_ptl_base_header_t* hdr = &frag->frag_base.frag_header;
    mca_pml_base_recv_request_t* request = frag->frag_request;
    mca_ptl_mx_module_t* mx_ptl = (mca_ptl_mx_module_t*)ptl;
    mca_ptl_mx_recv_frag_t* mx_frag = (mca_ptl_mx_recv_frag_t*)frag;
    unsigned int bytes_delivered = mx_frag->frag_size;
    bool ack_pending = false;

    /* generate an acknowledgment if required */
    if(hdr->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK) {
        int rc;
        mca_ptl_mx_send_frag_t* ack;
        MCA_PTL_MX_SEND_FRAG_ALLOC(ack, rc);
        if(NULL == ack) {
            OMPI_THREAD_LOCK(&mca_ptl_mx_component.mx_lock);
            ack_pending = true;
            ompi_list_append(&mca_ptl_mx_component.mx_pending_acks, (ompi_list_item_t*)frag);
            OMPI_THREAD_UNLOCK(&mca_ptl_mx_component.mx_lock);
        } else {
            mx_return_t mx_return;
            MCA_PTL_MX_SEND_FRAG_INIT_ACK(ack, ptl, mx_frag);
            if(hdr->hdr_common.hdr_flags & MCA_PTL_FLAGS_NBO) {
                MCA_PTL_BASE_ACK_HDR_HTON(ack->frag_send.frag_base.frag_header.hdr_ack);
            }

            /* start the fragment */
            mx_return = mx_isend(
                mx_ptl->mx_endpoint,
                ack->frag_segments,
                ack->frag_segment_count,
                ack->frag_send.frag_base.frag_peer->peer_addr,
                MCA_PTL_HDR_TYPE_ACK,
                ack,
                &ack->frag_request);
            if(mx_return != MX_SUCCESS) {
                ompi_output(0, "mca_ptl_mx_matched: mx_isend() failed with return value=%d\n", mx_return);
                OMPI_THREAD_LOCK(&mca_ptl_mx_component.mx_lock);
                ack_pending = true;
                ompi_list_append(&mca_ptl_mx_component.mx_pending_acks, (ompi_list_item_t*)frag);
                OMPI_THREAD_UNLOCK(&mca_ptl_mx_component.mx_lock);
            }
        }
    }

    /* copy data into users buffer */
    if(mx_frag->frag_size > 0) {
        struct iovec iov;
        unsigned int iov_count = 1;
        int free_after = 0;
        ompi_proc_t *proc = ompi_comm_peer_lookup(request->req_base.req_comm,
            request->req_base.req_ompi.req_status.MPI_SOURCE);
        ompi_convertor_t* convertor = &frag->frag_base.frag_convertor;

        /* initialize receive convertor */
        ompi_convertor_copy(proc->proc_convertor, convertor);
        ompi_convertor_init_for_recv(
            convertor,                      /* convertor */
            0,                              /* flags */
            request->req_base.req_datatype, /* datatype */
            request->req_base.req_count,    /* count elements */
            request->req_base.req_addr,     /* users buffer */
            0,                              /* offset in bytes into packed buffer */
            NULL );                         /* not allocating memory */
        /*ompi_convertor_get_packed_size(convertor, &request->req_bytes_packed); */

        iov.iov_base = mx_frag->frag_data;
        iov.iov_len = mx_frag->frag_size;
        ompi_convertor_unpack(convertor, &iov, &iov_count, &bytes_delivered, &free_after );
    }

    /* update request status */
    ptl->ptl_recv_progress(
        ptl,
        request,
        mx_frag->frag_size,
        bytes_delivered);

    /* release resources */
    if(ack_pending == false)
        MCA_PTL_MX_RECV_FRAG_RETURN(mx_frag);
}

