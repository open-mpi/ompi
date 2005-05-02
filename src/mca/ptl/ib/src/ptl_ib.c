/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
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

#include "ompi_config.h"
#include <string.h>
#include "util/output.h"
#include "util/if.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_ib.h"

mca_ptl_ib_module_t mca_ptl_ib_module = {
    {
        &mca_ptl_ib_component.super,
        1, /* max size of request cache */
        sizeof(mca_ptl_ib_send_frag_t), /* bytes required by ptl for a request */
        0, /* max size of first fragment */
        0, /* min fragment size */
        0, /* max fragment size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        MCA_PTL_PUT,  /* ptl flags */
        mca_ptl_ib_add_procs,
        mca_ptl_ib_del_procs,
        mca_ptl_ib_finalize,
        mca_ptl_ib_send,
        mca_ptl_ib_put,
        NULL,
        mca_ptl_ib_matched,
        mca_ptl_ib_request_init,
        mca_ptl_ib_request_fini,
        NULL,
        NULL,
        NULL
    }
};

int mca_ptl_ib_add_procs(
    struct mca_ptl_base_module_t* ptl, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_ptl_base_peer_t** peers, 
    ompi_bitmap_t* reachable)
{
    mca_ptl_ib_module_t* ib_ptl = (mca_ptl_ib_module_t*)ptl;
    int i, rc;

    for(i = 0; i < nprocs; i++) {

        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_ptl_ib_proc_t* ib_proc;
        mca_ptl_base_peer_t* ib_peer;

        if(NULL == (ib_proc = mca_ptl_ib_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this PTL instance to the proc.
         */

        OMPI_THREAD_LOCK(&ib_proc->proc_lock);

        /* The ptl_proc datastructure is shared by all IB PTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the ptl_proc.
         */
        ib_peer = OBJ_NEW(mca_ptl_ib_peer_t);
        if(NULL == ib_peer) {
            OMPI_THREAD_UNLOCK(&module_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        ib_peer->peer_ptl = ib_ptl;
        rc = mca_ptl_ib_proc_insert(ib_proc, ib_peer);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(ib_peer);
            OMPI_THREAD_UNLOCK(&module_proc->proc_lock);
            continue;
        }

        ompi_bitmap_set_bit(reachable, i);
        OMPI_THREAD_UNLOCK(&module_proc->proc_lock);
        peers[i] = ib_peer;
    }

    return OMPI_SUCCESS;
}

int mca_ptl_ib_del_procs(struct mca_ptl_base_module_t* ptl, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
        struct mca_ptl_base_peer_t ** peers)
{
    /* Stub */
    D_PRINT("Stub\n");
    return OMPI_SUCCESS;
}

int mca_ptl_ib_finalize(struct mca_ptl_base_module_t* ptl)
{
    /* Stub */
    D_PRINT("Stub\n");
    return OMPI_SUCCESS;
}

int mca_ptl_ib_request_init( struct mca_ptl_base_module_t* ptl,
    struct mca_pml_base_send_request_t* request)
{
    mca_ptl_ib_module_t* ib_ptl = (mca_ptl_ib_module_t*)ptl;
    mca_ptl_ib_send_frag_t* sendfrag;
    ompi_list_item_t* item;
    int rc;

    OMPI_FREE_LIST_GET(&ib_ptl->send_free, item, rc);
    if(NULL == (sendfrag = (mca_ptl_ib_send_frag_t*)item)) {
        return rc;
    }
    ((mca_ptl_ib_send_request_t*) request)->req_frag = sendfrag;
    return OMPI_SUCCESS;
}


void mca_ptl_ib_request_fini( struct mca_ptl_base_module_t* ptl,
    struct mca_pml_base_send_request_t* request)
{
    mca_ptl_ib_module_t* ib_ptl = (mca_ptl_ib_module_t*)ptl;  
    mca_ptl_ib_send_request_t* sendreq = (mca_ptl_ib_send_request_t*)request;
    OMPI_FREE_LIST_RETURN(&ib_ptl->send_free, (ompi_list_item_t*)sendreq->req_frag);
}

/*
 *  Initiate a send. If this is the first fragment, use the fragment
 *  descriptor allocated with the send requests, otherwise obtain
 *  one from the free list. Initialize the fragment and foward
 *  on to the peer.
 */

int mca_ptl_ib_send( struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_pml_base_send_request_t* sendreq,
    size_t offset,
    size_t size,
    int flags)
{
    mca_ptl_ib_module_t* ib_ptl = (mca_ptl_ib_module_t*)ptl;
    mca_ptl_ib_send_frag_t* sendfrag;
    mca_ptl_base_header_t *hdr;
    size_t hdr_length;
    int rc = OMPI_SUCCESS;

    if(sendreq->req_cached) {
        sendfrag = ((mca_ptl_ib_send_request_t*)sendreq)->req_frag;
    } else {
        ompi_list_item_t* item;
        OMPI_FREE_LIST_GET(&ib_ptl->send_free, item, rc);
        if(NULL == (sendfrag = (mca_ptl_ib_send_frag_t*)item)) {
            return rc;
        }
    }

    /* initialize convertor */
    if(size > 0) {
        ompi_convertor_t *convertor;
        int rc, freeAfter;
        unsigned int iov_count, max_data;
        struct iovec iov;

        /* first fragment (eager send) and first fragment of long
         * protocol can use the convertor initialized on the request,
         * remaining fragments must copy/reinit the convertor as the
         * transfer could be in parallel.
         */
        if( offset <= mca_ptl_ib_module.super.ptl_first_frag_size ) {
            convertor = &sendreq->req_convertor;
        } else {
            convertor = &sendfrag->frag_send.frag_base.frag_convertor;
            ompi_convertor_copy(&sendreq->req_convertor, convertor);
            ompi_convertor_init_for_send( convertor,
                                          0,
                                          sendreq->req_base.req_datatype,
                                          sendreq->req_base.req_count,
                                          sendreq->req_base.req_addr,
                                          offset,
                                          NULL );
        }

        /* if data is contigous, convertor will return an offset
         * into users buffer - otherwise will return an allocated buffer
         * that holds the packed data
         */
        if((flags & MCA_PTL_FLAGS_ACK) == 0) {
            iov.iov_base = &sendfrag->ib_buf.buf[sizeof(mca_ptl_base_match_header_t)];
        } else {
            iov.iov_base = &sendfrag->ib_buf.buf[sizeof(mca_ptl_base_rendezvous_header_t)];
        }
        iov.iov_len = size;
        iov_count = 1;
        max_data = size;

        if((rc = ompi_convertor_pack(convertor,&iov, &iov_count, &max_data, &freeAfter)) < 0) {
            ompi_output(0, "Unable to pack data");
            return rc;
        }

        /* adjust size to reflect actual number of bytes packed by convertor */
        size = iov.iov_len;
        sendfrag->frag_send.frag_base.frag_addr = iov.iov_base;
        sendfrag->frag_send.frag_base.frag_size = iov.iov_len;
    } else {
        sendfrag->frag_send.frag_base.frag_addr = NULL;
        sendfrag->frag_send.frag_base.frag_size = 0;
    }

    /* fragment state */
    sendfrag->frag_send.frag_base.frag_owner = &ptl_peer->peer_ptl->super;
    sendfrag->frag_send.frag_request = sendreq;
    sendfrag->frag_send.frag_base.frag_peer = ptl_peer;
    sendfrag->frag_progressed = 0;

    /* Initialize header */
    hdr = (mca_ptl_base_header_t *) &sendfrag->ib_buf.buf[0];
    hdr->hdr_common.hdr_flags = flags;
    hdr->hdr_match.hdr_contextid = sendreq->req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_dst = sendreq->req_base.req_peer;
    hdr->hdr_match.hdr_tag = sendreq->req_base.req_tag;
    hdr->hdr_match.hdr_msg_length = sendreq->req_bytes_packed;
    hdr->hdr_match.hdr_msg_seq = sendreq->req_base.req_sequence;
    if((flags & MCA_PTL_FLAGS_ACK) == 0) {
        hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
        hdr_length = sizeof(mca_ptl_base_match_header_t);
    } else {
        hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
        hdr->hdr_rndv.hdr_frag_length = sendfrag->frag_send.frag_base.frag_size;
        hdr->hdr_rndv.hdr_src_ptr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
        hdr->hdr_rndv.hdr_src_ptr.pval = sendfrag;
        hdr_length = sizeof(mca_ptl_base_rendezvous_header_t);
    }

    /* Update the offset after actual fragment size is determined,
     * and before attempting to send the fragment */
    sendreq->req_offset += size;

    IB_SET_SEND_DESC_LEN((&sendfrag->ib_buf), (hdr_length + size));
    if(OMPI_SUCCESS != (rc = mca_ptl_ib_peer_send(ptl_peer, sendfrag))) {
        return rc;
    }

    /* if this is the entire message - signal request is complete */
    if(sendreq->req_bytes_packed == size) {
        ompi_request_complete( &(sendreq->req_base.req_ompi) );
    }
    return OMPI_SUCCESS;
}

/*
 * RDMA local buffer to remote buffer address.
 */

int mca_ptl_ib_put( struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_pml_base_send_request_t* req, size_t offset,
    size_t size, int flags)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


/*
 * On a match send an ack to the peer.
 */

static void mca_ptl_ib_ack(
    mca_ptl_ib_module_t *ib_ptl,
    mca_ptl_ib_send_frag_t *send_frag,
    mca_ptl_ib_recv_frag_t *recv_frag)
{
    mca_ptl_base_header_t *hdr;
    mca_pml_base_recv_request_t *request;
    mca_ptl_ib_peer_t *ib_peer;
    ib_buffer_t *ib_buf;
    int recv_len;
    int len_to_reg, len_added = 0;
    void *addr_to_reg, *ack_buf;

    /* Header starts at beginning of registered
     * buffer space */

    hdr = (mca_ptl_base_header_t *)
        &send_frag->ib_buf.buf[0];

    request = recv_frag->super.frag_request;

    /* Amount of data we have already received */
    recv_len = 
        recv_frag->super.frag_base.frag_header.hdr_rndv.hdr_frag_length;

    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_ACK;
    hdr->hdr_common.hdr_flags = 0;

    /* Remote side send descriptor */
    hdr->hdr_ack.hdr_src_ptr =
        recv_frag->super.frag_base.frag_header.hdr_rndv.hdr_src_ptr;

    /* Matched request from recv side */
    hdr->hdr_ack.hdr_dst_match.lval = 0;
    hdr->hdr_ack.hdr_dst_match.pval = request;

    hdr->hdr_ack.hdr_dst_addr.lval = 0;

    addr_to_reg = (void*)((char*)request->req_base.req_addr + recv_len);
    hdr->hdr_ack.hdr_dst_addr.pval = addr_to_reg;

    len_to_reg = request->req_bytes_packed - recv_len;
    hdr->hdr_ack.hdr_dst_size = len_to_reg;

    A_PRINT("Dest addr : %p, RDMA Len : %d",
            hdr->hdr_ack.hdr_dst_addr.pval,
            hdr->hdr_ack.hdr_dst_size);

    ack_buf = (void*) ((char*) (&send_frag->ib_buf.buf[0]) + 
        sizeof(mca_ptl_base_ack_header_t));

    /* Prepare ACK packet with IB specific stuff */
    mca_ptl_ib_prepare_ack(ib_ptl, addr_to_reg, len_to_reg,
            ack_buf, &len_added);

    /* Send it right away! */
    ib_peer = (mca_ptl_ib_peer_t *)
        recv_frag->super.frag_base.frag_peer;

    ib_buf = &send_frag->ib_buf;

    IB_SET_SEND_DESC_LEN(ib_buf,
            (sizeof(mca_ptl_base_ack_header_t) + len_added));

    mca_ptl_ib_post_send(ib_ptl, ib_peer, &send_frag->ib_buf, send_frag);

    /* fragment state */
    send_frag->frag_send.frag_base.frag_owner = &ib_ptl->super;
    send_frag->frag_send.frag_base.frag_peer = recv_frag->super.frag_base.frag_peer;
    send_frag->frag_send.frag_base.frag_addr = NULL;
    send_frag->frag_send.frag_base.frag_size = 0;
}

/*
 *  A posted receive has been matched - if required send an
 *  ack back to the peer and process the fragment. Copy the
 *  data to user buffer
 */

void mca_ptl_ib_matched(
    mca_ptl_base_module_t* ptl,
    mca_ptl_base_recv_frag_t* frag)
{
    mca_ptl_ib_module_t* ib_ptl = (mca_ptl_ib_module_t*)ptl;
    mca_pml_base_recv_request_t *request;
    mca_ptl_base_header_t *header;
    mca_ptl_ib_recv_frag_t *recv_frag;

    header  = &frag->frag_base.frag_header;
    request = frag->frag_request;
    recv_frag = (mca_ptl_ib_recv_frag_t*) frag;

    D_PRINT("Matched frag\n");

    if (header->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK) {
        mca_ptl_ib_send_frag_t *send_frag;
        send_frag = mca_ptl_ib_alloc_send_frag(ib_ptl, NULL);
        if(NULL == send_frag) {
            ompi_output(0, "Cannot get send descriptor");
        } else {
            mca_ptl_ib_ack(ib_ptl, send_frag, recv_frag);
        }
    }

    /* Process the fragment */

    /* IN TCP case, IO_VEC is first allocated.
     * then recv the data, and copy if needed,
     * But in ELAN cases, we save the data into an
     * unex buffer if the recv descriptor is not posted
     * (for too long) (TODO).
     * We then need to copy from
     * unex_buffer to application buffer */

    if ((header->hdr_common.hdr_type & MCA_PTL_HDR_TYPE_MATCH) &&
        (header->hdr_match.hdr_msg_length > 0)) {
        struct iovec iov;
        ompi_proc_t *proc;
	unsigned int iov_count, max_data;
	int freeAfter;

        iov.iov_base = frag->frag_base.frag_addr;
        iov.iov_len  = frag->frag_base.frag_size;

        proc = ompi_comm_peer_lookup(request->req_base.req_comm,
                request->req_base.req_ompi.req_status.MPI_SOURCE);

        ompi_convertor_copy(proc->proc_convertor, &frag->frag_base.frag_convertor);

        ompi_convertor_init_for_recv( &frag->frag_base.frag_convertor,
				      0,
				      request->req_base.req_datatype,
				      request->req_base.req_count,
				      request->req_base.req_addr,
                      0, /* fragment offset */
				      NULL );
        ompi_convertor_unpack(&frag->frag_base.frag_convertor, &iov, &iov_count, &max_data, &freeAfter);
    }
    mca_ptl_ib_recv_frag_done(header, frag, request);
}
