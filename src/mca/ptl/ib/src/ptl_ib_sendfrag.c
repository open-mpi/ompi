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
#include "include/types.h"
#include "datatype/datatype.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_ib.h"
#include "ptl_ib_peer.h"
#include "ptl_ib_proc.h"
#include "ptl_ib_sendfrag.h"
#include "ptl_ib_priv.h"
#include "ptl_ib_memory.h"

static void mca_ptl_ib_send_frag_construct(mca_ptl_ib_send_frag_t* frag);
static void mca_ptl_ib_send_frag_destruct(mca_ptl_ib_send_frag_t* frag);

OBJ_CLASS_INSTANCE(mca_ptl_ib_send_frag_t, 
        mca_ptl_base_send_frag_t,
        mca_ptl_ib_send_frag_construct, 
        mca_ptl_ib_send_frag_destruct);

/*
 * Placeholders for send fragment constructor/destructors.
 */

static void mca_ptl_ib_send_frag_construct(mca_ptl_ib_send_frag_t* frag)
{
    frag->frag_progressed = 0;
    frag->frag_ack_pending = 0;
}

static void mca_ptl_ib_send_frag_destruct(mca_ptl_ib_send_frag_t* frag)
{
}

int mca_ptl_ib_send_frag_init(mca_ptl_ib_send_frag_t* sendfrag,
        struct mca_ptl_base_peer_t* ptl_peer,
        struct mca_pml_base_send_request_t* sendreq,
        size_t offset,
        size_t* size,
        int flags)
{
    size_t size_in = *size;
    size_t size_out;
    mca_ptl_base_rendezvous_header_t *hdr;
    struct iovec iov;
    int header_length;

    /* Start of the IB buffer */
    hdr = (mca_ptl_base_rendezvous_header_t *) &sendfrag->ib_buf.buf[0];

    /* Fill up the header for PML to make a match */
    if(offset == 0) { 
        hdr->hdr_match.hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
        hdr->hdr_match.hdr_common.hdr_flags = flags;
        hdr->hdr_src_ptr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */

        /* Ptr to send frag, so incoming ACK can locate the frag */
        hdr->hdr_src_ptr.pval = sendfrag;

        hdr->hdr_match.hdr_contextid = sendreq->req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst = sendreq->req_base.req_peer;
        hdr->hdr_match.hdr_tag = sendreq->req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_bytes_packed;
        hdr->hdr_match.hdr_msg_seq = sendreq->req_base.req_sequence;

        header_length = sizeof(mca_ptl_base_rendezvous_header_t);

    } else {

        hdr->hdr_match.hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FRAG;
        hdr->hdr_match.hdr_common.hdr_flags = flags;
        hdr->hdr_src_ptr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
        hdr->hdr_src_ptr.pval = sendfrag;

        header_length = sizeof(mca_ptl_base_rendezvous_header_t);
    }

    /* initialize convertor */
    if(size_in > 0) {
        ompi_convertor_t *convertor;
        int rc, freeAfter;
	unsigned int iov_count, max_data;

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
        iov.iov_base = &sendfrag->ib_buf.buf[header_length];
        iov.iov_len = size_in;
	iov_count = 1;
	max_data = size_in;

        if((rc = ompi_convertor_pack(convertor,&iov, &iov_count, &max_data, &freeAfter)) < 0) {

            ompi_output(0, "Unable to pack data");

            return OMPI_ERROR;
        }

        /* adjust size and request offset to reflect actual 
         * number of bytes packed by convertor */
        size_out = iov.iov_len;
        IB_SET_SEND_DESC_LEN((&sendfrag->ib_buf),
               (header_length + iov.iov_len));
    } else {
        size_out = size_in;
        IB_SET_SEND_DESC_LEN((&sendfrag->ib_buf),
               (header_length + size_in));
    }

    hdr->hdr_frag_length = size_out;

    /* fragment state */
    sendfrag->frag_send.frag_base.frag_owner = 
        &ptl_peer->peer_module->super;
    sendfrag->frag_send.frag_request = sendreq;

    sendfrag->frag_send.frag_base.frag_addr = iov.iov_base;
    sendfrag->frag_send.frag_base.frag_size = size_out;

    sendfrag->frag_send.frag_base.frag_peer = ptl_peer;
    sendfrag->frag_progressed = 0;

    *size = size_out;

    return OMPI_SUCCESS;
}

/*
 * Allocate a IB send descriptor
 *
 */
mca_ptl_ib_send_frag_t* mca_ptl_ib_alloc_send_frag(
        mca_ptl_base_module_t* ptl,
        mca_pml_base_send_request_t* request)
{
    ompi_free_list_t *flist;
    ompi_list_item_t *item;
    mca_ptl_ib_send_frag_t *ib_send_frag;

    flist = &((mca_ptl_ib_module_t *)ptl)->send_free;

    item = ompi_list_remove_first(&((flist)->super));

    while(NULL == item) {

        mca_ptl_tstamp_t tstamp = 0;

        D_PRINT("Gone one NULL descriptor ... trying again");

        ptl->ptl_component->ptlm_progress (tstamp);
        item = ompi_list_remove_first (&((flist)->super));
    }

    ib_send_frag = (mca_ptl_ib_send_frag_t *)item;

    B_PRINT("Allocated frag : %p", ib_send_frag);

    return ib_send_frag;
}


int mca_ptl_ib_register_send_frags(mca_ptl_base_module_t *ptl)
{
    int i, rc, num_send_frags;
    ompi_list_item_t *item;
    ompi_free_list_t *flist;
    ib_buffer_t *ib_buf_ptr;
    mca_ptl_ib_send_frag_t *ib_send_frag;
    mca_ptl_ib_state_t *ib_state;

    flist = &((mca_ptl_ib_module_t *)ptl)->send_free;

    ib_state = ((mca_ptl_ib_module_t *)ptl)->ib_state;

    num_send_frags = ompi_list_get_size(&(flist->super));

    item = ompi_list_get_first(&((flist)->super));

    /* Register the buffers */
    for(i = 0; i < num_send_frags; 
            item = ompi_list_get_next(item), i++) {

        ib_send_frag = (mca_ptl_ib_send_frag_t *) item;

        ib_send_frag->frag_progressed = 0;

        ib_buf_ptr = (ib_buffer_t *) &ib_send_frag->ib_buf;

        rc = mca_ptl_ib_register_mem(ib_state->nic, ib_state->ptag,
                (void*) ib_buf_ptr->buf, 
                MCA_PTL_IB_FIRST_FRAG_SIZE,
                &ib_buf_ptr->hndl);
        if(rc != OMPI_SUCCESS) {
            return OMPI_ERROR;
        }

        IB_PREPARE_SEND_DESC(ib_buf_ptr, 0, 
                MCA_PTL_IB_FIRST_FRAG_SIZE, ib_buf_ptr);
    }

    return OMPI_SUCCESS;
}

/*
 * Process RDMA Write completions
 *
 * Just return send fragment to free list
 */

void mca_ptl_ib_process_rdma_w_comp(mca_ptl_base_module_t *module,
        void* comp_addr)
{
    mca_ptl_ib_module_t *ib_module = (mca_ptl_ib_module_t *)module;
    mca_ptl_ib_send_frag_t *sendfrag = (mca_ptl_ib_send_frag_t *) comp_addr;
    
    /* deregister memory region for RDMA write */
    mca_ptl_ib_deregister_mem_with_registry(ib_module->ib_state,
        (void *)(unsigned long)(sendfrag->ib_buf.desc.sg_entry.addr),
        (size_t)(sendfrag->ib_buf.desc.sg_entry.len));
    
#if 0
    mca_ptl_ib_send_frag_t *sendfrag;
    ompi_free_list_t *flist;

    A_PRINT("Free RDMA send descriptor : %p", comp_addr);

    sendfrag = (mca_ptl_ib_send_frag_t *) comp_addr;

    flist = &(sendfrag->
            frag_send.frag_base.frag_peer->
            peer_module->send_free);

    OMPI_FREE_LIST_RETURN(flist, 
            ((ompi_list_item_t *) sendfrag));

#endif
}

/*
 * Process send completions
 *
 */

void mca_ptl_ib_process_send_comp(mca_ptl_base_module_t *module, 
        void* addr)
{
    mca_ptl_ib_send_frag_t *sendfrag;
    mca_ptl_base_header_t *header;
    mca_pml_base_send_request_t *req;
    ompi_free_list_t *flist;

    sendfrag = (mca_ptl_ib_send_frag_t *) addr;
    header = (mca_ptl_base_header_t *) sendfrag->ib_buf.buf;

    req = (mca_pml_base_send_request_t *) 
        sendfrag->frag_send.frag_request;

    flist = &(sendfrag->
            frag_send.frag_base.frag_peer->
            peer_module->send_free);

    if(header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_ACK) {
        /* Is this an ack descriptor ? */
        A_PRINT("Completion of send_ack");

        OMPI_FREE_LIST_RETURN(flist, 
                ((ompi_list_item_t *) sendfrag));

    } else if(header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_FIN) {

        A_PRINT("Completion of fin");

        module->ptl_send_progress(module,
                sendfrag->frag_send.frag_request,
                header->hdr_frag.hdr_frag_length);

        OMPI_FREE_LIST_RETURN(flist, 
                ((ompi_list_item_t *) sendfrag));
        
    } else if(NULL == req) {
        /* An ack descriptor ? Don't know what to do! */
        OMPI_FREE_LIST_RETURN(flist, 
                ((ompi_list_item_t *) sendfrag));
    } else if (0 == (header->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK)
            || mca_pml_base_send_request_matched(req)) {

        module->ptl_send_progress(module,
                sendfrag->frag_send.frag_request,
                header->hdr_rndv.hdr_frag_length);
        /* Return sendfrag to free list */

        B_PRINT("Return frag : %p", sendfrag);

        OMPI_FREE_LIST_RETURN(flist, 
                ((ompi_list_item_t *) sendfrag));
    } else {
        /* Not going to call progress on this send,
         * and not free-ing descriptor */
        A_PRINT("Why should I return sendfrag?");
    }
}

int mca_ptl_ib_put_frag_init(mca_ptl_ib_send_frag_t *sendfrag, 
        mca_ptl_base_peer_t *ptl_peer,
        mca_pml_base_send_request_t *req, 
        size_t offset, size_t *size, int flags)
{
    int rc;
    int size_in, size_out;
    mca_ptl_ib_fin_header_t *hdr;

    size_in = *size;

    hdr = (mca_ptl_ib_fin_header_t *)
        &sendfrag->ib_buf.buf[0];

    hdr->frag_hdr.hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FIN;
    hdr->frag_hdr.hdr_common.hdr_flags = flags;
    hdr->frag_hdr.hdr_frag_offset = offset;
    hdr->frag_hdr.hdr_src_ptr.lval = 0;
    hdr->frag_hdr.hdr_src_ptr.pval = sendfrag;
    hdr->frag_hdr.hdr_dst_ptr = req->req_peer_match;
    hdr->frag_hdr.hdr_frag_length = size_in;
    hdr->mr_addr.lval = req->req_peer_addr.lval;
    hdr->mr_size = req->req_peer_size;

    if(size_in > 0 && 0) {
        struct iovec iov;
        ompi_convertor_t *convertor;
	unsigned int iov_count, max_data;
	int freeAfter;

        if( offset <= mca_ptl_ib_module.super.ptl_first_frag_size) {
            convertor = &req->req_convertor;
        } else {
            convertor = &sendfrag->frag_send.frag_base.frag_convertor;
            ompi_convertor_copy(&req->req_convertor, convertor);
            ompi_convertor_init_for_send( convertor,
					  0,
					  req->req_base.req_datatype,
					  req->req_base.req_count,
					  req->req_base.req_addr,
					  offset,
					  NULL );
        }
        iov.iov_base = &sendfrag->ib_buf.buf[sizeof(mca_ptl_ib_fin_header_t)];
        iov.iov_len  = size_in;
	iov_count = 1;
	max_data = size_in;

        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &freeAfter);
        if (rc < 0) {
            ompi_output (0, "[%s:%d] Unable to pack data\n",
                    __FILE__, __LINE__);
            return rc;
        }
        size_out = iov.iov_len;
    } else {
        size_out = size_in;
    }

    *size = size_out;
    hdr->frag_hdr.hdr_frag_length = size_out;

    IB_SET_SEND_DESC_LEN((&sendfrag->ib_buf),
            (sizeof(mca_ptl_ib_fin_header_t)));

    /* fragment state */
    sendfrag->frag_send.frag_base.frag_owner = 
        &ptl_peer->peer_module->super;
    sendfrag->frag_send.frag_request = req;

    sendfrag->frag_send.frag_base.frag_addr = 
        &sendfrag->ib_buf.buf[sizeof(mca_ptl_ib_fin_header_t)];

    sendfrag->frag_send.frag_base.frag_size = size_out;

    sendfrag->frag_send.frag_base.frag_peer = ptl_peer;
    sendfrag->frag_progressed = 0;

    return OMPI_SUCCESS;
}
