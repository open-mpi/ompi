/*
 * $HEADER$
 */

#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "types.h"
#include "datatype/datatype.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "ptl_elan.h"
#include "ptl_elan_peer.h"
#include "ptl_elan_proc.h"
#include "ptl_elan_frag.h"
#include "ptl_elan_priv.h"

static void
mca_ptl_elan_data_frag (struct mca_ptl_elan_module_t *ptl,
                        mca_ptl_base_header_t * header)
{
    /* Allocate a recv frag descriptor */
    mca_ptl_elan_recv_frag_t *recv_frag;
    ompi_list_item_t *item;

    bool        matched; 
    int         rc = OMPI_SUCCESS;

    START_FUNC(PTL_ELAN_DEBUG_RECV);

    OMPI_FREE_LIST_GET (&mca_ptl_elan_component.elan_recv_frags_free, 
                        item, rc);

    while (OMPI_SUCCESS != rc) {

	/* TODO: progress the recv state machine */
        ompi_output (0,
                     "[%s:%d] Retry to allocate a recv fragment",
                     __FILE__, __LINE__);
	OMPI_FREE_LIST_GET (&mca_ptl_elan_component.elan_recv_frags_free, 
                item, rc);
    } 

    recv_frag = (mca_ptl_elan_recv_frag_t *) item;
    recv_frag->frag_recv.frag_base.frag_owner = (mca_ptl_base_module_t *) ptl;
    /* XXX: 
     * Another problem caused by TCP oriented PML.
     * a) Since elan is not connection oriented, 
     *    No information about which peer until checking the header
     *    Somewhere after the frag is matched, this peer information needs
     *    to be filled in so that ACK can be sent out.
     *
     * b) Possibly, another drawback of hooking the ack to the particular 
     *    recv fragment. If the ack fragment is not hooked this way,
     *    PML will provide the peer information when the ack is requested.
     * 
     * c) What if the recv request specifies MPI_ANY_SOURCE, then 
     *    for the handshaking to complete, peer should be fixed the
     *    handshaking. Then in this case, PML needs information from
     *    PTL to know about which peer this data is from.
     *    So PTL has to provide the peer information to PML.
     */
    recv_frag->frag_recv.frag_base.frag_peer = NULL;
    recv_frag->frag_recv.frag_request = NULL;
    recv_frag->frag_recv.frag_is_buffered = false;
    recv_frag->frag_hdr_cnt = 0;
    recv_frag->frag_msg_cnt = 0;
    recv_frag->frag_ack_pending = false;
    recv_frag->frag_progressed = 0;

    /* Copy the header, mca_ptl_base_match() does not do what it claims */
    recv_frag->frag_recv.frag_base.frag_header = *header;

    /* Taking the data starting point be default */
    recv_frag->frag_recv.frag_base.frag_addr = 
	(char *) header + sizeof (mca_ptl_base_header_t);
    recv_frag->frag_recv.frag_base.frag_size = header->hdr_frag.hdr_frag_length;

    /* match with preposted requests */
    matched = mca_ptl_base_recv_frag_match (
	    recv_frag->frag_recv.frag_base.frag_owner,
	    &recv_frag->frag_recv,
	    &recv_frag->frag_recv.frag_base.frag_header.hdr_match); 
    
    if (!matched) {
	/* Buffer the fragment into unex buffer
	 * TODO: 
	 *   Next version need to do this only when it is 
	 *   blocking the recv queue */
        memcpy (recv_frag->unex_buff,
                (char *) header + sizeof (mca_ptl_base_header_t),
                header->hdr_frag.hdr_frag_length);
        recv_frag->frag_recv.frag_is_buffered = true;
        recv_frag->frag_recv.frag_base.frag_addr = recv_frag->unex_buff;
    } 
    END_FUNC(PTL_ELAN_DEBUG_RECV);
}


static void
mca_ptl_elan_last_frag (struct mca_ptl_elan_module_t *ptl,
                        mca_ptl_base_header_t * hdr)
{
    mca_pml_base_recv_request_t *request; 

    START_FUNC(PTL_ELAN_DEBUG_RECV);
    request = (mca_pml_base_recv_request_t*) hdr->hdr_frag.hdr_dst_ptr.pval; 
    /*pml_req->req_peer_match;*/

    if (PTL_ELAN_DEBUG_FLAG & PTL_ELAN_DEBUG_ACK) {
	char hostname[32];
	gethostname(hostname, 32);
	fprintf(stderr, "[%s][%s:%d] local req %p addr %p, length %d\n", 
		hostname, __FUNCTION__, __LINE__,
		request, request->req_base.req_addr,
		hdr->hdr_frag.hdr_frag_length);
    }

    ptl->super.ptl_recv_progress (ptl, request, 
	    hdr->hdr_frag.hdr_frag_length,
	    hdr->hdr_frag.hdr_frag_length);

    END_FUNC(PTL_ELAN_DEBUG_RECV);
}

static void
mca_ptl_elan_ctrl_frag (struct mca_ptl_elan_module_t *ptl,
                        mca_ptl_base_header_t * header)
{
     /* TODO:
      * 0) First of all, no need to allocate recv frag descriptors, 
      *    since control packet does not contain data.
      * 1) Update the send request, which will start successive fragments 
      *    if it is an ACK and there are more data to go.
      * XXX: Resend if it is a NACK, which layer PML/PTL to handle that?
      */
     mca_ptl_elan_send_frag_t* desc;
     mca_pml_base_send_request_t* req;

     START_FUNC(PTL_ELAN_DEBUG_ACK);
 
     desc = (mca_ptl_elan_send_frag_t*) header->hdr_ack.hdr_src_ptr.pval;
     req = (mca_pml_base_send_request_t *) desc->desc->req;

     req->req_peer_match = header->hdr_ack.hdr_dst_match;
     req->req_peer_addr  = header->hdr_ack.hdr_dst_addr;
     req->req_peer_size  = header->hdr_ack.hdr_dst_size;

    if (PTL_ELAN_DEBUG_FLAG & PTL_ELAN_DEBUG_ACK) {
	char hostname[32];
	gethostname(hostname, 32);
	fprintf(stderr, "[%s][%s:%d] remote req %p addr %p, length %d\n", 
		hostname, __FUNCTION__, __LINE__,
		req->req_peer_match.pval, 
		req->req_peer_addr.pval, 
		req->req_peer_size);
    }

     /* FIXME: 
      * This sort of synchronized fragment release will lead
      * to race conditions, also see the note insize the follwoing routine */
     mca_ptl_elan_send_desc_done (desc, req); 
     END_FUNC(PTL_ELAN_DEBUG_RECV);
}

static void
mca_ptl_elan_init_qdma_desc (struct mca_ptl_elan_send_frag_t *frag,
                             mca_ptl_elan_module_t * ptl,
			     struct mca_ptl_elan_peer_t *ptl_peer,
                             mca_pml_base_send_request_t *pml_req,
			     size_t offset,
			     size_t *size,
			     int flags)
{
    int         header_length;
    int         destvp;
    int         size_out;
    int         size_in;
    int         rc = OMPI_SUCCESS;
   
    mca_ptl_base_header_t *hdr;
    struct ompi_ptl_elan_qdma_desc_t * desc;
   
    START_FUNC(PTL_ELAN_DEBUG_NONE);

    desc   = (ompi_ptl_elan_qdma_desc_t *)frag->desc;
    destvp = ptl_peer->peer_vp;
    size_in = *size;

    hdr = (mca_ptl_base_header_t *) & desc->buff[0];

    if(offset == 0) {
	hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
	hdr->hdr_common.hdr_flags = flags;
	hdr->hdr_common.hdr_size = sizeof (mca_ptl_base_match_header_t);
	hdr->hdr_frag.hdr_frag_offset = offset;
	hdr->hdr_frag.hdr_frag_seq = 0;
        hdr->hdr_frag.hdr_src_ptr.lval = 0;
	/* Frag descriptor, so that incoming ack will locate it */
	hdr->hdr_frag.hdr_src_ptr.pval = frag; 
	hdr->hdr_frag.hdr_dst_ptr.lval = 0;

	hdr->hdr_match.hdr_contextid = pml_req->req_base.req_comm->c_contextid;
	hdr->hdr_match.hdr_src = pml_req->req_base.req_comm->c_my_rank;
	hdr->hdr_match.hdr_dst = pml_req->req_base.req_peer;
	hdr->hdr_match.hdr_tag = pml_req->req_base.req_tag;
	hdr->hdr_match.hdr_msg_length = pml_req->req_bytes_packed;
	hdr->hdr_match.hdr_msg_seq = pml_req->req_base.req_sequence;
	header_length = sizeof (mca_ptl_base_match_header_t);
    } else {
	hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FRAG;
	hdr->hdr_common.hdr_flags = flags;
	hdr->hdr_common.hdr_size = sizeof (mca_ptl_base_frag_header_t);
	hdr->hdr_frag.hdr_frag_offset = offset;
	hdr->hdr_frag.hdr_frag_seq = 0;
        hdr->hdr_frag.hdr_src_ptr.lval = 0;
	hdr->hdr_frag.hdr_src_ptr.pval = frag; /* Frag descriptor */
        hdr->hdr_frag.hdr_dst_ptr = pml_req->req_peer_match;
	header_length = sizeof (mca_ptl_base_frag_header_t);
    }

    if (PTL_ELAN_DEBUG_FLAG & PTL_ELAN_DEBUG_SEND) {                    
	char hostname[32]; gethostname(hostname, 32);    
	fprintf(stderr, "[%s:%s:%d] frag %p req %p \n",
	    hostname, __FUNCTION__, __LINE__,
	    hdr->hdr_frag.hdr_src_ptr.pval,
	    hdr->hdr_frag.hdr_dst_ptr.pval);           
    }                                                    

    /* initialize convertor */
    if(size_in > 0) {
	struct iovec iov;
        ompi_convertor_t *convertor;

        if( offset <= mca_ptl_elan_module.super.ptl_first_frag_size ) {
            convertor = &pml_req->req_convertor;
        } else {
            convertor = &frag->frag_base.frag_convertor;
            ompi_convertor_copy(&pml_req->req_convertor, convertor);
            ompi_convertor_init_for_send( 
                convertor,
                0, 
                pml_req->req_base.req_datatype,
                pml_req->req_base.req_count,
                pml_req->req_base.req_addr,
                offset);
        }

	/* For now, eager sends are always packed into the descriptor
         * TODO: Inline up to 256 bytes (including the header), then
	 *       do a chained send for mesg < first_frag_size */
        iov.iov_base = &desc->buff[header_length];
        iov.iov_len  = size_in;
        rc = ompi_convertor_pack(convertor, &iov, 1);
       	if (rc < 0) {
	    ompi_output (0, "[%s:%d] Unable to pack data\n",
			 __FILE__, __LINE__);
            return;
	}
        size_out = iov.iov_len;
    } else {
	size_out = size_in;
    }

    *size = size_out;
    hdr->hdr_frag.hdr_frag_length = size_out;

    /* TODO: 
     *     For now just save the information to the provided header 
     *     Later will use the inline header to report the progress */
    frag->frag_base.frag_header = *hdr; 

    desc->main_dma.dma_srcAddr = MAIN2ELAN (desc->ptl->ptl_elan_ctx, 
                                            &desc->buff[0]);

    /* XXX: Hardcoded DMA retry count */
    desc->main_dma.dma_typeSize = E4_DMA_TYPE_SIZE ((header_length +
                                                     size_out),
                                                    DMA_DataTypeByte,
                                                    DMA_QueueWrite, 16);

    desc->main_dma.dma_cookie =
        elan4_local_cookie (ptl->queue->tx_cpool, 
		E4_COOKIE_TYPE_LOCAL_DMA, destvp);

    if (PTL_ELAN_DEBUG_FLAG & PTL_ELAN_DEBUG_SEND) {
	char hostname[32];

	gethostname(hostname, 32);
	fprintf(stderr, "[%s send...] destvp %d type %d flag %d size %d\n",
		hostname, destvp, hdr->hdr_common.hdr_type,
		hdr->hdr_common.hdr_flags,
		hdr->hdr_common.hdr_size);
    }
	
    desc->main_dma.dma_vproc = destvp;

    /* Make main memory coherent with IO domain (IA64) */
    MEMBAR_VISIBLE ();
    END_FUNC(PTL_ELAN_DEBUG_NONE);
}

static void
mca_ptl_elan_init_putget_desc (struct mca_ptl_elan_send_frag_t *frag,
                               mca_ptl_elan_module_t * ptl,
			       struct mca_ptl_elan_peer_t *ptl_peer,
                               mca_pml_base_send_request_t *pml_req,
			       size_t offset,
			       size_t *size,
			       int flags)
{
    int         destvp;
    int         size_out;
    int         size_in;
    int         rc = OMPI_SUCCESS;
    ELAN4_CTX   *ctx;

    struct ompi_ptl_elan_putget_desc_t * desc;

    mca_ptl_base_header_t *hdr;

    START_FUNC((PTL_ELAN_DEBUG_PUT | PTL_ELAN_DEBUG_GET));

    if (PTL_ELAN_DEBUG_FLAG & (PTL_ELAN_DEBUG_PUT|PTL_ELAN_DEBUG_GET)) {
	char hostname[32]; gethostname(hostname, 32);    
	fprintf(stderr, "[%s:%s:%d] frag %p ptl %p ptl_peer %p req %p "
		"offset %d size %d flags %d \n",     
	    hostname, __FUNCTION__, __LINE__,
	    frag, ptl, ptl_peer, pml_req, offset, *size, flags);           
    }

    hdr = &frag->frag_base.frag_header;
    desc   = (ompi_ptl_elan_putget_desc_t *)frag->desc;
    destvp = ptl_peer->peer_vp;
    size_in = *size;
    ctx =  ptl->ptl_elan_ctx;

    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_LAST;
    hdr->hdr_common.hdr_flags = flags;
    hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_frag_header_t);
    hdr->hdr_frag.hdr_frag_offset = offset;
    hdr->hdr_frag.hdr_frag_seq = 0;
    hdr->hdr_frag.hdr_src_ptr.lval = 0; 
    hdr->hdr_frag.hdr_src_ptr.pval = frag; /* No need to hook a frag */
    hdr->hdr_frag.hdr_dst_ptr = pml_req->req_peer_match;
    hdr->hdr_frag.hdr_frag_length = size_in;

    desc->src_elan_addr = elan4_main2elan (ctx, pml_req->req_base.req_addr);
    desc->dst_elan_addr = (E4_Addr)pml_req->req_peer_addr.lval;
    desc->desc_buff = hdr;

    if (PTL_ELAN_DEBUG_FLAG & PTL_ELAN_DEBUG_PUT) {
	char hostname[32];
	gethostname(hostname, 32);
	fprintf(stderr, "[%s][%s:%d] remote req %p addr %x, length %d\n", 
		hostname, __FUNCTION__, __LINE__,
		pml_req->req_peer_match.pval, 
		pml_req->req_peer_addr.lval, 
		pml_req->req_peer_size);
    }

    /* initialize convertor */
    if(size_in > 0 && 0) {
	struct iovec iov;
        ompi_convertor_t *convertor;

        if( offset <= mca_ptl_elan_module.super.ptl_first_frag_size ) {
            convertor = &pml_req->req_convertor;
        } else {
            convertor = &frag->frag_base.frag_convertor;
            ompi_convertor_copy(&pml_req->req_convertor, convertor);
            ompi_convertor_init_for_send( 
                convertor,
                0, 
                pml_req->req_base.req_datatype,
                pml_req->req_base.req_count,
                pml_req->req_base.req_addr,
                offset);
        }

	/* For now, eager sends are always packed into the descriptor
         * TODO: Inline up to 256 bytes (including the header), then
	 *       do a chained send for mesg < first_frag_size */

	/*desc->src_elan_addr = elan4_main2elan(ctx, desc->desc_buff);*/
        iov.iov_base = desc->desc_buff;
        iov.iov_len  = size_in;

        rc = ompi_convertor_pack(convertor, &iov, 1);
       	if (rc < 0) {
	    ompi_output (0, "[%s:%d] Unable to pack data\n",
			 __FILE__, __LINE__);
            return;
	}
        size_out = iov.iov_len;
    } else {
	size_out = size_in;
    }

    *size = size_out;
    hdr->hdr_frag.hdr_frag_length = size_out;

    /* XXX: no additional flags for the DMA, remote, shmem, qwrite, 
     *      broadcast, etc */
    flags = 0;

    /* 
     * FIXME: 
     *   Be sure to correctly setup a chained DMA.
     */

    /* Setup the chain dma */
    desc->chain_dma.dma_typeSize = E4_DMA_TYPE_SIZE (
	    sizeof(mca_ptl_base_frag_header_t), 
	    DMA_DataTypeByte, DMA_QueueWrite, 8);
    desc->chain_dma.dma_cookie   = elan4_local_cookie(ptl->putget->pg_cpool,
	    E4_COOKIE_TYPE_LOCAL_DMA, destvp);
    desc->chain_dma.dma_vproc    = destvp;
    desc->chain_dma.dma_srcAddr  = elan4_main2elan (ctx, (void *) hdr);
    desc->chain_dma.dma_dstAddr  = 0x0ULL; 
    desc->chain_dma.dma_srcEvent = elan4_main2elan (ctx, desc->elan_event);

    /* causes the inputter to redirect the dma to the inputq */
    desc->chain_dma.dma_dstEvent = elan4_main2elan (ctx, 
	    (void *) ptl->queue->input);

    INITEVENT_WORD (ctx, (E4_Event *) desc->elan_event, &desc->main_doneWord);
    RESETEVENT_WORD (&desc->main_doneWord);

    /* Be sure that padding E4_Event is not causing problems */
    PRIMEEVENT_WORD (ctx, (E4_Event *)desc->elan_event, 1);

    desc->chain_dma.dma_typeSize |= RUN_DMA_CMD;
    desc->chain_dma.dma_pad       = NOP_CMD;

    if (PTL_ELAN_DEBUG_PUT & PTL_ELAN_DEBUG_FLAG) {
	char hostname[32]; gethostname(hostname, 32);    
	fprintf(stderr, "[%s:%s:%d] desc %p chain_buff %p chain_event %p "
		"src_addr %x dst_addr %x size %d\n",
		hostname, __FUNCTION__, __LINE__,
		desc, desc->chain_buff, desc->chain_event,
		(int)desc->src_elan_addr, 
		(int)desc->dst_elan_addr, size_out);           
    }

    /* Copy down the chain dma to the chain buffer in elan sdram  */
    memcpy ((void *)desc->chain_buff, (void *)&desc->chain_dma, 
	    sizeof (E4_DMA64));

    desc->chain_event->ev_CountAndType = E4_EVENT_INIT_VALUE(-32,
	    E4_EVENT_COPY, E4_EVENT_DTYPE_LONG, 8);
    desc->chain_event->ev_Params[0]    = elan4_main2elan (ctx, 
	    (void *)desc->chain_buff);
    /* XXX: 
     * The chain dma will go directly into a command stream
     * so we need addend the command queue control bits.
     * Allocate space from command queues hanged off the CTX.
     */
    desc->chain_event->ev_Params[1] = elan4_alloccq_space (ctx, 8, CQ_Size8K);

    desc->main_dma.dma_srcAddr = desc->src_elan_addr;
    desc->main_dma.dma_dstAddr = desc->dst_elan_addr;

    /* Chain an event */
    desc->main_dma.dma_srcEvent= elan4_main2elan(ctx, 
	    (E4_Event *)desc->chain_event);
    desc->main_dma.dma_dstEvent= 0x0ULL; /*disable remote event */

    /* XXX: Hardcoded DMA retry count */
    desc->main_dma.dma_typeSize = E4_DMA_TYPE_SIZE (size_out, 
	    DMA_DataTypeByte, flags, ptl->putget->pg_retryCount);

    /* Just a normal DMA, no need to have additional flags */
    desc->main_dma.dma_cookie = elan4_local_cookie (
	    ptl->putget->pg_cpool, 
	    E4_COOKIE_TYPE_LOCAL_DMA, 
	    destvp);
    desc->main_dma.dma_vproc = destvp;

    if (PTL_ELAN_DEBUG_FLAG & PTL_ELAN_DEBUG_PUT) {
	char hostname[32];
	gethostname(hostname, 32);
	fprintf(stderr, "[%s send...] destvp %d type %d flag %d size %d\n",
		hostname, destvp, hdr->hdr_common.hdr_type,
		hdr->hdr_common.hdr_flags,
		hdr->hdr_common.hdr_size);
    }

    /* Make main memory coherent with IO domain (IA64) */
    MEMBAR_VISIBLE ();
    END_FUNC(PTL_ELAN_DEBUG_NONE);
}

int
mca_ptl_elan_start_desc (mca_ptl_elan_send_frag_t * desc,
			 struct mca_ptl_elan_peer_t *ptl_peer,
			 struct mca_pml_base_send_request_t *sendreq,
			 size_t offset,
			 size_t *size,
			 int flags)
{
    mca_ptl_elan_module_t *ptl;

    ptl = ptl_peer->peer_ptl;

    START_FUNC(PTL_ELAN_DEBUG_NONE);

    if (desc->desc->desc_type == MCA_PTL_ELAN_DESC_QDMA) {
        struct ompi_ptl_elan_qdma_desc_t *qdma;

        qdma = (ompi_ptl_elan_qdma_desc_t *)desc->desc;
	
        mca_ptl_elan_init_qdma_desc (desc, ptl, ptl_peer, sendreq, 
		offset, size, flags);
        elan4_run_dma_cmd (ptl->queue->tx_cmdq, (DMA *) & qdma->main_dma);
	/*ptl->queue->tx_cmdq->cmdq_flush */
	elan4_flush_cmdq_reorder (ptl->queue->tx_cmdq);

        /* Insert desc into the list of outstanding DMA's */
        ompi_list_append (&ptl->queue->tx_desc, (ompi_list_item_t *) desc);

    } else if (MCA_PTL_ELAN_DESC_PUT == desc->desc->desc_type) {

        struct ompi_ptl_elan_putget_desc_t *pdesc;

        pdesc = (ompi_ptl_elan_putget_desc_t *)desc->desc;

	/* For each put/get descriptor, a QDMA is chained off. */
        mca_ptl_elan_init_putget_desc (desc, ptl, ptl_peer, sendreq, 
		offset, size, flags);

        elan4_run_dma_cmd (ptl->putget->put_cmdq, (E4_DMA *) &pdesc->main_dma);

	/*ptl->queue->tx_cmdq->cmdq_flush */
	elan4_flush_cmdq_reorder (ptl->putget->put_cmdq);

        /* Insert desc into the list of outstanding DMA's */
        ompi_list_append (&ptl->putget->put_desc, (ompi_list_item_t *) desc);
    } else {
        ompi_output (0, "To support GET and Other types of DMA "
	       "are not supported right now \n");
        return OMPI_ERROR;
    }

    /* fragment state */
    desc->frag_base.frag_owner = (struct mca_ptl_base_module_t *) 
	&ptl_peer->peer_ptl->super;
    desc->frag_base.frag_peer = (struct mca_ptl_base_peer_t *) ptl_peer;
    desc->frag_base.frag_addr = NULL;
    desc->frag_base.frag_size = *size;
    desc->frag_progressed  = 0;
    desc->frag_ack_pending = 0; /* this is ack for internal elan */

    END_FUNC(PTL_ELAN_DEBUG_NONE);
    return OMPI_SUCCESS;
}

/* Initialize an ack descriptor and queue it to the command queue */
int
mca_ptl_elan_start_ack ( mca_ptl_base_module_t * ptl, 
			 mca_ptl_elan_send_frag_t * desc,
			 mca_ptl_elan_recv_frag_t * recv_frag)
{
    struct ompi_ptl_elan_qdma_desc_t *qdma;
    mca_ptl_base_header_t *hdr;
    mca_pml_base_recv_request_t* request;
    mca_ptl_elan_module_t *elan_ptl;

    int destvp;

    START_FUNC(PTL_ELAN_DEBUG_ACK);

    destvp = ((mca_ptl_elan_peer_t *)
	    recv_frag->frag_recv.frag_base.frag_peer)->peer_vp;
	
    elan_ptl = (mca_ptl_elan_module_t *) ptl;
    desc->desc->desc_type = MCA_PTL_ELAN_DESC_QDMA; 
    qdma = (ompi_ptl_elan_qdma_desc_t *)desc->desc;

    hdr = (mca_ptl_base_header_t *) & qdma->buff[0];

    request = recv_frag->frag_recv.frag_request;

    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_ACK;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_ack_header_t);

    /* Remote send fragment descriptor */
    hdr->hdr_ack.hdr_src_ptr = 
	recv_frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_src_ptr;

    /* Matched request from recv side */
    hdr->hdr_ack.hdr_dst_match.lval = 0; 
    hdr->hdr_ack.hdr_dst_match.pval = request;

    /* FIXME: this needs to be some offsete from the base addr */
    hdr->hdr_ack.hdr_dst_addr.pval = 0; 
    hdr->hdr_ack.hdr_dst_addr.lval = elan4_main2elan(
	    elan_ptl->ptl_elan_ctx, request->req_base.req_addr);

    /* FIXME: posted buffer size is the leftover */
    hdr->hdr_ack.hdr_dst_size = 
	request->req_bytes_packed - request->req_bytes_received;

    /* XXX: No need to set the fragment size */
    /*hdr->hdr_common.hdr_frag_length = sizeof(mca_ptl_base_ack_header_t);*/

    if (PTL_ELAN_DEBUG_FLAG & PTL_ELAN_DEBUG_ACK) {                    
	char hostname[32]; gethostname(hostname, 32);    
	fprintf(stderr, "[%s:%s:%d] remote frag %p local req %p buffer %p size %d \n",
	    hostname, __FUNCTION__, __LINE__,
	    hdr->hdr_ack.hdr_src_ptr.pval,
	    hdr->hdr_ack.hdr_dst_match.pval,
	    hdr->hdr_ack.hdr_dst_addr.pval,
	    hdr->hdr_ack.hdr_dst_size);           
    }                                                    

    /* Filling up QDMA descriptor */
    qdma->main_dma.dma_srcAddr = elan4_main2elan(
	    elan_ptl->ptl_elan_ctx, &qdma->buff[0]);

    /* XXX: Hardcoded DMA retry count */
    qdma->main_dma.dma_typeSize = E4_DMA_TYPE_SIZE (
	    sizeof(mca_ptl_base_ack_header_t),
	    DMA_DataTypeByte, DMA_QueueWrite, 16);
    qdma->main_dma.dma_vproc = destvp;
    qdma->main_dma.dma_cookie = elan4_local_cookie (
	    elan_ptl->queue->tx_cpool, 
	    E4_COOKIE_TYPE_LOCAL_DMA, destvp);

    /* Make main memory coherent with IO domain (IA64) */
    MEMBAR_VISIBLE ();

    elan4_run_dma_cmd (elan_ptl->queue->tx_cmdq, (DMA *) & qdma->main_dma);

    /*ptl->queue->tx_cmdq->cmdq_flush */
    elan4_flush_cmdq_reorder (elan_ptl->queue->tx_cmdq);

    /* Insert desc into the list of outstanding DMA's */
    ompi_list_append (&elan_ptl->queue->tx_desc, (ompi_list_item_t *) desc);

    /* fragment state */
    desc->desc->req = NULL;
    desc->frag_base.frag_owner = ptl;
    desc->frag_base.frag_peer = recv_frag->frag_recv.frag_base.frag_peer; 
    desc->frag_base.frag_addr = NULL;
    desc->frag_base.frag_size = 0;
    desc->frag_progressed     = 0;
    desc->desc->desc_status   = MCA_PTL_ELAN_DESC_LOCAL;

    END_FUNC(PTL_ELAN_DEBUG_ACK);
    return OMPI_SUCCESS;
}

int
mca_ptl_elan_drain_recv (mca_ptl_elan_component_t * emp)
{
    struct ompi_ptl_elan_queue_ctrl_t *queue;
    ompi_ptl_elan_recv_queue_t *rxq;
    struct mca_ptl_elan_module_t *ptl;
    ELAN_CTX   *ctx;

    int         num_ptl_modules;
    int         i;
    int         rc;

    START_FUNC(PTL_ELAN_DEBUG_NONE);
    num_ptl_modules = emp->elan_num_ptl_modules;

    /* Iterate over all the PTL input Queues */
    for (i = 0; i < num_ptl_modules; i++) {

        ptl = emp->elan_ptl_modules[i];
        queue = emp->elan_ptl_modules[i]->queue;
        rxq = queue->rxq;
        ctx = ptl->ptl_elan_ctx;

        OMPI_LOCK (&queue->rx_lock);

#if 0
        rc = (*(int *) (&rxq->qr_doneWord));
#else
        rc = elan4_pollevent_word (ctx, &rxq->qr_doneWord, 2000);
#endif
 
        if (rc) {

            mca_ptl_base_header_t *header;

            header = (mca_ptl_base_header_t *) rxq->qr_fptr;

	    if (PTL_ELAN_DEBUG_FLAG & PTL_ELAN_DEBUG_RECV) {
		char hostname[32];
		gethostname(hostname, 32);

		fprintf(stderr, 
			"[%s recv...] type %d flag %d size %d\n",
			hostname,
			header->hdr_common.hdr_type,
			header->hdr_common.hdr_flags,
			header->hdr_common.hdr_size);
	    }
	
            switch (header->hdr_common.hdr_type) {
            case MCA_PTL_HDR_TYPE_MATCH:
            case MCA_PTL_HDR_TYPE_FRAG:
                /* a data fragment */
                mca_ptl_elan_data_frag (ptl, header);
                break;
            case MCA_PTL_HDR_TYPE_ACK:
            case MCA_PTL_HDR_TYPE_NACK:
                /* a control fragment for a message */
                mca_ptl_elan_ctrl_frag (ptl, header);
                break;
            case MCA_PTL_HDR_TYPE_LAST:
                /* a control fragment for a message */
                mca_ptl_elan_last_frag (ptl, header);
		break;
            default:
                fprintf(stdout, "[%s:%d] unknow fragment type %d\n",
                             __FILE__, __LINE__,
                             header->hdr_common.hdr_type);
		fflush(stdout);
                break;
            }

            /* Work out the new front pointer */
            if (rxq->qr_fptr == rxq->qr_top) {
                rxq->qr_fptr = rxq->qr_base;
                rxq->qr_efptr = rxq->qr_efitem;
            } else {
                rxq->qr_fptr = (void *) ((uintptr_t) rxq->qr_fptr
                                         + queue->rx_slotsize);
                rxq->qr_efptr += queue->rx_slotsize;
            }

            /* PCI Write */
            queue->input->q_fptr = rxq->qr_efptr;

            /* Reset the event */
            RESETEVENT_WORD (&rxq->qr_doneWord);

            /* Order RESETEVENT wrt to wait_event_cmd */
            MEMBAR_STORESTORE ();

            /* Re-prime queue event by issuing a waitevent(1) on it */
            elan4_wait_event_cmd (rxq->qr_cmdq,
                    /* Is qr_elanDone really a main memory address? */
                    MAIN2ELAN (ctx, rxq->qr_elanDone),
                    E4_EVENT_INIT_VALUE (-32, E4_EVENT_WRITE,
                        E4_EVENT_DTYPE_LONG, 0), 
                    MAIN2ELAN (ctx, (void *) &rxq->qr_doneWord),
                    0xfeedfacedeadbeef);

            /*rxq->qr_cmdq->cmdq_flush */
	    elan4_flush_cmdq_reorder (rxq->qr_cmdq);

        }
        OMPI_UNLOCK (&queue->rx_lock);
    }

    END_FUNC(PTL_ELAN_DEBUG_NONE);
    return OMPI_SUCCESS;
}

int
mca_ptl_elan_update_desc (mca_ptl_elan_component_t * emp)
{
    struct mca_ptl_elan_module_t *ptl;
    ompi_ptl_elan_queue_ctrl_t *queue;
    mca_ptl_elan_send_frag_t *desc;
    ELAN4_CTX  *ctx;

    int         num_ptl_modules;
    int         i;
    int         rc = 0;

    START_FUNC(PTL_ELAN_DEBUG_NONE);

    num_ptl_modules = emp->elan_num_ptl_modules;

    /* Update the send request if any of send's is completed */
    for (i = 0; i < num_ptl_modules; i++) {

        ptl = emp->elan_ptl_modules[i];
        queue = ptl->queue;
        ctx = ptl->ptl_elan_ctx;

	while (ompi_list_get_size (&queue->tx_desc) > 0) {
            desc = (mca_ptl_elan_send_frag_t *)
                ompi_list_get_first (&queue->tx_desc);
#if 0
            rc = * ((int *) (&desc->desc->main_doneWord));
#else
            /* Poll the completion event for 1usec */
            rc = elan4_pollevent_word(ctx, &desc->desc->main_doneWord, 2000);
#endif
	    if (rc) {
		mca_ptl_base_header_t *header;
                mca_ptl_elan_send_request_t *req;
		struct ompi_ptl_elan_qdma_desc_t *qdma;

                /* Remove the desc, update the request, put back to free list */
                desc = (mca_ptl_elan_send_frag_t *)
                    ompi_list_remove_first (&queue->tx_desc);
		qdma = (ompi_ptl_elan_qdma_desc_t*)desc->desc;

                req = (mca_ptl_elan_send_request_t *)qdma->req;
		header = (mca_ptl_base_header_t *)&qdma->buff[0];

		if (PTL_ELAN_DEBUG_FLAG & PTL_ELAN_DEBUG_SEND) {
		    char hostname[32];
		    gethostname(hostname, 32);

		    fprintf(stderr, 
			    "[%s comp sending...] type %d flag %d size %d\n",
			    hostname,
			    header->hdr_common.hdr_type,
			    header->hdr_common.hdr_flags,
			    header->hdr_common.hdr_size);
		}
 		mca_ptl_elan_send_desc_done (desc, req);

		/* Remember to reset the events */
		INITEVENT_WORD (ctx, qdma->elan_event, &qdma->main_doneWord);
		RESETEVENT_WORD (&qdma->main_doneWord);
		PRIMEEVENT_WORD (ctx, qdma->elan_event, 1);

 	    } else {
 		/* XXX: Stop at any incomplete send desc */
		break;
	    }
	} /* end of the while loop */
    } /* end of the for loop */

    /* Have the putget list checking to be in the same function */
    mca_ptl_elan_update_putget(emp);

    END_FUNC(PTL_ELAN_DEBUG_NONE);
    return OMPI_SUCCESS;
}

int
mca_ptl_elan_update_putget (mca_ptl_elan_component_t * emp)
{
    struct mca_ptl_elan_module_t *ptl;
    ompi_ptl_elan_putget_ctrl_t *putget;
    mca_ptl_elan_send_frag_t *desc;
    ELAN4_CTX  *ctx;

    int         num_ptl_modules;
    int         i;
    int         rc = 0;

    START_FUNC(PTL_ELAN_DEBUG_NONE);

    num_ptl_modules = emp->elan_num_ptl_modules;

    /* Update the send request if any of send's is completed */
    for (i = 0; i < num_ptl_modules; i++) {

        ptl = emp->elan_ptl_modules[i];
        putget = ptl->putget;
        ctx = ptl->ptl_elan_ctx;

	while (ompi_list_get_size (&putget->put_desc) > 0) {
            desc = (mca_ptl_elan_send_frag_t *)
                ompi_list_get_first (&putget->put_desc);
#if 0
            rc = * ((int *) (&desc->desc->main_doneWord));
#else
            /* Poll the completion event for 1usec */
            rc = elan4_pollevent_word(ctx, &desc->desc->main_doneWord, 2000);
#endif
	    if (rc) {
		mca_ptl_base_header_t *header;
                mca_ptl_elan_send_request_t *req;
		struct ompi_ptl_elan_putget_desc_t *pdesc;

                /* Remove the desc, update the request, 
		   put back to free list */
                desc = (mca_ptl_elan_send_frag_t *)
                    ompi_list_remove_first (&putget->put_desc);
		pdesc = (ompi_ptl_elan_putget_desc_t*)desc->desc;

                req = (mca_ptl_elan_send_request_t *)pdesc->req;
		header = (mca_ptl_base_header_t *)pdesc->desc_buff;

		if (PTL_ELAN_DEBUG_FLAG & PTL_ELAN_DEBUG_PUT) {
		    char hostname[32];
		    gethostname(hostname, 32);

		    fprintf(stderr, 
			    "[%s comp sending...] type %d flag %d size %d\n",
			    hostname,
			    header->hdr_common.hdr_type,
			    header->hdr_common.hdr_flags,
			    header->hdr_common.hdr_size);
		}

 		/*mca_ptl_elan_send_desc_done (desc, req);*/
		OMPI_FREE_LIST_RETURN (&putget->put_desc_free,
			(ompi_list_item_t *) desc);
		/* Remember to reset the events */

		INITEVENT_WORD (ctx, pdesc->elan_event, &pdesc->main_doneWord);
		RESETEVENT_WORD (&pdesc->main_doneWord);
		PRIMEEVENT_WORD (ctx, pdesc->elan_event, 1);
	    }
	} /* end of the while loop */
    } /* end of the for loop */

    END_FUNC(PTL_ELAN_DEBUG_NONE);
    return OMPI_SUCCESS;
}

