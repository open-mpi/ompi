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
#include "ptl_elan_req.h"
#include "ptl_elan_priv.h"

static void
mca_ptl_elan_init_qdma_desc (struct ompi_ptl_elan_qdma_desc_t *desc,
                             mca_ptl_elan_t * ptl,
			     struct mca_ptl_elan_peer_t *ptl_peer,
                             mca_pml_base_send_request_t *pml_req,
			     size_t offset,
			     size_t size,
			     int flags)
{
    char       *app_buff;
    int         header_length;
    int         mesg_length;
    mca_ptl_base_header_t *hdr;

    int         destvp;
   
    START_FUNC();

    destvp = ptl_peer->peer_vp;

    /* TODO: For now, assume data are contiguous and less than eager size */
    app_buff = (char *) pml_req->super.req_addr;

    header_length = sizeof (mca_ptl_base_match_header_t);
    mesg_length = pml_req->req_bytes_packed;

    hdr = (mca_ptl_base_header_t *) & desc->buff[0];

    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
    /*hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FRAG;*/
    hdr->hdr_common.hdr_flags = flags;
    hdr->hdr_common.hdr_size = sizeof (mca_ptl_base_match_header_t);
    hdr->hdr_frag.hdr_frag_offset = 0;
    hdr->hdr_frag.hdr_frag_seq = 0;
    hdr->hdr_frag.hdr_src_ptr.pval = 0;
    hdr->hdr_frag.hdr_dst_ptr.lval = 0;

    hdr->hdr_match.hdr_contextid = pml_req->super.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = pml_req->super.req_comm->c_my_rank;
    hdr->hdr_match.hdr_dst = pml_req->super.req_peer;
    hdr->hdr_match.hdr_tag = pml_req->super.req_tag;
    hdr->hdr_match.hdr_msg_length = mesg_length;
    hdr->hdr_match.hdr_msg_seq = pml_req->super.req_sequence;
    hdr->hdr_frag.hdr_frag_length = mesg_length;

    /* Fill up all the necessary fields */
    memcpy (&desc->buff[header_length], app_buff, mesg_length);
    desc->main_dma.dma_srcAddr = MAIN2ELAN (desc->rail->r_ctx,
                                            &desc->buff[0]);

    /* XXXX Hardwired DMA retry count */
    desc->main_dma.dma_typeSize = E4_DMA_TYPE_SIZE ((header_length +
                                                     mesg_length),
                                                    DMA_DataTypeByte,
                                                    DMA_QueueWrite, 16);

    desc->main_dma.dma_cookie =
        elan4_local_cookie (ptl->queue->tx_cpool, 
		E4_COOKIE_TYPE_LOCAL_DMA, destvp);

    if (CHECK_ELAN)
    {
	char hostname[32];
	gethostname(hostname, 32);

	fprintf(stderr, 
		"[%s send...] destvp %d type %d flag %d size %d\n",
		hostname, destvp,
		hdr->hdr_common.hdr_type,
		hdr->hdr_common.hdr_flags,
		hdr->hdr_common.hdr_size);
    }
	
    desc->main_dma.dma_vproc = destvp;

    /* Make main memory coherent with IO domain (IA64) */
    MEMBAR_VISIBLE ();
    END_FUNC();
}


int
mca_ptl_elan_start_desc (mca_ptl_elan_desc_item_t * desc,
			 struct mca_ptl_elan_peer_t *ptl_peer,
			 struct mca_pml_base_send_request_t *sendreq,
			 size_t offset,
			 size_t size,
			 int flags)
{
    mca_ptl_elan_t *ptl;

    START_FUNC();

    if (desc->desc->desc_type == MCA_PTL_ELAN_QDMA_DESC) {
        struct ompi_ptl_elan_qdma_desc_t *qdma;

        qdma = (ompi_ptl_elan_qdma_desc_t *)desc->desc;
        ptl = qdma->ptl;
	
	/* Could assert the following is the same as sendreq */
        /*req = qdma->req;*/

        mca_ptl_elan_init_qdma_desc (qdma, ptl, ptl_peer, sendreq, 
		offset, size, flags);

        elan4_run_dma_cmd (ptl->queue->tx_cmdq, (DMA *) & qdma->main_dma);

	/*ptl->queue->tx_cmdq->cmdq_flush */
	elan4_flush_cmdq_reorder (ptl->queue->tx_cmdq);

        /* Insert desc into the list of outstanding DMA's */
        ompi_list_append (&ptl->queue->tx_desc, (ompi_list_item_t *) desc);

    } else {
        ompi_output (0,
                     "Other types of DMA are not supported right now \n");
        return OMPI_ERROR;
    }

    END_FUNC();
    return OMPI_SUCCESS;
}


static void
mca_ptl_elan_data_frag (struct mca_ptl_elan_t *ptl,
                        mca_ptl_base_header_t * header)
{
    /* For PML interfacing, refer to mca_ptl_tcp_recv_frag_match(frag, sd);*/
    /* Allocate a recv frag descriptor */
    mca_ptl_elan_recv_frag_t *recv_frag;
    ompi_list_item_t *item;
    mca_pml_base_recv_request_t *request;

    int         rc;

    OMPI_FREE_LIST_GET (&mca_ptl_elan_module.elan_recv_frags_free,
                        item, rc);

    if (OMPI_SUCCESS != rc) {
        ompi_output (0,
                     "[%s:%d] Unable to allocate a recv fragment",
                     __FILE__, __LINE__);
        return;
        /* TODO: progress the recv state machine */
    }

    recv_frag = (mca_ptl_elan_recv_frag_t *) item;
    recv_frag->super.super.frag_owner = (mca_ptl_t *) ptl;
    recv_frag->super.super.frag_addr = NULL;
    recv_frag->super.super.frag_size = 0;

    /* XXX: 
     * Since elan is not connection oriented, 
     * No information about which peer until checking the header
     */
    recv_frag->super.super.frag_peer = NULL;    
    recv_frag->super.frag_request = 0;
    recv_frag->super.frag_is_buffered = false;
    recv_frag->frag_hdr_cnt = 0;
    recv_frag->frag_msg_cnt = 0;

    /* Take the header */
    recv_frag->super.super.frag_header = *header;

    /* match with preposted requests */
    if (mca_ptl_base_recv_frag_match (recv_frag->super.super.frag_owner,
                                      &recv_frag->super,
                                      &recv_frag->super.super.frag_header.
                                      hdr_match)) {
	/*mca_ptl_tcp_recv_frag_matched(frag);*/
        /* copy into the request buffer */
        request = recv_frag->super.frag_request;
	if (header->hdr_frag.hdr_frag_length > 0) {
	    memcpy (request->super.req_addr,
		    (char *) header + sizeof (mca_ptl_base_header_t),
		    header->hdr_frag.hdr_frag_length);
	}
        recv_frag->super.frag_is_buffered = false ;
	recv_frag->super.super.frag_addr = request->super.req_addr;
	recv_frag->super.super.frag_size = header->hdr_frag.hdr_frag_length;
    } else {
	/* XXX: Fragment is buffered */
        recv_frag->super.frag_is_buffered = true;
        recv_frag->super.super.frag_addr = recv_frag->unex_buff;
        recv_frag->super.super.frag_size =
            header->hdr_frag.hdr_frag_length;
        memcpy (recv_frag->unex_buff,
                (char *) header + sizeof (mca_ptl_base_header_t),
                header->hdr_frag.hdr_frag_length);
    }

    /* Complete the fragment */
    if (fetchNset (&recv_frag->frag_progressed, 1) == 0 && 
	    NULL != recv_frag->super.frag_request) {
        mca_ptl_base_recv_progress_fn_t progress;

        progress = recv_frag->super.super.frag_owner->ptl_recv_progress;
        request = recv_frag->super.frag_request;

        /* progress the request */
        progress (recv_frag->super.super.frag_owner, request,
                  &recv_frag->super);

	/* FIXME: 
	 * To support the required ACK, do not return
	 * until the ack is out */
        mca_ptl_elan_recv_frag_return (recv_frag->super.super.frag_owner,
                                       recv_frag);
    }
}

static void
mca_ptl_elan_ctrl_frag (struct mca_ptl_elan_t *ptl,
                        mca_ptl_base_header_t * header)
{
    /* TODO:
     * 0) First of all, no need to allocate frag descriptors, 
     *    since control packet does not contain data.
     * 1) Start successive fragments if it is an ACK
     * 2) Resend the original fragment if it is a NACK 
     * 3) Update the request if no more fragment.
     */
}

int
mca_ptl_elan_drain_recv (mca_ptl_elan_module_1_0_0_t * emp)
{
    struct ompi_ptl_elan_queue_ctrl_t *queue;
    ompi_ptl_elan_recv_queue_t *rxq;
    struct mca_ptl_elan_t *ptl;
    ELAN_CTX   *ctx;

    int         num_ptls;
    int         i;
    int         rc;

    START_FUNC();
    num_ptls = emp->elan_num_ptls;

    /* Iterate over all the PTL input Queues */
    for (i = 0; i < num_ptls; i++) {

        ptl = emp->elan_ptls[i];
        queue = emp->elan_ptls[i]->queue;
        rxq = queue->rxq;
        ctx = ptl->ptl_elan_ctx;

        OMPI_LOCK (&queue->rx_lock);

        rc = elan4_pollevent_word (ctx, &rxq->qr_doneWord, 0);

        if (rc) {

            mca_ptl_base_header_t *header;

            header = (mca_ptl_base_header_t *) rxq->qr_fptr;

	    if (CHECK_ELAN)
	    {
		char hostname[32];
		gethostname(hostname, 32);

		fprintf(stderr, 
			"[%s recv...] type %x flag %x size %x\n",
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
            MEMBAR_STORESTORE ();

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

    END_FUNC();
    return OMPI_SUCCESS;
}

int
mca_ptl_elan_update_send (mca_ptl_elan_module_1_0_0_t * emp)
{
    struct mca_ptl_elan_t *ptl;
    ompi_ptl_elan_queue_ctrl_t *queue;
    mca_ptl_elan_desc_item_t *desc;
    ELAN4_CTX  *ctx;

    int         num_ptls;
    int         i;

    START_FUNC();

    num_ptls = emp->elan_num_ptls;

    /* Update the send request if any of send's is completed */
    for (i = 0; i < num_ptls; i++) {
        ptl = emp->elan_ptls[i];
        queue = ptl->queue;
        ctx = ptl->ptl_elan_ctx;

	while (ompi_list_get_size (&queue->tx_desc) > 0) {
            desc = (mca_ptl_elan_desc_item_t *)
                ompi_list_get_first (&queue->tx_desc);

#if 0
            if ((int *) (&desc->desc->main_doneWord))
#else
            /* Poll the completion event for 1usec */
            if (elan4_pollevent_word
                (ctx, &desc->desc->main_doneWord, 1))
#endif
            {
                mca_ptl_elan_send_request_t *req;
                /* Remove the desc, update the request, put back to free list */
                desc = (mca_ptl_elan_desc_item_t *)
                    ompi_list_remove_first (&queue->tx_desc);
                req = desc->desc->req;

		if(NULL == req ) { /*(IS_ACK)*/
		    OMPI_FREE_LIST_RETURN (&queue->tx_desc_free,
			    (ompi_list_item_t *) desc);
		} else if (1) { //NO_NEED_FOR_MATCH || ALREADY_MATCHED) 
		    /* XXX: 
		     * mca_pml_base_send_request_matched(request)
		     * hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
		     * Be sure to add an atomic lock for threaded cases */

		    mca_ptl_base_send_frag_t frag;
		    frag.super.frag_size = 4;
		    req->super.req_bytes_sent = req->super.req_bytes_packed;
		    ptl->super.ptl_send_progress(ptl, req, &frag); 

		    /* the first fragment is allocated with the 
		     * request, all others need to be returned 
		     * to the free list  */ 
		    if (1) { // NOT_FIRST_FRAG
		       	OMPI_FREE_LIST_RETURN (&queue->tx_desc_free,
			       	(ompi_list_item_t *) desc);
		    }
		}

            } else {
                break;
            }
        } 
    }

    END_FUNC();
    return OMPI_SUCCESS;
}
