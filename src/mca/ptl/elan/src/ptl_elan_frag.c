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

#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "ptl_elan.h"

static void
mca_ptl_elan_send_frag_construct (mca_ptl_elan_send_frag_t * frag)
{
    frag->frag_progressed = 0;
    frag->desc = 0;
}

static void
mca_ptl_elan_send_frag_destruct (mca_ptl_elan_send_frag_t * frag)
{
    /* Nothing to do then */
}

ompi_class_t mca_ptl_elan_send_frag_t_class = {
    "mca_ptl_elan_send_frag_t",
    OBJ_CLASS (mca_ptl_base_frag_t),
    (ompi_construct_t) mca_ptl_elan_send_frag_construct,
    (ompi_destruct_t) mca_ptl_elan_send_frag_destruct
};

static void
mca_ptl_elan_recv_frag_construct (mca_ptl_elan_recv_frag_t * frag)
{
    frag->frag_hdr_cnt = 0;
    frag->frag_msg_cnt = 0;
    frag->frag_progressed = 0;

    frag->frag.qdma = NULL;
    frag->alloc_buff = (char *) malloc (sizeof (char) * 2048 + 32);
    if (NULL == frag->alloc_buff) {
        ompi_output (0,
                     "[%s:%d] Fatal error, unable to allocate recv buff \n",
                     __FILE__, __LINE__);
    }
    frag->unex_buff = (char *) (((int) frag->alloc_buff + 32) >> 5 << 5);
}

static void
mca_ptl_elan_recv_frag_destruct (mca_ptl_elan_recv_frag_t * frag)
{
    frag->frag_hdr_cnt = 0;
    frag->frag_msg_cnt = 0;
    frag->frag_progressed = 0;

    frag->frag.qdma = NULL;
    free (frag->alloc_buff);
    frag->alloc_buff = NULL;
    frag->unex_buff = NULL;
}

ompi_class_t mca_ptl_elan_recv_frag_t_class = {
    "mca_ptl_elan_recv_frag_t",
    OBJ_CLASS (mca_ptl_base_recv_frag_t),
    (ompi_construct_t) mca_ptl_elan_recv_frag_construct,
    (ompi_destruct_t) mca_ptl_elan_recv_frag_destruct
};


extern mca_ptl_elan_state_t mca_ptl_elan_global_state;

mca_ptl_elan_send_frag_t *
mca_ptl_elan_alloc_send_desc (struct mca_ptl_base_module_t *ptl_ptr,
                  struct mca_pml_base_send_request_t *sendreq, 
		  int desc_type)
{

    ompi_free_list_t *flist;
    ompi_list_item_t *item;
    mca_ptl_elan_send_frag_t *desc;

    START_FUNC();

    /* For now, bind to queue DMA directly */
    if (MCA_PTL_ELAN_DESC_QDMA) {
        flist = &(((mca_ptl_elan_module_t *) ptl_ptr)->queue)->tx_desc_free;
    } else if (MCA_PTL_ELAN_DESC_PUT) {
        flist = &(((mca_ptl_elan_module_t *) ptl_ptr)->putget)->put_desc_free;
    } else if (MCA_PTL_ELAN_DESC_GET) {
	/*struct mca_ptl_elan_peer_t *peer;*/
        flist = &(((mca_ptl_elan_module_t *) ptl_ptr)->putget)->get_desc_free;
    } else {
        ompi_output (0,
                     "[%s:%d] Error: unknown to descriptor desc type\n",
                     __FILE__, __LINE__);
	return NULL;
    }

    if (ompi_using_threads ()) {

	ompi_mutex_lock(&flist->fl_lock);
	item = ompi_list_remove_first (&((flist)->super));

	/* Progress this PTL module to get back a descriptor,
	 * Is it OK to progress with ptl->ptl_send_progress? */
	while (NULL == item) {
	    mca_ptl_tstamp_t tstamp = 0;

	    ptl_ptr->ptl_component->ptlm_progress (tstamp);
	    item = ompi_list_remove_first (&((flist)->super));
	}
	ompi_mutex_unlock(&flist->fl_lock);
    } else {
	item = ompi_list_remove_first (&((flist)->super));

	/* Progress this PTL module to get back a descriptor,
	 * Is it OK to progress with ptl->ptl_send_progress()? */
	while (NULL == item) {
	    mca_ptl_tstamp_t tstamp = 0;

	    /* XXX: 
	     * Well, this still does not trigger the progress on 
	     * PTL's from other modules.  Wait for PML to change.
	     * Otherwise have to trigger PML progress from PTL.  Ouch..
	     */
	    ptl_ptr->ptl_component->ptlm_progress (tstamp);
	    item = ompi_list_remove_first (&((flist)->super));
	}
    }
    desc = (mca_ptl_elan_send_frag_t *) item; 
    desc->desc->req = (struct mca_ptl_elan_send_request_t *) sendreq;

    desc->desc->desc_type = desc_type;

    END_FUNC();
    return desc;
}

mca_ptl_elan_recv_frag_t *
mca_ptl_elan_alloc_recv_desc (struct mca_pml_base_recv_request_t * req)
{
    return NULL;
}


void 
mca_ptl_elan_send_desc_done (
       	mca_ptl_elan_send_frag_t *desc,
       	mca_ptl_elan_send_request_t *req) 
{ 
    mca_ptl_elan_module_t *ptl;
    ompi_ptl_elan_queue_ctrl_t *queue;
    mca_ptl_base_header_t *header;
 
    ptl = ((ompi_ptl_elan_qdma_desc_t *)desc->desc)->ptl;
    header = &desc->frag_base.frag_header;
    queue = ptl->queue;

    if(NULL == req) { /* An ack descriptor */
	OMPI_FREE_LIST_RETURN (&queue->tx_desc_free,
		(ompi_list_item_t *) desc);
    } 
#if 1   
    else if (0 == (header->hdr_common.hdr_flags 
		& MCA_PTL_FLAGS_ACK_MATCHED)
	    || mca_pml_base_send_request_matched(req)) {

	if(fetchNset (&desc->frag_progressed, 1) == 0) {
	    ptl->super.ptl_send_progress(ptl, req, 
		    header->hdr_frag.hdr_frag_length);
	}

	/* Return a frag or if not cached, or it is a follow up */ 
	if((header->hdr_frag.hdr_frag_offset != 0) || (desc->desc->desc_status 
		    != MCA_PTL_ELAN_DESC_CACHED)) 
	    OMPI_FREE_LIST_RETURN (&queue->tx_desc_free,
		    (ompi_list_item_t *) desc);
     }
#else
    else  {

    /* XXX: need to discuss this with TSW.
     * There is a little confusion here. 
     * Why the release of this send fragment is dependent 
     * on the receiving of an acknowledgement 
     * There are two drawbacks,
     * a) Send fragment is not immediately return the free pool
     * b) Some list is needed to hold on this fragment and
     *    later on find an time slot to process it.
     * c) If ever local completion happens later then the receive
     *    of the acknowledgement. The following will happen
     *    1) The receiving of an acknoledgement can not immediatly
     *    trigger the scheduling the followup fragment since it
     *    is dependent on the send fragment to complete.
     *    2) Later, the local send completeion cannot trigger 
     *       the start of following fragments. As the logic is not there.
     */
	if(fetchNset (&desc->frag_progressed, 1) == 0) {
	    ptl->super.ptl_send_progress(ptl, req, 
		    header->hdr_frag.hdr_frag_length);
	}

	/* Return a frag or if not cached, or it is a follow up */ 
	if((header->hdr_frag.hdr_frag_offset != 0) || (desc->desc->desc_status 
		    != MCA_PTL_ELAN_DESC_CACHED)) 
	    OMPI_FREE_LIST_RETURN (&queue->tx_desc_free,
		    (ompi_list_item_t *) desc);
    } 
#endif
}
 
void 
mca_ptl_elan_recv_frag_done (
       	mca_ptl_base_header_t *header,
       	mca_ptl_elan_recv_frag_t* frag,
       	mca_pml_base_recv_request_t *request) 
{ 
    frag->frag_recv.frag_base.frag_owner->ptl_recv_progress (
	    frag->frag_recv.frag_base.frag_owner, 
	    request, 
	    frag->frag_recv.frag_base.frag_size, 
	    frag->frag_recv.frag_base.frag_size);

    /* FIXME: 
     * To support the required ACK, do not return
     * until the ack is out */
    if (frag->frag_ack_pending == false) {
	mca_ptl_elan_recv_frag_return (
		frag->frag_recv.frag_base.frag_owner, frag);
    } else {
	/* Chaining it into the list of completion pending recv_frag,
	 * Until the ack frag is sent out, they will stay in the list */
    }
}


