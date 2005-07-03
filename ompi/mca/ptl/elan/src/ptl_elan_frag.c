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

#include "ompi_config.h"

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

opal_class_t mca_ptl_elan_send_frag_t_class = {
    "mca_ptl_elan_send_frag_t",
    OBJ_CLASS (mca_ptl_base_frag_t),
    (opal_construct_t) mca_ptl_elan_send_frag_construct,
    (opal_destruct_t) mca_ptl_elan_send_frag_destruct
};

static void
mca_ptl_elan_recv_frag_construct (mca_ptl_elan_recv_frag_t * frag)
{
    frag->frag_hdr_cnt = 0;
    frag->frag_msg_cnt = 0;
    frag->frag_progressed = 0;

    /*frag->frag.qdma = NULL;*/
    frag->alloc_buff = (char *) malloc (sizeof (char) * 2048 + 32);
    if (NULL == frag->alloc_buff) {
        opal_output (0,
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

    /*frag->frag.qdma = NULL;*/
    free (frag->alloc_buff);
    frag->alloc_buff = NULL;
    frag->unex_buff = NULL;
}

opal_class_t mca_ptl_elan_recv_frag_t_class = {
    "mca_ptl_elan_recv_frag_t",
    OBJ_CLASS (mca_ptl_base_recv_frag_t),
    (opal_construct_t) mca_ptl_elan_recv_frag_construct,
    (opal_destruct_t) mca_ptl_elan_recv_frag_destruct
};


extern mca_ptl_elan_state_t mca_ptl_elan_global_state;

mca_ptl_elan_send_frag_t *
mca_ptl_elan_alloc_desc (struct mca_ptl_base_module_t *ptl_ptr,
       	struct mca_pml_base_request_t *req, int desc_type)
{

    ompi_free_list_t *flist;
    opal_list_item_t *item = NULL;
    mca_ptl_elan_send_frag_t *desc;

    /* TODO: Dynamically bind a base request to PUT/GET/QDMA/STEN */
    if (MCA_PTL_ELAN_DESC_QDMA == desc_type) {
        flist = &(((mca_ptl_elan_module_t *) ptl_ptr)->queue)->tx_desc_free;
    } else if (MCA_PTL_ELAN_DESC_PUT == desc_type) {
        flist = &(((mca_ptl_elan_module_t *) ptl_ptr)->putget)->put_desc_free;
    } else if (MCA_PTL_ELAN_DESC_GET == desc_type) {
        flist = &(((mca_ptl_elan_module_t *) ptl_ptr)->putget)->get_desc_free;
    } else {
        opal_output (0,
                     "[%s:%d] Error: unknown to descriptor desc type\n",
                     __FILE__, __LINE__);
	return NULL;
    }

    LOG_PRINT(PTL_ELAN_DEBUG_SEND, "flist %p length %d type %d\n", 
	    flist, flist->super.opal_list_length, desc_type);
    if (opal_using_threads ()) {
	opal_mutex_lock(&flist->fl_lock);
	item = opal_list_remove_first (&((flist)->super));
	while (NULL == item) {
	    mca_ptl_tstamp_t tstamp = 0;
	    ptl_ptr->ptl_component->ptlm_progress (tstamp);
	    item = opal_list_remove_first (&((flist)->super));
	}
	opal_mutex_unlock(&flist->fl_lock);
    } else {
	item = opal_list_remove_first (&((flist)->super));
	/* XXX: 
	 * Ouch..., this still does not trigger the progress on 
	 * PTL's from other modules.  Wait for PML to change.
	 * Otherwise have to trigger PML progress from PTL.  */
	while (NULL == item) {
	    mca_ptl_tstamp_t tstamp = 0;
	    ptl_ptr->ptl_component->ptlm_progress (tstamp);
	    item = opal_list_remove_first (&((flist)->super));
	}
    }
    desc = (mca_ptl_elan_send_frag_t *) item; 
    desc->desc->req = req;
    desc->desc->desc_type = desc_type;
    LOG_PRINT(PTL_ELAN_DEBUG_SEND, "Got frag %p desc %d type %d\n", 
	    desc, desc->desc, desc_type);
    return desc;
}

mca_ptl_elan_recv_frag_t *
mca_ptl_elan_alloc_recv_desc (struct mca_pml_base_recv_request_t * req)
{
    return NULL;
}


void 
mca_ptl_elan_send_desc_done (
       	mca_ptl_elan_send_frag_t *frag,
       	mca_pml_base_send_request_t *req) 
{ 
    mca_ptl_elan_module_t *ptl;
    mca_ptl_base_header_t *header;
    int dtype;
 
    dtype = frag->desc->desc_type;
    ptl = ((ompi_ptl_elan_qdma_desc_t *)frag->desc)->ptl;
    header = &frag->frag_base.frag_header;

#if OMPI_PTL_ELAN_ENABLE_GET
    if (frag->desc->desc_type == MCA_PTL_ELAN_DESC_GET) {
	if(opal_atomic_fetch_and_set_int (&frag->frag_progressed, 1) == 0) {
	    ptl->super.ptl_recv_progress(ptl, 
		    (mca_pml_base_recv_request_t *) req, 
		    frag->frag_base.frag_size,
		    frag->frag_base.frag_size);
	}
	PTL_ELAN4_FREE_QBUFF (ptl->ptl_elan_ctx,
		((ompi_ptl_elan_putget_desc_t *) frag->desc)
		->chain_event->ev_Params[1], 8);
	OMPI_FREE_LIST_RETURN (&ptl->putget->get_desc_free,
	       	(opal_list_item_t *) frag);
	return;
    }
#endif

    LOG_PRINT(PTL_ELAN_DEBUG_SEND, 
	    "req %p done frag %p desc %p desc_type %d length %d\n", 
	    req, frag, frag->desc, 
	    frag->desc->desc_type,
	    header->hdr_frag.hdr_frag_length);

    if(NULL == req) { /* An ack descriptor */
	OMPI_FREE_LIST_RETURN (&ptl->queue->tx_desc_free,
		(opal_list_item_t *) frag);
    } else if (0 == (header->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK)
	    || mca_pml_base_send_request_matched(req)) {
	if(opal_atomic_fetch_and_set_int (&frag->frag_progressed, 1) == 0) 
	{
	    ptl->super.ptl_send_progress(
		    (struct mca_ptl_base_module_t*) ptl, 
		    req, header->hdr_frag.hdr_frag_length);
	}

	LOG_PRINT(PTL_ELAN_DEBUG_SEND, "return frag %p desc %p type %d\n", 
		frag, frag->desc, frag->desc->desc_type);

	/* Return a frag or if not cached, or it is a follow up */ 
	if ( (frag->desc->desc_status != MCA_PTL_ELAN_DESC_CACHED)){
	    ompi_free_list_t  *flist;
	    if (frag->desc->desc_type == MCA_PTL_ELAN_DESC_PUT) {
		flist = &ptl->putget->put_desc_free;
		PTL_ELAN4_FREE_QBUFF (ptl->ptl_elan_ctx,
		       	((ompi_ptl_elan_putget_desc_t *) frag->desc)
			->chain_event->ev_Params[1], 8);
	    } else {
		flist = &ptl->queue->tx_desc_free;
	    }
	    OMPI_FREE_LIST_RETURN (flist, (opal_list_item_t *) frag);
	} else {
	    LOG_PRINT(PTL_ELAN_DEBUG_ACK,
		    "PML will return frag to list %p, length %d\n", 
		    &ptl->queue->tx_desc_free,
		    ptl->queue->tx_desc_free.super.opal_list_length);
	}
    }
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
	/* XXX: Chaining it into the list of completion pending recv_frag,
	 * Until the ack frag is sent out, they will stay in the list */
    }
}


