/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include "include/types.h"
#include "datatype/datatype.h"
#include "ptl_gm.h"
#include "ptl_gm_sendfrag.h"
#include "ptl_gm_priv.h"

static void mca_ptl_gm_send_frag_construct (mca_ptl_gm_send_frag_t * frag);
static void mca_ptl_gm_send_frag_destruct (mca_ptl_gm_send_frag_t * frag);

static void mca_ptl_gm_recv_frag_construct (mca_ptl_gm_recv_frag_t * frag);
static void mca_ptl_gm_recv_frag_destruct (mca_ptl_gm_recv_frag_t * frag);

ompi_class_t mca_ptl_gm_send_frag_t_class = {
    "mca_ptl_gm_send_frag_t",
    OBJ_CLASS (mca_ptl_base_send_frag_t),
    (ompi_construct_t) mca_ptl_gm_send_frag_construct,
    (ompi_destruct_t) mca_ptl_gm_send_frag_destruct
};

/*
 * send fragment constructor/destructors.
 */

static void
mca_ptl_gm_send_frag_construct (mca_ptl_gm_send_frag_t * frag)
{
}

static void
mca_ptl_gm_send_frag_destruct (mca_ptl_gm_send_frag_t * frag)
{
}

/* It's not yet clear for me what's the best solution here. Block until we
 * get a free request or allocate a new one. The fist case allow us to never
 * take care of the gm allocated DMA buffer as all send fragments already have
 * one attached, but it can stop the application progression. The second case
 * require special cases: we should set the data in the header inside the fragment
 * and later when we get some free fragments with DMA memory attached we should
 * put the header back there, and send it.
 *
 * I will implement the first case and add the second one in my TODO list.
 */
mca_ptl_gm_send_frag_t *
mca_ptl_gm_alloc_send_frag( struct mca_ptl_gm_module_t *ptl,
			    struct mca_pml_base_send_request_t * sendreq )
{

    ompi_free_list_t *flist;
    ompi_list_item_t *item;
    mca_ptl_gm_send_frag_t *sendfrag;
    int32_t rc;

    flist = &(ptl->gm_send_frags);

    /* first get a gm_send_frag */
    OMPI_FREE_LIST_WAIT( &(ptl->gm_send_frags), item, rc );
    sendfrag = (mca_ptl_gm_send_frag_t *)item;
    /* And then get some DMA memory to put the data */
    ompi_atomic_sub( &(ptl->num_send_tokens), 1 );
    assert( ptl->num_send_tokens >= 0 );
    OMPI_FREE_LIST_WAIT( &(ptl->gm_send_dma_frags), item, rc );
    sendfrag->send_buf = (void*)item;

    sendfrag->req = (struct mca_pml_base_send_request_t *)sendreq;
    sendfrag->status        = -1;
    sendfrag->type          = -1;
    sendfrag->wait_for_ack  =  0;
    sendfrag->put_sent      = -1;
    sendfrag->send_complete = -1;
    
    return sendfrag;
}


int mca_ptl_gm_send_frag_done( mca_ptl_gm_send_frag_t * frag,
			       mca_pml_base_send_request_t * req )
{
    return OMPI_SUCCESS;
}

int mca_ptl_gm_send_ack_init( struct mca_ptl_gm_send_frag_t* ack,
			      struct mca_ptl_gm_module_t *ptl,
			      struct mca_ptl_gm_peer_t* ptl_peer,
			      struct mca_ptl_gm_recv_frag_t* frag,
			      char * buffer,
			      int size )
{
    mca_ptl_base_ack_header_t * hdr;
    mca_pml_base_recv_request_t *request;

    hdr = (mca_ptl_base_ack_header_t*)ack->send_buf;

    ack->status = -1;
    ack->type = -1;
    ack->wait_for_ack = 0;
    ack->put_sent = -1;
    ack->send_complete = -1;

    request = frag->frag_recv.frag_request;

    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_ACK;
    hdr->hdr_common.hdr_flags = 0;

    hdr->hdr_src_ptr.pval =
	frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_src_ptr.pval;

    hdr->hdr_dst_match.lval = 0;
    hdr->hdr_dst_match.pval = request; /*should this be dst_match */
    hdr->hdr_dst_addr.lval = 0; /*we are filling both p and val of dest address */
    hdr->hdr_dst_addr.pval = (void *)buffer;
    hdr->hdr_dst_size = size;

    ack->send_frag.frag_request = 0;
    ack->send_frag.frag_base.frag_peer = (struct mca_ptl_base_peer_t *)ptl_peer;
    ack->send_frag.frag_base.frag_owner = (mca_ptl_base_module_t *)ptl;
    ack->send_frag.frag_base.frag_addr = NULL;
    ack->send_frag.frag_base.frag_size = 0;
    ack->status = 1; /* was able to register memory */
    ack->ptl = ptl;
    ack->send_frag.frag_base.frag_header.hdr_ack = *hdr;
    ack->wait_for_ack = 0;
    ack->type = ACK;

    return OMPI_SUCCESS;

}


int mca_ptl_gm_put_frag_init( struct mca_ptl_gm_send_frag_t* putfrag,
			      struct mca_ptl_gm_peer_t * ptl_peer,
			      struct mca_ptl_gm_module_t * gm_ptl,
			      struct mca_pml_base_send_request_t * request,
			      size_t offset,
			      size_t* size,
			      int flags )
{
    mca_ptl_base_header_t *hdr;
    hdr = (mca_ptl_base_header_t *)putfrag->send_buf;

    putfrag->status = -1;
    putfrag->type = -1;
    putfrag->wait_for_ack = 0;
    putfrag->put_sent = -1;
    putfrag->send_complete = -1;

    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FIN;
    hdr->hdr_common.hdr_flags = 0;
  
    hdr->hdr_ack.hdr_dst_match.lval = 0;
    hdr->hdr_ack.hdr_dst_match.pval = request->req_peer_match.pval;
    hdr->hdr_ack.hdr_dst_addr.lval  = 0;
    hdr->hdr_ack.hdr_dst_addr.pval  = (void *)(request->req_peer_addr.pval);
    hdr->hdr_ack.hdr_dst_size       = *size; 
    hdr->hdr_ack.hdr_src_ptr.lval   = 0;
    hdr->hdr_ack.hdr_src_ptr.pval   = (void*)putfrag;

    putfrag->send_frag.frag_request         = request; 
    putfrag->send_frag.frag_base.frag_peer  = (struct mca_ptl_base_peer_t *)ptl_peer;
    putfrag->send_frag.frag_base.frag_owner = (mca_ptl_base_module_t *)gm_ptl;
    putfrag->send_frag.frag_base.frag_addr  = NULL;
    putfrag->send_frag.frag_base.frag_size  = 0;
    putfrag->ptl = gm_ptl;

    putfrag->wait_for_ack = 0;
    putfrag->put_sent     = 0;
    putfrag->type         = PUT;
    putfrag->req          = request; 
    assert(putfrag->req != NULL);  
    return OMPI_SUCCESS; 
}

ompi_class_t mca_ptl_gm_recv_frag_t_class = {
    "mca_ptl_gm_recv_frag_t",
    OBJ_CLASS (mca_ptl_base_recv_frag_t),
    (ompi_construct_t) mca_ptl_gm_recv_frag_construct,
    (ompi_construct_t) mca_ptl_gm_recv_frag_destruct
};

/*
 * recv fragment constructor/destructors.
 */

static void
mca_ptl_gm_recv_frag_construct (mca_ptl_gm_recv_frag_t * frag)
{
    frag->frag_hdr_cnt = 0;
    frag->frag_msg_cnt = 0;
}

static void
mca_ptl_gm_recv_frag_destruct (mca_ptl_gm_recv_frag_t *frag)
{
}

mca_ptl_gm_recv_frag_t *
mca_ptl_gm_alloc_recv_frag( struct mca_ptl_base_module_t *ptl )
{
    int rc;
    ompi_list_item_t* item;

    OMPI_FREE_LIST_GET( &(((mca_ptl_gm_module_t *)ptl)->gm_recv_frags_free), item, rc );

    return (mca_ptl_gm_recv_frag_t *)item;

}

