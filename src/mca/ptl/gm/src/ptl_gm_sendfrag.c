/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *HEADER$
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

/*XXX : take care of multi threading*/

mca_ptl_gm_send_frag_t *
mca_ptl_gm_alloc_send_frag( struct mca_ptl_gm_module_t *ptl,
			    struct mca_pml_base_send_request_t * sendreq )
{

    ompi_free_list_t *flist;
    ompi_list_item_t *item;
    mca_ptl_gm_send_frag_t *frag;
    mca_ptl_tstamp_t tstamp = 0;

    flist = &(ptl->gm_send_frags);

    item = ompi_list_remove_first( &(flist->super) );
 
    while(NULL == item) {
	A_PRINT("888888888888888888888888 calling progress to allocate send frag\n");
	((mca_ptl_base_module_t*)ptl)->ptl_component->ptlm_progress(tstamp);
	item = ompi_list_remove_first( &(flist->super) );
    }

    frag = (mca_ptl_gm_send_frag_t *)item;
    frag->req = (struct mca_pml_base_send_request_t *)sendreq;
    GM_DBG( PTL_GM_DBG_COMM, "request is %p\t, frag->req = %p\n", (void*)sendreq, (void*)frag->req );
    frag->type =  0;
    
    return frag;
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

    A_PRINT( "inside ack init: the src frag ptr is %p,hdr_len is %u\n",
             hdr->hdr_src_ptr, hdr->hdr_common.hdr_size );

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
    ompi_free_list_t *flist;
    ompi_list_item_t *item;
    mca_ptl_gm_recv_frag_t *frag;
    mca_ptl_tstamp_t tstamp = 0;

    GM_DBG(PTL_GM_DBG_COMM,"INSIDE ALLOC RECV FRAG\n");
    flist =&( ((mca_ptl_gm_module_t *)ptl)->gm_recv_frags_free);
    item = ompi_list_remove_first(&((flist)->super));

    while(NULL == item) {
        ptl->ptl_component->ptlm_progress(tstamp);
        item = ompi_list_remove_first (&((flist)->super));
    }

    frag = (mca_ptl_gm_recv_frag_t *)item;
    return frag;

}

