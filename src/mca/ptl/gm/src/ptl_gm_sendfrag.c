/*
 *HEADER$
 */
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "include/types.h"
#include "datatype/datatype.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "ptl_gm.h"
#include "ptl_gm_peer.h"
#include "ptl_gm_proc.h"
#include "ptl_gm_sendfrag.h"
#include "ptl_gm_priv.h"


/*#define frag_header     super.super.frag_header
#define frag_owner      super.super.frag_owner
#define frag_peer       super.super.frag_peer
#define frag_convertor  super.super.frag_convertor */


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
mca_ptl_gm_alloc_send_frag(struct mca_ptl_base_module_t *ptl,
                           struct mca_pml_base_send_request_t * sendreq)
{

    ompi_free_list_t *flist;
    ompi_list_item_t *item;
    mca_ptl_gm_send_frag_t *frag;
    mca_ptl_tstamp_t tstamp = 0;

    flist =&( ((mca_ptl_gm_module_t *)ptl)->gm_send_frags );
    item = ompi_list_remove_first(&((flist)->super));

    while(NULL == item)
    {
         ptl->ptl_component->ptlm_progress(tstamp);
         item = ompi_list_remove_first (&((flist)->super));
    }

    frag = (mca_ptl_gm_send_frag_t *)item;
    frag->req = (struct mca_pml_base_send_request_t *)sendreq;
    frag->type =  0 ;/* XXX: should be EAGER_SEND; */
    return frag;
  
}


int mca_ptl_gm_send_frag_done(
        mca_ptl_gm_send_frag_t * frag,
        mca_pml_base_send_request_t * req)
{

   return OMPI_SUCCESS;

}



int mca_ptl_gm_send_ack_init(
    struct mca_ptl_gm_send_frag_t* ack,
    mca_ptl_gm_module_t *ptl,
    mca_ptl_gm_peer_t* ptl_peer,
    struct mca_ptl_gm_recv_frag_t* frag,
    char * buffer,
    int size)
{
   int header_length;
   mca_ptl_base_header_t * hdr;
   mca_pml_base_recv_request_t *request;
   hdr = (mca_ptl_base_header_t *)ack->send_buf;
   request = frag->frag_recv.frag_request;
   hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_ACK;
   hdr->hdr_common.hdr_flags = 0;
   hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_ack_header_t);

   hdr->hdr_ack.hdr_src_ptr =  frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_src_ptr;
   hdr->hdr_ack.hdr_dst_match.lval = 0;
   hdr->hdr_ack.hdr_dst_match.pval = request;
   hdr->hdr_ack.hdr_dst_addr.lval = 0;
   hdr->hdr_ack.hdr_dst_addr.pval = (void *)buffer;/*request->req_base.req_addr;*/
                                   /*posted registered buffer */ 
   hdr->hdr_ack.hdr_dst_size = size;
                                   /*size of registered buffer */

  ack->send_frag.frag_request = 0;

  ack->send_frag.frag_base.frag_peer = (struct mca_ptl_base_peer_t *)ptl_peer;
  ack->send_frag.frag_base.frag_owner = (mca_ptl_base_module_t *)ptl;
  ack->send_frag.frag_base.frag_addr = NULL;
  ack->send_frag.frag_base.frag_size = 0;
  ack->status = 1; /* was able to register memory */
  ack->ptl = ptl;
  ack->send_frag.frag_base.frag_header = *hdr;
  ack->wait_for_ack = 0;
  header_length = sizeof(mca_ptl_base_ack_header_t);

   /* need to add registered buffer information */

 return OMPI_SUCCESS;

}


/*
int mca_ptl_gm_send_fini_init(
    mca_ptl_gm_send_frag_t* fini,
    mca_ptl_gm_module_t *ptl,
    mca_ptl_gm_peer_t* ptl_peer,
    mca_pml_base_send_request_t* request)
{

#if 1
   int header_length;
   mca_ptl_base_header_t * hdr;
   hdr = (mca_ptl_base_header_t *)fini->send_buf;
   hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FIN;
   hdr->hdr_common.hdr_flags = 0;
   hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_ack_header_t);
   hdr->hdr_ack.hdr_dst_match.lval = 0;
   hdr->hdr_ack.hdr_dst_addr.lval = 0;

  fini->send_frag.frag_request = 0;
  fini->send_frag.frag_base.frag_peer = ptl_peer;
  fini->send_frag.frag_base.frag_owner = ptl;
  fini->send_frag.frag_base.frag_addr = NULL;
  fini->send_frag.frag_base.frag_size = 0;
  fini->ptl = ptl;

  fini->wait_for_ack = 0;
  header_length = sizeof(mca_ptl_base_ack_header_t);
#endif

 return OMPI_SUCCESS;

}
*/

int mca_ptl_gm_put_frag_init(
    mca_ptl_gm_send_frag_t* putfrag,
    mca_ptl_gm_peer_t * ptl_peer,
    mca_ptl_gm_module_t * gm_ptl,
    mca_pml_base_send_request_t * request,
    size_t offset,
    size_t* size,
    int flags)
{
  mca_ptl_base_header_t *hdr;
  void * buffer;
  int header_length;

  #if 1
   hdr = (mca_ptl_base_header_t *)putfrag->send_buf;
   hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FIN;
   hdr->hdr_common.hdr_flags = 0;
   hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_ack_header_t);
   hdr->hdr_ack.hdr_dst_match.lval = 0;
   /*hdr->hdr_ack.hdr_dst_match.pval = request->req_peer_match;*/
   hdr->hdr_ack.hdr_dst_addr.lval = 0;
   hdr->hdr_ack.hdr_dst_addr.pval = (void *)(request->req_base.req_addr);
   hdr->hdr_ack.hdr_dst_size = request->req_bytes_packed;


   putfrag->send_frag.frag_request = request; /* XXX: check this */
   putfrag->send_frag.frag_base.frag_peer = ptl_peer;
   putfrag->send_frag.frag_base.frag_owner = (mca_ptl_base_module_t *)gm_ptl;
   putfrag->send_frag.frag_base.frag_addr = NULL;
   putfrag->send_frag.frag_base.frag_size = 0;
   putfrag->ptl = gm_ptl;

  #endif

  hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_ack_header_t);
  putfrag->send_frag.frag_base.frag_size = *size;
  putfrag->ptl = gm_ptl;
  putfrag->wait_for_ack = 0;
  putfrag->put_sent = 0;
  return OMPI_SUCCESS; 
}




int mca_ptl_gm_send_frag_init(
    mca_ptl_gm_send_frag_t* sendfrag,
    mca_ptl_gm_peer_t * ptl_peer,
    mca_pml_base_send_request_t * sendreq,
    size_t offset,
    size_t* size,
    int flags)

{
   int header_length; 
   mca_ptl_base_header_t *hdr;
   void *buffer;
   buffer = sendfrag->send_buf; 

    hdr = (mca_ptl_base_header_t *)sendfrag->send_buf; 
   if (offset == 0) {
     hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
     hdr->hdr_common.hdr_flags = flags;
     hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_match_header_t);
     hdr->hdr_frag.hdr_frag_offset = offset;
     hdr->hdr_frag.hdr_frag_seq = 0;
     hdr->hdr_frag.hdr_dst_ptr.lval = 0;
     hdr->hdr_frag.hdr_src_ptr.pval = sendfrag; /* pointer to the frag */
     hdr->hdr_frag.hdr_dst_ptr.lval = 0;

    hdr->hdr_match.hdr_contextid = sendreq->req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_dst = sendreq->req_base.req_peer;
    hdr->hdr_match.hdr_tag = sendreq->req_base.req_tag;
    hdr->hdr_match.hdr_msg_length= sendreq->req_bytes_packed;
    hdr->hdr_match.hdr_msg_seq = sendreq->req_base.req_sequence;
    header_length = sizeof (mca_ptl_base_match_header_t);
  } else {
    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FRAG;
    hdr->hdr_common.hdr_flags = flags;
    hdr->hdr_common.hdr_size = sizeof (mca_ptl_base_frag_header_t);
    hdr->hdr_frag.hdr_frag_offset = offset;
    hdr->hdr_frag.hdr_frag_seq = 0;
    hdr->hdr_frag.hdr_src_ptr.lval = 0;
    hdr->hdr_frag.hdr_src_ptr.pval = sendfrag;
    hdr->hdr_frag.hdr_dst_ptr = sendreq->req_peer_match;
    header_length = sizeof (mca_ptl_base_frag_header_t);
  }

   /*initialize convertor */

#if 0
   /*fragment state*/
   sendfrag->frag_base.frag_owner = &ptl_peer->peer_ptl->super;
   sendfrag->frag_base.frag_peer = ptl_peer;
   sendfrag->frag_base.frag_addr = NULL;
   sendfrag->frag_base.frag_size = *size;
#endif
 
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
;

}

mca_ptl_gm_recv_frag_t *
mca_ptl_gm_alloc_recv_frag( struct mca_ptl_base_module_t *ptl )
{

    ompi_free_list_t *flist;
    ompi_list_item_t *item;
    mca_ptl_gm_recv_frag_t *frag;
    mca_ptl_tstamp_t tstamp = 0;

    flist =&( ((mca_ptl_gm_module_t *)ptl)->gm_recv_frags_free);
    item = ompi_list_remove_first(&((flist)->super));

    while(NULL == item) {
        ptl->ptl_component->ptlm_progress(tstamp);
        item = ompi_list_remove_first (&((flist)->super));
    }

    frag = (mca_ptl_gm_recv_frag_t *)item;
    return frag;

}

