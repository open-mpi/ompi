/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * $HEADER$
 */
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/fcntl.h>
/*#include <netinet/gm.h>*/
#include <netinet/in.h>
#include <arpa/inet.h>

#include "include/types.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ns/base/base.h"
#include "ptl_gm.h"
#include "ptl_gm_req.h"
#include "ptl_gm_addr.h"
#include "ptl_gm_peer.h"
#include "ptl_gm_proc.h"
#include "ptl_gm_sendfrag.h"
#include "ptl_gm_priv.h"

int mca_ptl_gm_peer_put(mca_ptl_gm_peer_t *ptl_peer,
                        mca_ptl_gm_send_frag_t *fragment, 
                        struct mca_pml_base_send_request_t *sendreq, 
                        size_t offset,
                        size_t *size, 
                        int flags,
                        void * target_buffer,
                        int bytes) 
{

   gm_put( ptl_peer->peer_ptl->my_port, fragment->registered_buf,
	   (gm_remote_ptr_t)target_buffer, bytes, GM_LOW_PRIORITY,
	   ptl_peer->local_id, ptl_peer->port_number, 
	   put_callback, (void *)fragment );


    fragment->send_frag.frag_base.frag_owner = &ptl_peer->peer_ptl->super;
    fragment->send_frag.frag_base.frag_peer =
                             (struct mca_ptl_base_peer_t*)ptl_peer;
    fragment->send_frag.frag_base.frag_addr =(void *)target_buffer;
    fragment->send_frag.frag_base.frag_size = bytes; 
    return OMPI_SUCCESS;
}



int mca_ptl_gm_peer_send(mca_ptl_gm_peer_t *ptl_peer,
                        mca_ptl_gm_send_frag_t *fragment, 
                        struct mca_pml_base_send_request_t *sendreq, 
                        size_t offset,
                        size_t *size, 
                        int flags) 
{
    struct iovec outvec;
    size_t size_in,size_out;
    int header_length;
    mca_ptl_base_frag_header_t* header;

    header = (mca_ptl_base_frag_header_t*)fragment->send_buf;
    header_length = ((mca_ptl_base_header_t*)header)->hdr_common.hdr_size;
    A_PRINT("peer send (could be ack) : headerlen is %d \n", header_length);
    size_in = *size;
  
    outvec.iov_base = (char*)fragment->send_buf;
   
    if( (size_in + header_length) <= GM_SEND_BUF_SIZE ) 
            outvec.iov_len = size_in;
    else
            outvec.iov_len = GM_SEND_BUF_SIZE - header_length;


    if(size_in > 0) {
        ompi_convertor_t *convertor;
        int rc, freeAfter;
	unsigned int in_size, max_data;

        /* first fragment (eager send) and first fragment of long protocol
         * can use the convertor initialized on the request, remaining
         * fragments
         * must copy/reinit the convertor as the transfer could be in
         * parallel.
         */
        if( offset <= mca_ptl_gm_module.super.ptl_first_frag_size ) {
            convertor = &sendreq->req_convertor;
        } else {
            convertor = &(fragment->send_frag.frag_base.frag_convertor);
            ompi_convertor_copy(&sendreq->req_convertor, convertor);
            ompi_convertor_init_for_send( convertor,
                                          0,
                                          sendreq->req_base.req_datatype,
                                          sendreq->req_base.req_count,
                                          sendreq->req_base.req_addr,
                                          offset, NULL );
        }

        /* if data is contigous convertor will return an offset
         * into users buffer - otherwise will return an allocated buffer 
         * that holds the packed data
         */

        /*copy the data to the registered buffer*/
        outvec.iov_base = ((char*)fragment->send_buf) + header_length;
	max_data = outvec.iov_len;
	in_size = 1;
        if((rc = ompi_convertor_pack(convertor, &(outvec), &in_size, &max_data, &freeAfter)) < 0)
            return OMPI_ERROR;
    }
    /* update the fields */
    outvec.iov_len += header_length;
    outvec.iov_base = fragment->send_buf;

    /* adjust size and request offset to reflect actual number of bytes
     * packed by convertor */
    size_out = outvec.iov_len;
  
    A_PRINT( "peer_send request is %p\t, frag->req = %p, fragment is %p,size is %d, send_frag is %p\n",
	     sendreq, fragment->req,fragment,size_out,
	     ((mca_ptl_base_header_t *)header)->hdr_ack.hdr_src_ptr);
 
    /* initiate the gm send */
    gm_send_with_callback( ptl_peer->peer_ptl->my_port, fragment->send_buf, 
                           GM_SIZE, size_out, GM_LOW_PRIORITY, ptl_peer->local_id,
                           ptl_peer->port_number, send_callback, (void *)fragment );

    fragment->send_frag.frag_base.frag_owner = &ptl_peer->peer_ptl->super;
    fragment->send_frag.frag_base.frag_peer = (struct mca_ptl_base_peer_t*)ptl_peer;
    fragment->send_frag.frag_base.frag_addr = ((char*)outvec.iov_base) + header_length; 
    fragment->send_frag.frag_base.frag_size = size_out - header_length;

  #if 1 
    fragment->send_frag.frag_request = sendreq;
    if ((header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_FRAG) ||
            (header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_MATCH))
    header->hdr_frag_length = size_out - header_length;
  #endif

    *size = (size_out - header_length);
    A_PRINT("inside peer send : bytes sent is %d\n",*size);
    return OMPI_SUCCESS;
}


void put_callback(struct gm_port *port,void * context, gm_status_t status)
{
    mca_ptl_gm_module_t *ptl;
    mca_ptl_gm_send_frag_t *putfrag;
    mca_pml_base_send_request_t *send_req;
    mca_ptl_base_header_t* header;
    int bytes2;

    putfrag = (mca_ptl_gm_send_frag_t *)context;
    header = (mca_ptl_base_header_t*)putfrag->send_buf;
    bytes2 = header->hdr_ack.hdr_dst_size;
    ptl = (mca_ptl_gm_module_t *)putfrag->ptl;
    send_req = putfrag->req;

    A_PRINT("ENTERING PUT CALLBACK\n");

    switch  (status) {
    case GM_SUCCESS:
        /* local put completed, mark put as complete */
   
        GM_DBG(PTL_GM_DBG_COMM,"PUTCALLBACK WITH CASE GM_SUCCESS\n");
        ptl->num_send_tokens++;
        putfrag->put_sent = 1;

        /* send the header information through send/receive channel */
#if 0
        rc = mca_ptl_gm_peer_send (putfrag->peer,putfrag,send_req,
                                offset,&size,flags);
        assert(rc == 0);
        GM_DBG(PTL_GM_DBG_COMM,"FINISHED SENDING FIN\n");
        GM_DBG(PTL_GM_DBG_COMM,"after issuing the put completion the request offset = %d\n",send_req->req_offset);
#endif

        /* deregister the user memory */
       status = gm_deregister_memory(ptl->my_port, (char *)(putfrag->registered_buf), bytes2);

       if(GM_SUCCESS != status) {
    	   ompi_output(0," unpinning memory failed\n");
       } 
       else {
    	   GM_DBG(PTL_GM_DBG_COMM, " unpinning %d bytes of memory  success\n",bytes2);
       }    
       break;

    case GM_SEND_TIMED_OUT:
        /* need to take care of retransmission */
        break;

    case GM_SEND_DROPPED:
        /* need to handle this case */
        break;

    default:
        ompi_output(0,
                    "[%s:%d] error in message completion\n",__FILE__,__LINE__);
        break;
    }

}


void send_callback(struct gm_port *port,void * context, gm_status_t status)
{
    mca_ptl_gm_module_t *ptl;
    mca_ptl_gm_send_frag_t *frag;
    int header_length;
    mca_pml_base_send_request_t *gm_send_req;
    mca_ptl_base_header_t* header;


    frag = (mca_ptl_gm_send_frag_t *)context;
    ptl = (mca_ptl_gm_module_t *)frag->send_frag.frag_base.frag_owner;
    gm_send_req = frag->send_frag.frag_request;   
 
    header = (mca_ptl_base_header_t*)frag->send_buf;
    header_length = ((mca_ptl_base_header_t*)header)->hdr_common.hdr_size;


    switch  (status) {
        case GM_SUCCESS:
              ptl->num_send_tokens++;
    
              if(header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_ACK) 
              {
                   A_PRINT("send callback: Completion of send_ack, sent frag is %p\n", frag);
                   OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), ((ompi_list_item_t *) frag));

              } 

              else if(header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_FIN) 
              {
                    A_PRINT("send callback : Completion of fin, bytes complete =\
                                %d\n",header->hdr_ack.hdr_dst_size);
                    ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl, frag->send_frag.frag_request, 
                                        header->hdr_ack.hdr_dst_size);

                    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), ((ompi_list_item_t *) frag));

              }
 
              /* else if (NULL == gm_send_req) {
               A_PRINT("send callback for ack : \n");
               OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), ((ompi_list_item_t *) frag));
               } */

              else if (0 == (header->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK_MATCHED)
                            || mca_pml_base_send_request_matched(gm_send_req)) 
              {
                    A_PRINT(" send callback : match not required\n");
                    ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl, frag->send_frag.frag_request,
                                                     header->hdr_frag.hdr_frag_length);

                    /* Return sendfrag to free list */
                    A_PRINT("Return frag : %p", frag);
                    OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), ((ompi_list_item_t *) frag));
             } 

            else 
            {
                   /* Not going to call progress on this send,
                    * and not free-ing descriptor */
                   frag->send_complete = 1;
                   A_PRINT("send callback : match required but not yet recv ack sendfrag is %p\n",frag);
            }
            break;


    case GM_SEND_TIMED_OUT:
        /* need to take care of retransmission */
        break;

    case GM_SEND_DROPPED:
        /* need to handle this case */
        break;

    default:
        ompi_output(0,
                    "[%s:%d] error in message completion\n",__FILE__,__LINE__);
        break;
    }

    A_PRINT("RETURNING FROM SEND_CALLBACK\n");
}



#if 0
void send_callback(struct gm_port *port,void * context, gm_status_t status)
{
    mca_ptl_gm_module_t *ptl;
    mca_ptl_gm_send_frag_t *frag; 
    ompi_list_t *list;
    int bytes, header_length;
    mca_pml_base_send_request_t *gm_send_req;
    mca_ptl_base_header_t* header;


    frag = (mca_ptl_gm_send_frag_t *)context;
    ptl = (mca_ptl_gm_module_t *)frag->send_frag.frag_base.frag_owner;
    gm_send_req = frag->req;
    GM_DBG(PTL_GM_DBG_COMM,"INSIDE SENDCALLBACK  request is %p\t, frag->req = %p, frag is %p\n",
	   gm_send_req, frag->req,frag);

    header = (mca_ptl_base_header_t*)frag->send_buf;
    header_length = ((mca_ptl_base_header_t*)header)->hdr_common.hdr_size;
   
    if (frag->type == PUT)
    {
       /*printf("fin completion: frag is %p\n",frag);
       fflush(stdout); */
 
       bytes =  header->hdr_ack.hdr_dst_size;
    }
    else
        bytes = frag->send_frag.frag_base.frag_size - header_length; 


    if (NULL != gm_send_req)
    {
        if(1 ==  (( mca_ptl_gm_send_request_t *)gm_send_req)->need_ack  )
              frag->wait_for_ack = 1;
    }

    switch  (status) {
    case GM_SUCCESS:

        /* send completed, can reuse the user buffer */
       	GM_DBG(PTL_GM_DBG_COMM,
	       	"SENDCALLBACK WITH CASE GM_SUCCESS: frag->wait_for_ack = %d\n",
	       	frag->wait_for_ack);

        ptl->num_send_tokens++;
        frag->send_complete = 1;
        /*OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), (ompi_list_item_t * *)frag);*/
        /*A_PRINT("returned frag pointer  %p, free_list_num = %d\n",*/
        /*frag,(&(ptl->gm_send_frags))->fl_num_allocated);*/

        if ((frag->wait_for_ack == 0) && (gm_send_req != NULL)) 
        {

          if (frag->type == PUT)
          {
            /*printf("returned frag pointer  %p, free_list_num = %d frag type =
            %d\n", frag,(&(ptl->gm_send_frags))->fl_num_allocated,frag->type);
            fflush(stdout);*/
          }
          
	      GM_DBG(PTL_GM_DBG_COMM,"inside send callback  : calling send progress bytes = %d\n",bytes);
	      A_PRINT("inside send callback  : calling send progress bytes = %d\n",bytes);
	  
          ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl, gm_send_req, bytes );
          OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), (ompi_list_item_t *)frag);
       }

       if (gm_send_req == NULL)
       { 
          OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), (ompi_list_item_t *)frag);
          A_PRINT("returned ack frag pointer  %p, free_list_num = %d\n", frag,(&(ptl->gm_send_frags))->fl_num_allocated);
       }
       break;

    case GM_SEND_TIMED_OUT:
        /* need to take care of retransmission */
        break;

    case GM_SEND_DROPPED:
        /* need to handle this case */
        break;

    default:
        ompi_output(0,
                    "[%s:%d] error in message completion\n",__FILE__,__LINE__);
        break;
    }

    GM_DBG(PTL_GM_DBG_COMM,"RETURNING FROM SEND_CALLBACK\n");
}
#endif




void ptl_gm_ctrl_frag(struct mca_ptl_gm_module_t *ptl,
                    mca_ptl_base_header_t * header)
{
  mca_ptl_gm_send_frag_t * frag;
  mca_pml_base_send_request_t *req;
  mca_pml_base_recv_request_t *request;
  int header_length, bytes;
  char * reg_buf; 
  int status;
  
  if(header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_ACK)
    {
   
      A_PRINT( "ack header is %d, frag_ptr is %p\n", header->hdr_common.hdr_size,
	       header->hdr_ack.hdr_src_ptr.pval);
      frag = (mca_ptl_gm_send_frag_t *)(header->hdr_ack.hdr_src_ptr.pval);
      A_PRINT("inside ACK, corresp frag pointer  %p\n",frag);
      req = (mca_pml_base_send_request_t *) frag->req;
      assert(req != NULL);
      req->req_peer_match.pval = header->hdr_ack.hdr_dst_match.pval;
      req->req_peer_addr.pval = header->hdr_ack.hdr_dst_addr.pval;
      req->req_peer_size = header->hdr_ack.hdr_dst_size;
      frag->wait_for_ack = 0;
  
      header_length = frag->send_frag.frag_base.frag_header.hdr_frag.hdr_common.hdr_size;
      bytes = frag->send_frag.frag_base.frag_size; 
     
      if(frag->send_complete == 1)
	{

	  ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl,
					frag->send_frag.frag_request,bytes); 
            
	  OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), (ompi_list_item_t *)frag);
	  A_PRINT( "inside ACK,returning frag pointer  %p, request is %p, bytes is %d\n",
		   frag, frag->send_frag.frag_request, header->hdr_frag.hdr_frag_length );
	}

    }

  if(header->hdr_common.hdr_type == MCA_PTL_HDR_TYPE_FIN)
    {

      GM_DBG(PTL_GM_DBG_COMM,"CASE: HDR_TYPE_FIN\n");
      request = (mca_pml_base_recv_request_t*)
	header->hdr_ack.hdr_dst_match.pval;
      /* call receive progress and indicate the recv has been completed  */
    
      GM_DBG(PTL_GM_DBG_COMM,"Calling recv_progress with bytes = %d\n",header->hdr_ack.hdr_dst_size);
      ptl->super.ptl_recv_progress (
				    (mca_ptl_base_module_t *) ptl,
				    request , 
				    header->hdr_ack.hdr_dst_size,
				    header->hdr_ack.hdr_dst_size);
      /* deregister the memory */
      bytes = header->hdr_ack.hdr_dst_size;
      reg_buf =(char *) header->hdr_ack.hdr_dst_addr.pval; 
      status = gm_deregister_memory(ptl->my_port, reg_buf, bytes);

      if(GM_SUCCESS != status) 
	{
	  ompi_output(0," unpinning memory failed\n");
	}
      else 
	{
	  GM_DBG(PTL_GM_DBG_COMM, "unpinning memory success,addr:%p,bytes:%d\n",reg_buf,bytes);
	}

    }

}



mca_ptl_gm_recv_frag_t* ptl_gm_data_frag( struct mca_ptl_gm_module_t *ptl,
                                          gm_recv_event_t* event )
{
    mca_ptl_gm_recv_frag_t * recv_frag;
    bool matched;
    mca_ptl_base_header_t *header;

    header = (mca_ptl_base_header_t *)gm_ntohp(event->recv.buffer);
  
    /* allocate a receive fragment */
    recv_frag = mca_ptl_gm_alloc_recv_frag( (struct mca_ptl_base_module_t*)ptl );
    A_PRINT("the allocate drecv fragment is %p\n", recv_frag);
 
    recv_frag->frag_recv.frag_base.frag_owner = (struct mca_ptl_base_module_t*)ptl;
    recv_frag->frag_recv.frag_base.frag_peer = NULL;
    recv_frag->frag_recv.frag_request = NULL;
    recv_frag->frag_recv.frag_is_buffered = false;
    recv_frag->frag_hdr_cnt = 0;
    recv_frag->frag_msg_cnt = 0;
    recv_frag->frag_ack_pending = false;
    recv_frag->frag_progressed = 0;
   
    recv_frag->frag_recv.frag_base.frag_header = *header;
    #if 0
    recv_frag->frag_recv.frag_base.frag_addr = header;
    /* + sizeof(mca_ptl_base_header_t);*/ /* XXX: bug */
    recv_frag->frag_recv.frag_base.frag_size = gm_ntohl(event->recv.length);
    #endif
   #if 1
    recv_frag->frag_recv.frag_base.frag_addr =
                (char *) header + sizeof (mca_ptl_base_header_t);
    recv_frag->frag_recv.frag_base.frag_size = header->hdr_frag.hdr_frag_length;
   #endif

    recv_frag->matched = false;
    recv_frag->have_allocated_buffer = false;
    recv_frag->ptl = ptl;

    matched = ptl->super.ptl_match( &(ptl->super),
                                    &(recv_frag->frag_recv),
                                    &(recv_frag->frag_recv.frag_base.frag_header.hdr_match) );
    if( matched ) {
        return NULL;
    }

    GM_DBG(PTL_GM_DBG_COMM,
	    "matching receive not yet posted get tag %d comm %d source %d\n",
	    header->hdr_match.hdr_tag, header->hdr_match.hdr_contextid, header->hdr_match.hdr_src );
    return recv_frag;
}




mca_ptl_gm_recv_frag_t* ptl_gm_handle_recv( mca_ptl_gm_module_t *ptl, gm_recv_event_t* event )
{
    mca_ptl_gm_recv_frag_t* frag = NULL;
    mca_ptl_base_header_t *header;

    header = (mca_ptl_base_header_t *)gm_ntohp(event->recv.buffer);

    switch(header->hdr_common.hdr_type) {
    case MCA_PTL_HDR_TYPE_MATCH:
    case MCA_PTL_HDR_TYPE_FRAG:
        frag = ptl_gm_data_frag( ptl, event );
        break;

    case MCA_PTL_HDR_TYPE_ACK:
    case MCA_PTL_HDR_TYPE_NACK:
    case MCA_PTL_HDR_TYPE_FIN:
        ptl_gm_ctrl_frag(ptl,header);
        break;
    default:
        ompi_output(0,"[%s:%d] unexpected frag type %d\n",
                    __FILE__,__LINE__,header->hdr_common.hdr_type);
        break;
    }
    return frag;
}




void mca_ptl_gm_outstanding_recv(mca_ptl_gm_module_t *ptl)
{
  
    mca_ptl_gm_recv_frag_t * frag = NULL;
    int  size;
    bool matched; 
 
    size = ompi_list_get_size (&ptl->gm_recv_outstanding_queue);


    if (size > 0)
    {  
        frag = (mca_ptl_gm_recv_frag_t *)
                ompi_list_remove_first( (ompi_list_t *)&(ptl->gm_recv_outstanding_queue) );


        GM_DBG(PTL_GM_DBG_COMM," the frag size to be matched is %d\n",frag->frag_recv.frag_base.frag_size);
        matched = ptl->super.ptl_match( &(ptl->super),
                                    &(frag->frag_recv),
                                   &(frag->frag_recv.frag_base.frag_header.hdr_match) );
          
        GM_DBG(PTL_GM_DBG_COMM,"the value of matched is %d\n", matched);
  
        if(!matched) {
           ompi_list_append((ompi_list_t *)&(ptl->gm_recv_outstanding_queue),
                      (ompi_list_item_t *) frag);
        } else {
           /* if allocated buffer, free the buffer */
           /* return the recv descriptor to the free list */
           OMPI_FREE_LIST_RETURN(&(ptl->gm_recv_frags_free), (ompi_list_item_t *)frag);   
        }  
    }

} 

int mca_ptl_gm_incoming_recv (mca_ptl_gm_component_t * gm_comp)
{
    int i;
    gm_recv_event_t *event;
    void * mesg;
    mca_ptl_gm_module_t *ptl;
    mca_ptl_gm_recv_frag_t * frag;

    for( i = 0; i< gm_comp->gm_num_ptl_modules; i++) {
        ptl = gm_comp->gm_ptl_modules[i];
        event = gm_receive(ptl->my_port);

        switch (gm_ntohc(event->recv.type)) {
        case GM_RECV_EVENT:
        case GM_HIGH_RECV_EVENT:
        case GM_PEER_RECV_EVENT:
        case GM_HIGH_PEER_RECV_EVENT:
            mesg = gm_ntohp(event->recv.buffer);
            frag = ptl_gm_handle_recv( ptl, event );
            GM_DBG(PTL_GM_DBG_COMM,"FINISHED HANDLING INCOMING EVENT\n");

            if( (frag != NULL) && !(frag->matched) ) {
                /* allocate temporary buffer: temporary until the fragment will be finally matched */
                char* buffer = malloc( GM_SEND_BUF_SIZE );
                if (NULL == buffer)
                {
                  ompi_output(0, "[%s:%d] error in allocating memory \n",
                                                        __FILE__, __LINE__);

                }
                /* copy the data from the registered buffer to the newly allocated one */
                memcpy( buffer, mesg, gm_ntohl(event->recv.length) );
                /* associate the buffer with the unexpected fragment */
                frag->frag_recv.frag_base.frag_addr = (void *)buffer;
                /* mark the fragment as having pending buffers */
                frag->have_allocated_buffer = true;

            }
            gm_provide_receive_buffer( ptl->my_port, gm_ntohp(event->recv.buffer),
                                       GM_SIZE, GM_LOW_PRIORITY );
            break;
        case GM_NO_RECV_EVENT:
            break;

        default:
            gm_unknown(ptl->my_port, event);

        }

    }  
    return 0;
}


