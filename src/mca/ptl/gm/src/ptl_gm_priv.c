
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
            (gm_remote_ptr_t) target_buffer,bytes, GM_LOW_PRIORITY,
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

    size_in = *size;
  
    outvec.iov_base = (char*)fragment->send_buf;
   
     if( (size_in + header_length) <= GM_SEND_BUF_SIZE ) 
            outvec.iov_len = size_in;
    else
            outvec.iov_len = GM_SEND_BUF_SIZE - header_length;


    if(size_in > 0) {
        ompi_convertor_t *convertor;
        int rc;

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
                                          offset);
        }

        /* if data is contigous convertor will return an offset
         * into users buffer - otherwise will return an allocated buffer 
         * that holds the packed data
         */

        /*XXX: need to add the header */

        /*copy the data to the registered buffer*/
        outvec.iov_base = ((char*)fragment->send_buf) + header_length;

        if((rc = ompi_convertor_pack(convertor, &(outvec), 1)) < 0)
            return OMPI_ERROR;
    }
    /* update the fields */
    outvec.iov_len += header_length;
    outvec.iov_base = fragment->send_buf;

    /* adjust size and request offset to reflect actual number of bytes
     * packed by convertor */
    size_out = outvec.iov_len;
   
    /* initiate the gm send */
    gm_send_with_callback( ptl_peer->peer_ptl->my_port, fragment->send_buf, 
                           GM_SIZE, size_out, GM_LOW_PRIORITY, ptl_peer->local_id,
                           ptl_peer->port_number, send_callback, (void *)fragment );

    fragment->send_frag.frag_base.frag_owner = &ptl_peer->peer_ptl->super;
    fragment->send_frag.frag_base.frag_peer = (struct mca_ptl_base_peer_t*)ptl_peer;
    fragment->send_frag.frag_base.frag_addr = outvec.iov_base;
    fragment->send_frag.frag_base.frag_size = size_out; 

    *size = (size_out - header_length);
    return OMPI_SUCCESS;
}


void put_callback(struct gm_port *port,void * context, gm_status_t status)
{
    mca_ptl_gm_module_t *ptl;
    mca_ptl_gm_send_frag_t *putfrag;
    int bytes;
    mca_pml_base_send_request_t *send_req;
    size_t offset = 0;
    size_t size = 0;
    int flags = 0;
    int rc;

    putfrag = (mca_ptl_gm_send_frag_t *)context;
    ptl = (mca_ptl_gm_module_t *)putfrag->ptl;
    send_req = putfrag->req;

    bytes = putfrag->send_frag.frag_base.frag_size; 

    GM_DBG(PTL_GM_DBG_COMM,"ENTERING PUT CALLBACK\n");

    switch  (status) {
    case GM_SUCCESS:
        /* local put completed, mark put as complete */
   
        GM_DBG(PTL_GM_DBG_COMM,"PUTCALLBACK WITH CASE GM_SUCCESS\n");
        ptl->num_send_tokens++;
        putfrag->put_sent = 1;

        /* send the header information through send/receive channel */

        rc = mca_ptl_gm_peer_send (putfrag->peer,putfrag,send_req,
                                offset,&size,flags);
        assert(rc == 0);
        GM_DBG(PTL_GM_DBG_COMM,"FINISHED SENDING FIN\n");
        GM_DBG(PTL_GM_DBG_COMM,"after issuing the put completion the request offset = %d\n",send_req->req_offset);

        /* deregister the user memory */
       status = gm_deregister_memory(ptl->my_port, (char *)(putfrag->registered_buf), bytes);

       if(GM_SUCCESS != status) {
	   ompi_output(0," unpinning memory failed\n");
       } 
       else {
	   GM_DBG(PTL_GM_DBG_COMM, " unpinning %d bytes of memory success\n",bytes);
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
    ompi_list_t *list;
    int bytes, header_length;
    mca_pml_base_send_request_t *gm_send_req;
    mca_ptl_base_header_t* header;

    frag = (mca_ptl_gm_send_frag_t *)context;
    ptl = (mca_ptl_gm_module_t *)frag->send_frag.frag_base.frag_owner;
    gm_send_req = frag->req;

    header = (mca_ptl_base_header_t*)frag->send_buf;
    header_length = ((mca_ptl_base_header_t*)header)->hdr_common.hdr_size;
   
    if (frag->type == PUT)
    {
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

        if ((frag->wait_for_ack == 0) && (gm_send_req != NULL)) {
	    GM_DBG(PTL_GM_DBG_COMM,"inside send callback  : calling send progress bytes = %d\n",bytes);
	    ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl,
		    gm_send_req, bytes );
        }
        OMPI_FREE_LIST_RETURN(&(ptl->gm_send_frags), (ompi_list_item_t *)frag);
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
    frag = (mca_ptl_gm_send_frag_t *)header->hdr_ack.hdr_src_ptr.pval;
    req = (mca_pml_base_send_request_t *) frag->req;
    assert(req != NULL);
    req->req_peer_match.pval = header->hdr_ack.hdr_dst_match.pval;
    req->req_peer_addr.pval = header->hdr_ack.hdr_dst_addr.pval;
    req->req_peer_size = header->hdr_ack.hdr_dst_size;
   
    frag->wait_for_ack = 0;
  
     /* check if send has completed */   
    header_length = 
          frag->send_frag.frag_base.frag_header.hdr_frag.hdr_common.hdr_size;
    bytes = frag->send_frag.frag_base.frag_size - 64; /*header_length;*/
     
    if(frag->send_complete == 1)
    {

         ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl,
                                                 req, bytes );
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
       status = gm_deregister_memory(ptl->my_port, reg_buf, 
                                 bytes);

       if(GM_SUCCESS != status) {
           ompi_output(0," unpinning memory failed\n");
       } else {
	   GM_DBG(PTL_GM_DBG_COMM, 
		 "unpinning memory success,addr:%p,bytes:%d\n",reg_buf,bytes);
       }

#if 0
      /*return the recv fragment to the free list */
       OMPI_FREE_LIST_RETURN(
	       &(((mca_ptl_gm_module_t *)ptl)->gm_recv_frags_free), 
	       (ompi_list_item_t *)recv_frag);

     /* free the associated buffer */
       if(recv_frag->have_allocated == true)
          free(recv_frag->frag_recv.frag_base.frag_add * GM_SEND_BUF_SIZE);
#endif
   }

 /* XXX: will handle NACK later */
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
   
    recv_frag->frag_recv.frag_base.frag_owner = (struct mca_ptl_base_module_t*)ptl;
    recv_frag->frag_recv.frag_base.frag_peer = NULL;
    recv_frag->frag_recv.frag_request = NULL;
    recv_frag->frag_recv.frag_is_buffered = false;
    recv_frag->frag_hdr_cnt = 0;
    recv_frag->frag_msg_cnt = 0;
    recv_frag->frag_ack_pending = false;
    recv_frag->frag_progressed = 0;
   
    recv_frag->frag_recv.frag_base.frag_header = *header;
    recv_frag->frag_recv.frag_base.frag_addr = header;
    /* + sizeof(mca_ptl_base_header_t);*/ /* XXX: bug */
    recv_frag->frag_recv.frag_base.frag_size = gm_ntohl(event->recv.length);

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
    int i, size;
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

                #if 0
                /* append to the receive queue */
                ompi_list_append (&(ptl->gm_recv_outstanding_queue),
                      (ompi_list_item_t *) frag);

                printf ("frag appended to recv_oustanding queue \n");
                #endif

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


