
/*
 * $HEADER$
 */

#if 0
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
#include "ptl_gm_addr.h"
#include "ptl_gm_peer.h"
#include "ptl_gm_proc.h"
#include "ptl_gm_sendfrag.h"

int mca_ptl_gm_peer_send(mca_ptl_gm_peer_t *ptl_peer,
                        mca_ptl_gm_send_frag_t *fragment, 
                        struct mca_pml_base_send_request_t *sendreq, 
                        size_t offset,
                        size_t *size, 
                        int flags) 
{
    struct iovec outvec[1];
    size_t size_in,size_out;
   
    size_in = *size;
  
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

            convertor = &(fragment->frag_base.frag_base.frag_convertor);
            ompi_convertor_copy(&sendreq->req_convertor, convertor);
            ompi_convertor_init_for_send(
                convertor,
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

       /*copy the data to the registered buffer*/
        outvec[0].iov_base = fragment->send_buf;
        if( size_in < GM_SEND_BUF_SIZE ) outvec[0].iov_len = size_in;
        else outvec[0].iov_len = GM_SEND_BUF_SIZE;

        if((rc = ompi_convertor_pack(convertor, &(outvec[0]), 1)) < 0)
            return OMPI_ERROR;

        /* adjust size and request offset to reflect actual number of bytes
         * packed by convertor */
        size_out = outvec[0].iov_len;
    }
   
   /* initiate the gm send */
   gm_send_with_callback(ptl_peer->peer_ptl->my_port, fragment->send_buf, 
                        GM_SIZE,size_out,
                         GM_LOW_PRIORITY, ptl_peer->local_id,
                         ptl_peer->port_number,send_callback, (void *)sendreq );
   

    fragment->frag_base.frag_base.frag_owner = &ptl_peer->peer_ptl->super;
    fragment->frag_base.frag_base.frag_peer = ptl_peer;
    fragment->frag_base.frag_base.frag_addr = outvec[0].iov_base;
    fragment->frag_base.frag_base.frag_size = *size; /*XXX: should this be size_out */

   return OMPI_SUCCESS;
}


void send_callback(struct gm_port *port,void * context, gm_status_t status)
{
#if 0 
   mca_ptl_gm_send_request_t *gm_send_req; /*should be a fragment*/
   mca_ptl_gm_t *ptl;
   mca_ptl_gm_frag_t *frag; 
   int bytes;
   gm_send_req = (mca_ptl_gm_send_reguest_t *)context;
   ptl = gm_send_req->;

   switch (status)
    {
    case GM_SUCCESS:
         /* send completed, can reuse the user buffer */

         if(!fragment->done)
         {
              /*reuse the same buffer for the next segment*/
              ptl->num_send_tokens++;
         }
         else
         {
            ptl->num_send_tokens++;
            /* return to free list */
             ptl->gm_send_progress(ptl,gm_send_req,bytes);
             ompi_list_append(ptl->send_frag_queue,(ompi_list_item_t *)frag); 
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
#endif
}

#endif
