
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
#include "ptl_gm_addr.h"
#include "ptl_gm_peer.h"
#include "ptl_gm_proc.h"
#include "ptl_gm_sendfrag.h"
#include "ptl_gm_priv.h"

int mca_ptl_gm_peer_send(mca_ptl_gm_peer_t *ptl_peer,
                        mca_ptl_gm_send_frag_t *fragment, 
                        struct mca_pml_base_send_request_t *sendreq, 
                        size_t offset,
                        size_t *size, 
                        int flags) 
{
    struct iovec outvec[1];
    size_t size_in,size_out;
    int header_length;
    mca_ptl_base_frag_header_t* header;

    header = (mca_ptl_base_frag_header_t*)fragment->send_buf;
    header_length = ((mca_ptl_base_header_t*)header)->hdr_common.hdr_size;

    size_in = *size;
  
    outvec[0].iov_base = (char*)fragment->send_buf;
    if( (size_in + header_length) < GM_SEND_BUF_SIZE ) outvec[0].iov_len = size_in;
    else outvec[0].iov_len = GM_SEND_BUF_SIZE;

    /*header_length = sizeof(mca_ptl_base_frag_header_t);*/

    /* copy the header in the buffer */
    /**header = fragment->send_frag.frag_base.frag_header;*/

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
        outvec[0].iov_base = ((char*)fragment->send_buf) + header_length;

        if((rc = ompi_convertor_pack(convertor, &(outvec[0]), 1)) < 0)
            return OMPI_ERROR;
    }
    /* update the fields */
    outvec[0].iov_len += header_length;
    outvec[0].iov_base = fragment->send_buf;
    /* adjust size and request offset to reflect actual number of bytes
     * packed by convertor */
    size_out = outvec[0].iov_len;
   
    /* initiate the gm send */
    gm_send_with_callback( ptl_peer->peer_ptl->my_port, fragment->send_buf, 
                           GM_SIZE, size_out, GM_LOW_PRIORITY, ptl_peer->local_id,
                           ptl_peer->port_number, send_callback, (void *)fragment );

    fragment->send_frag.frag_base.frag_owner = &ptl_peer->peer_ptl->super;
    fragment->send_frag.frag_base.frag_peer = (struct mca_ptl_base_peer_t*)ptl_peer;
    fragment->send_frag.frag_base.frag_addr = outvec[0].iov_base;
    fragment->send_frag.frag_base.frag_size = size_out; /*XXX: should this be size_out */

    return OMPI_SUCCESS;
}


void send_callback(struct gm_port *port,void * context, gm_status_t status)
{
    mca_ptl_gm_module_t *ptl;
    mca_ptl_gm_send_frag_t *frag; 
    ompi_list_t *list;
    int bytes, header_length;
    mca_pml_base_send_request_t *gm_send_req;
    mca_ptl_base_frag_header_t* header;

    frag = (mca_ptl_gm_send_frag_t *)context;
    ptl = (mca_ptl_gm_module_t *)frag->ptl;
    gm_send_req = frag->req;

    header = (mca_ptl_base_frag_header_t*)frag->send_buf;
    header_length = ((mca_ptl_base_header_t*)header)->hdr_common.hdr_size;
    bytes = frag->send_frag.frag_base.frag_size - header_length;

    switch  (status) {
    case GM_SUCCESS:
        /* send completed, can reuse the user buffer */
        ptl->num_send_tokens++;
        ptl->super.ptl_send_progress( (mca_ptl_base_module_t*)ptl, gm_send_req, bytes );
        list = (ompi_list_t *)(&(ptl->gm_send_frags_queue));
        ompi_list_remove_first(list);
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

void ptl_gm_ctrl_frag(struct mca_ptl_gm_module_t *ptl,
                    mca_ptl_base_header_t * header)
{
    mca_ptl_gm_send_frag_t * frag;
    mca_pml_base_send_request_t *req;

    frag = (mca_ptl_gm_send_frag_t *)header->hdr_ack.hdr_src_ptr.pval;
    req = (mca_pml_base_send_request_t *) frag->req;
    req->req_peer_match = header->hdr_ack.hdr_dst_match;

    /* return the send fragment to the free list */
}

mca_ptl_gm_recv_frag_t* ptl_gm_data_frag( struct mca_ptl_gm_module_t *ptl,
                                          gm_recv_event_t* event )
{
    mca_ptl_gm_recv_frag_t * recv_frag;
    bool matched;
    mca_ptl_base_header_t *header;

    header = (mca_ptl_base_header_t *)gm_ntohp(event->recv.buffer);

    recv_frag = mca_ptl_gm_alloc_recv_frag( (struct mca_ptl_base_module_t*)ptl );
    /* allocate a receive fragment */
   
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
    recv_frag->frag_recv.frag_base.frag_size = gm_ntohl(event->recv.length);

    recv_frag->matched = false;
    recv_frag->have_allocated_buffer = false;

    matched = ptl->super.ptl_match( &(ptl->super),
                                    &(recv_frag->frag_recv),
                                    &(recv_frag->frag_recv.frag_base.frag_header.hdr_match) );
    if( matched ) {
        return NULL;
    }
    ompi_output(0,"matching receive not yet posted\n");
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
        ptl_gm_ctrl_frag(ptl,header);
        break;
    default:
        ompi_output(0,"[%s:%d] unexpected frag type %d\n",
                    __FILE__,__LINE__,header->hdr_common.hdr_type);
        break;
    }
    return frag;
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
            if( (frag != NULL) && !(frag->matched) ) {
                /* allocate temporary buffer: temporary until the fragment will be finally matched */
                char* buffer = malloc( GM_SEND_BUF_SIZE );
                /* copy the data from the registered buffer to the newly allocated one */
                memcpy( buffer, mesg, gm_ntohl(event->recv.length) );
                /* associate the buffer with the unexpected fragment */
                frag->frag_recv.frag_base.frag_addr = buffer;
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


