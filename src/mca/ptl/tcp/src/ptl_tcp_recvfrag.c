/*
 * $HEADER$
 */
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_tcp.h"
#include "ptl_tcp_peer.h"
#include "ptl_tcp_recvfrag.h"
#include "ptl_tcp_sendfrag.h"



static void mca_ptl_tcp_recv_frag_construct(mca_ptl_tcp_recv_frag_t* frag);
static void mca_ptl_tcp_recv_frag_destruct(mca_ptl_tcp_recv_frag_t* frag);
static bool mca_ptl_tcp_recv_frag_header(mca_ptl_tcp_recv_frag_t* frag, int sd, size_t);
static bool mca_ptl_tcp_recv_frag_ack(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_frag(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_match(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_data(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_discard(mca_ptl_tcp_recv_frag_t* frag, int sd);


ompi_class_t  mca_ptl_tcp_recv_frag_t_class = {
    "mca_ptl_tcp_recv_frag_t",
    OBJ_CLASS(mca_ptl_base_recv_frag_t),
    (ompi_construct_t)mca_ptl_tcp_recv_frag_construct,
    (ompi_destruct_t)mca_ptl_tcp_recv_frag_destruct
};
                                                                                                           

/*
 * TCP fragment constructor
 */

static void mca_ptl_tcp_recv_frag_construct(mca_ptl_tcp_recv_frag_t* frag)
{
}


/*
 * TCP fragment destructor
 */

static void mca_ptl_tcp_recv_frag_destruct(mca_ptl_tcp_recv_frag_t* frag)
{
}


/*
 * Initialize a TCP receive fragment for a specific peer.
 */

void mca_ptl_tcp_recv_frag_init(mca_ptl_tcp_recv_frag_t* frag, mca_ptl_base_peer_t* peer)
{
    frag->frag_recv.frag_base.frag_owner = &peer->peer_ptl->super;
    frag->frag_recv.frag_base.frag_addr = NULL;
    frag->frag_recv.frag_base.frag_size = 0;
    frag->frag_recv.frag_base.frag_peer = peer;
    frag->frag_recv.frag_request = 0;
    frag->frag_recv.frag_is_buffered = false;
    frag->frag_hdr_cnt = 0;
    frag->frag_msg_cnt = 0;
    frag->frag_ack_pending = false;
    frag->frag_progressed = 0;
}
                                                                                                                
/*
 * Callback from event library when socket has data available
 * for receive.
 */

bool mca_ptl_tcp_recv_frag_handler(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    /* read common header */
    if(frag->frag_hdr_cnt < sizeof(mca_ptl_base_header_t))
        if(mca_ptl_tcp_recv_frag_header(frag, sd, sizeof(mca_ptl_base_header_t)) == false)
            return false;

    switch(frag->frag_recv.frag_base.frag_header.hdr_common.hdr_type) {
    case MCA_PTL_HDR_TYPE_MATCH:
         return mca_ptl_tcp_recv_frag_match(frag, sd);
    case MCA_PTL_HDR_TYPE_FRAG:
        return mca_ptl_tcp_recv_frag_frag(frag, sd);
    case MCA_PTL_HDR_TYPE_ACK: 
    case MCA_PTL_HDR_TYPE_NACK:
        return mca_ptl_tcp_recv_frag_ack(frag, sd);
    default:
        ompi_output(0, "mca_ptl_tcp_recv_frag_handler: invalid message type: %08X", 
            *(unsigned long*)&frag->frag_recv.frag_base.frag_header);
         return false;
    }
}

/*
 * Receive fragment header 
 */

static bool mca_ptl_tcp_recv_frag_header(mca_ptl_tcp_recv_frag_t* frag, int sd, size_t size)
{
    /* non-blocking read - continue if interrupted, otherwise wait until data available */
    unsigned char* ptr = (unsigned char*)&frag->frag_recv.frag_base.frag_header;
    while(frag->frag_hdr_cnt < size) {
        int cnt = recv(sd, ptr + frag->frag_hdr_cnt, size - frag->frag_hdr_cnt, 0);
        if(cnt == 0) {
            mca_ptl_tcp_peer_close(frag->frag_recv.frag_base.frag_peer);
            OMPI_FREE_LIST_RETURN(&mca_ptl_tcp_module.tcp_recv_frags, (ompi_list_item_t*)frag);
            return false;
        }
        if(cnt < 0) {
            switch(errno) {
            case EINTR:
                continue;
            case EWOULDBLOCK:
                /* ompi_output(0, "mca_ptl_tcp_recv_frag_header: EWOULDBLOCK\n"); */
                return false;
            default:
                ompi_output(0, "mca_ptl_tcp_recv_frag_header: recv() failed with errno=%d", errno);
                mca_ptl_tcp_peer_close(frag->frag_recv.frag_base.frag_peer);
                OMPI_FREE_LIST_RETURN(&mca_ptl_tcp_module.tcp_recv_frags, (ompi_list_item_t*)frag);
                return false;
            }
        }
       frag->frag_hdr_cnt += cnt;
#if MCA_PTL_TCP_STATISTICS
       ((mca_ptl_tcp_t*)frag->frag_owner)->ptl_bytes_recv += cnt;
#endif
    }
    return true;
}


/*
 * Receive and process an ack.
 */

static bool mca_ptl_tcp_recv_frag_ack(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    mca_ptl_tcp_send_frag_t* sendfrag;
    mca_pml_base_send_request_t* sendreq;
    sendfrag = (mca_ptl_tcp_send_frag_t*)frag->frag_recv.frag_base.frag_header.hdr_ack.hdr_src_ptr.pval;
    sendreq = sendfrag->frag_send.frag_request;
    sendreq->req_peer_match = frag->frag_recv.frag_base.frag_header.hdr_ack.hdr_dst_match;
    mca_ptl_tcp_send_frag_progress(sendfrag);
    mca_ptl_tcp_recv_frag_return(frag->frag_recv.frag_base.frag_owner, frag);
    return true;
}


/*
 * Receive and process a match request - first fragment.
 */

static bool mca_ptl_tcp_recv_frag_match(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    /* first pass through - attempt a match */
    if(NULL == frag->frag_recv.frag_request && 0 == frag->frag_msg_cnt) {
        /* attempt to match a posted recv */
        if (mca_ptl_base_recv_frag_match( 
            frag->frag_recv.frag_base.frag_owner, 
            &frag->frag_recv, 
            &frag->frag_recv.frag_base.frag_header.hdr_match)) {
            mca_ptl_tcp_recv_frag_matched(frag);
        } else {
            /* match was not made - so allocate buffer for eager send */
            if(frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length > 0) {
                frag->frag_recv.frag_base.frag_addr = 
                    malloc(frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length);
                frag->frag_recv.frag_base.frag_size = 
                    frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length;
                frag->frag_recv.frag_is_buffered = true;
            }
        }
    } 

    /* receive fragment data */
    if(frag->frag_msg_cnt < frag->frag_recv.frag_base.frag_size) {
        if(mca_ptl_tcp_recv_frag_data(frag, sd) == false) {
            return false;
        }
    }

    /* discard any data that exceeds the posted receive */
    if(frag->frag_msg_cnt < frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length)
        if(mca_ptl_tcp_recv_frag_discard(frag, sd) == false) {
            return false;
    }

    /* if fragment has already been matched - go ahead and process */
    if (NULL != frag->frag_recv.frag_request) 
        mca_ptl_tcp_recv_frag_progress(frag);
    return true;
}


/*
 * Receive and process 2nd+ fragments of a multi-fragment message.
 */

static bool mca_ptl_tcp_recv_frag_frag(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    /* get request from header */
    if(frag->frag_msg_cnt == 0) {
        frag->frag_recv.frag_request = frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_dst_ptr.pval;
        mca_ptl_tcp_recv_frag_matched(frag);
    }

    /* continue to receive user data */
    if(frag->frag_msg_cnt < frag->frag_recv.frag_base.frag_size) {
        if(mca_ptl_tcp_recv_frag_data(frag, sd) == false)
            return false;
    }

    if(frag->frag_msg_cnt < frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length)
        if(mca_ptl_tcp_recv_frag_discard(frag, sd) == false)
            return false;

    /* indicate completion status */
    mca_ptl_tcp_recv_frag_progress(frag);
    return true;
}


/*
 * Continue with non-blocking recv() calls until the entire
 * fragment is received.
 */

static bool mca_ptl_tcp_recv_frag_data(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    while(frag->frag_msg_cnt < frag->frag_recv.frag_base.frag_size) {
        int cnt = recv(sd, (unsigned char*)frag->frag_recv.frag_base.frag_addr+frag->frag_msg_cnt,  
            frag->frag_recv.frag_base.frag_size-frag->frag_msg_cnt, 0);
        if(cnt == 0) {
            mca_ptl_tcp_peer_close(frag->frag_recv.frag_base.frag_peer);
            OMPI_FREE_LIST_RETURN(&mca_ptl_tcp_module.tcp_recv_frags, (ompi_list_item_t*)frag);
            return false;
        }
        if(cnt < 0) {
            switch(errno) {
            case EINTR:
                continue;
            case EWOULDBLOCK:
                return false;
            default:
                ompi_output(0, "mca_ptl_tcp_recv_frag_data: recv() failed with errno=%d", errno);
                mca_ptl_tcp_peer_close(frag->frag_recv.frag_base.frag_peer);
                OMPI_FREE_LIST_RETURN(&mca_ptl_tcp_module.tcp_recv_frags, (ompi_list_item_t*)frag);
                return false;
            }
        }
        frag->frag_msg_cnt += cnt;
#if MCA_PTL_TCP_STATISTICS
        ((mca_ptl_tcp_t*)frag->frag_owner)->ptl_bytes_recv += cnt;
#endif
    }
    return true;
}


/*
 *  If the app posted a receive buffer smaller than the
 *  fragment, receive and discard remaining bytes.
*/

static bool mca_ptl_tcp_recv_frag_discard(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    while(frag->frag_msg_cnt < frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length) {
        size_t count = frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length - frag->frag_msg_cnt;
        void *rbuf = malloc(count);
        int cnt = recv(sd, rbuf, count, 0);
        free(rbuf);
        if(cnt == 0) {
            mca_ptl_tcp_peer_close(frag->frag_recv.frag_base.frag_peer);
            OMPI_FREE_LIST_RETURN(&mca_ptl_tcp_module.tcp_recv_frags, (ompi_list_item_t*)frag);
            return false;
        }
        if(cnt < 0) {
            switch(errno) {
            case EINTR:
                continue;
            case EWOULDBLOCK:
                /* ompi_output(0, "mca_ptl_tcp_recv_frag_discard: EWOULDBLOCK\n"); */
                return false;
            default:
                ompi_output(0, "mca_ptl_tcp_recv_frag_discard: recv() failed with errno=%d", errno);
                mca_ptl_tcp_peer_close(frag->frag_recv.frag_base.frag_peer);
                OMPI_FREE_LIST_RETURN(&mca_ptl_tcp_module.tcp_recv_frags, (ompi_list_item_t*)frag);
                return false;
            }
        }
        frag->frag_msg_cnt += cnt;
#if MCA_PTL_TCP_STATISTICS
        ((mca_ptl_tcp_t*)frag->frag_owner)->ptl_bytes_recv += cnt;
#endif
    }
    return true;
}

