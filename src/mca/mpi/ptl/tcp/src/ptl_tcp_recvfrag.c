/*
 * $HEADER$
 */
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "ptl_tcp.h"
#include "ptl_tcp_peer.h"
#include "ptl_tcp_recvfrag.h"
#include "ptl_tcp_sendfrag.h"


#define frag_header super.super.frag_header
#define frag_peer   super.super.frag_peer
#define frag_owner  super.super.frag_owner


static void mca_ptl_tcp_recv_frag_construct(mca_ptl_tcp_recv_frag_t* frag);
static void mca_ptl_tcp_recv_frag_destruct(mca_ptl_tcp_recv_frag_t* frag);
static bool mca_ptl_tcp_recv_frag_header(mca_ptl_tcp_recv_frag_t* frag, int sd, size_t);
static bool mca_ptl_tcp_recv_frag_ack(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_frag(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_match(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_data(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_discard(mca_ptl_tcp_recv_frag_t* frag, int sd);


lam_class_t  mca_ptl_tcp_recv_frag_t_class = {
    "mca_ptl_tcp_recv_frag_t",
    OBJ_CLASS(mca_ptl_base_recv_frag_t),
    (lam_construct_t)mca_ptl_tcp_recv_frag_construct,
    (lam_destruct_t)mca_ptl_tcp_recv_frag_destruct
};
                                                                                                           

static void mca_ptl_tcp_recv_frag_construct(mca_ptl_tcp_recv_frag_t* frag)
{
}


static void mca_ptl_tcp_recv_frag_destruct(mca_ptl_tcp_recv_frag_t* frag)
{
}


void mca_ptl_tcp_recv_frag_reinit(mca_ptl_tcp_recv_frag_t* frag, mca_ptl_base_peer_t* peer)
{
    frag->frag_owner = &peer->peer_ptl->super;
    frag->super.frag_request = 0;
    frag->frag_peer = peer;
    frag->frag_buff = NULL;
    frag->frag_hdr_cnt = 0;
    frag->frag_msg_cnt = 0;
}
                                                                                                                

bool mca_ptl_tcp_recv_frag_handler(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    /* read common header */
    if(frag->frag_hdr_cnt < sizeof(mca_ptl_base_common_header_t))
        if(mca_ptl_tcp_recv_frag_header(frag, sd, sizeof(mca_ptl_base_common_header_t)) == false)
            return false;

    switch(frag->frag_header.hdr_type) {
    case MCA_PTL_HDR_TYPE_MATCH:
         return mca_ptl_tcp_recv_frag_match(frag, sd);
    case MCA_PTL_HDR_TYPE_FRAG:
        return mca_ptl_tcp_recv_frag_frag(frag, sd);
    case MCA_PTL_HDR_TYPE_ACK: 
    case MCA_PTL_HDR_TYPE_NACK:
        return mca_ptl_tcp_recv_frag_ack(frag, sd);
    default:
        return true; 
    }
}


static bool mca_ptl_tcp_recv_frag_header(mca_ptl_tcp_recv_frag_t* frag, int sd, size_t size)
{
    /* non-blocking read - continue if interrupted, otherwise wait until data available */
    unsigned char* ptr = (unsigned char*)&frag->frag_header;
    int cnt = -1;
    while(cnt < 0) {
        cnt = recv(sd, ptr + frag->frag_hdr_cnt, size - frag->frag_hdr_cnt, 0);
        if(cnt == 0) {
            mca_ptl_tcp_peer_close(frag->frag_peer);
            lam_free_list_return(&mca_ptl_tcp_module.tcp_recv_frags, (lam_list_item_t*)frag);
            return false;
        }
        if(cnt < 0) {
            switch(errno) {
            case EINTR:
                continue;
            case EWOULDBLOCK:
                return false;
            default:
                lam_output(0, "mca_ptl_tcp_recv_frag_header: recv() failed with errno=%d", errno);
                mca_ptl_tcp_peer_close(frag->frag_peer);
                lam_free_list_return(&mca_ptl_tcp_module.tcp_recv_frags, (lam_list_item_t*)frag);
                return false;
            }
        }
    frag->frag_hdr_cnt += cnt;
    }

    /* is the entire common header available? */
    return (frag->frag_hdr_cnt == size);
}


static bool mca_ptl_tcp_recv_frag_ack(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    mca_ptl_tcp_send_frag_t* sendfrag;
    if (frag->frag_hdr_cnt < sizeof(mca_ptl_base_ack_header_t))
        if (mca_ptl_tcp_recv_frag_header(frag, sd, sizeof(mca_ptl_base_ack_header_t)) == false)
            return false;

    sendfrag = (mca_ptl_tcp_send_frag_t*)frag->frag_header.hdr_ack.hdr_src_ptr.pval;
    sendfrag->super.frag_request->req_peer_request = frag->frag_header.hdr_ack.hdr_dst_ptr;
    sendfrag->frag_owner->ptl_send_progress(sendfrag->super.frag_request, &sendfrag->super);
    /* don't return first fragment - it is returned along with the request */
    return true;
}


static bool mca_ptl_tcp_recv_frag_match(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    if(frag->frag_hdr_cnt < sizeof(mca_ptl_base_match_header_t))
       if(mca_ptl_tcp_recv_frag_header(frag, sd, sizeof(mca_ptl_base_match_header_t)) == false)
            return false;

    if(frag->frag_msg_cnt == 0) {

        /* attempt to match a posted recv */
        mca_ptl_base_recv_frag_match(&frag->super, &frag->frag_header.hdr_match);

        /* match was not made - so allocate buffer for eager send */
        if (NULL == frag->super.frag_request) {

            if(frag->frag_header.hdr_frag.hdr_frag_length > 0) {
                frag->frag_buff = malloc(frag->frag_header.hdr_frag.hdr_frag_length);
                frag->super.super.frag_size = frag->frag_header.hdr_frag.hdr_frag_length;
            }

        /* match was made - use application buffer */
        } else {
            frag->frag_buff = (unsigned char*)frag->super.super.frag_addr;
        }
    } 

    if(frag->frag_msg_cnt < frag->super.super.frag_size) {
        if(mca_ptl_tcp_recv_frag_data(frag, sd) == false)
            return false;
    }

    if(frag->frag_msg_cnt < frag->frag_header.hdr_frag.hdr_frag_length)
        if(mca_ptl_tcp_recv_frag_discard(frag, sd) == false)
            return false;

    /* if fragment has already been matched - go ahead and process */
    if (frag->super.frag_request != NULL)
        mca_ptl_tcp_recv_frag_process(frag);
    return true;
}


static bool mca_ptl_tcp_recv_frag_frag(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    if(frag->frag_hdr_cnt < sizeof(mca_ptl_base_match_header_t))
       if(mca_ptl_tcp_recv_frag_header(frag, sd, sizeof(mca_ptl_base_match_header_t)) == false)
            return false;

    /* get request from header */
    if(frag->frag_msg_cnt == 0) {
        frag->super.frag_request = frag->frag_header.hdr_frag.hdr_dst_ptr.pval;
        mca_ptl_base_recv_frag_init(&frag->super);
    }

    /* continue to receive user data */
    if(frag->frag_msg_cnt < frag->super.super.frag_size) {
        if(mca_ptl_tcp_recv_frag_data(frag, sd) == false)
            return false;
    }

    if(frag->frag_msg_cnt < frag->frag_header.hdr_frag.hdr_frag_length)
        if(mca_ptl_tcp_recv_frag_discard(frag, sd) == false)
            return false;

    /* indicate completion status */
    mca_ptl_tcp_recv_frag_process(frag);
    return true;
}



/*
 * Continue with non-blocking recv() calls until the entire
 * fragment is received.
 */

static bool mca_ptl_tcp_recv_frag_data(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    int cnt = -1;
    while(cnt < 0) {
        cnt = recv(sd, (unsigned char*)frag->frag_buff+frag->frag_msg_cnt,  
            frag->super.super.frag_size-frag->frag_msg_cnt, 0);
        if(cnt == 0) {
            mca_ptl_tcp_peer_close(frag->frag_peer);
            lam_free_list_return(&mca_ptl_tcp_module.tcp_recv_frags, (lam_list_item_t*)frag);
            return false;
        }
        if(cnt < 0) {
            switch(errno) {
            case EINTR:
                continue;
            case EWOULDBLOCK:
                return false;
            default:
                lam_output(0, "mca_ptl_tcp_recv_frag_data: recv() failed with errno=%d", errno);
                mca_ptl_tcp_peer_close(frag->frag_peer);
                lam_free_list_return(&mca_ptl_tcp_module.tcp_recv_frags, (lam_list_item_t*)frag);
                return false;
            }
        }
    }
    frag->frag_msg_cnt += cnt;
    return (frag->frag_msg_cnt >= frag->super.super.frag_size);
}


/*
 *  If the app posted a receive buffer smaller than the
 *  fragment, receive and discard remaining bytes.
*/

static bool mca_ptl_tcp_recv_frag_discard(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    int cnt = -1;
    while(cnt < 0) {
        void *rbuf = malloc(frag->frag_header.hdr_frag.hdr_frag_length - frag->frag_msg_cnt);
        cnt = recv(sd, rbuf, frag->frag_header.hdr_frag.hdr_frag_length - frag->frag_msg_cnt, 0);
        free(rbuf);
        if(cnt == 0) {
            mca_ptl_tcp_peer_close(frag->frag_peer);
            lam_free_list_return(&mca_ptl_tcp_module.tcp_recv_frags, (lam_list_item_t*)frag);
            return false;
        }
        if(cnt < 0) {
            switch(errno) {
            case EINTR:
                continue;
            case EWOULDBLOCK:
                return false;
            default:
                lam_output(0, "mca_ptl_tcp_recv_frag_discard: recv() failed with errno=%d", errno);
                mca_ptl_tcp_peer_close(frag->frag_peer);
                lam_free_list_return(&mca_ptl_tcp_module.tcp_recv_frags, (lam_list_item_t*)frag);
                return false;
            }
        }
    }
    frag->frag_msg_cnt += cnt;
    return (frag->frag_msg_cnt >= frag->frag_header.hdr_frag.hdr_frag_length);
}


/*
 *  Queue up an acknowledgment to the peer.
*/

bool mca_ptl_tcp_recv_frag_cts(mca_ptl_tcp_recv_frag_t* frag) 
{
    int rc;
    mca_ptl_tcp_send_frag_t* ack = mca_ptl_tcp_send_frag_alloc(&rc);
    mca_ptl_base_peer_t* ptl_peer = frag->frag_peer;
    mca_ptl_base_header_t* hdr;
    if (NULL == ack)
        return false;

    hdr = &ack->frag_header;
    hdr->hdr_type = MCA_PTL_HDR_TYPE_ACK;
    hdr->hdr_flags = 0;
    hdr->hdr_size = sizeof(mca_ptl_base_ack_header_t);
    hdr->hdr_ack.hdr_src_ptr = frag->frag_header.hdr_frag.hdr_src_ptr;
    hdr->hdr_ack.hdr_dst_ptr.pval = frag->super.frag_request;
    ack->frag_owner = &ptl_peer->peer_ptl->super;
    ack->frag_peer = ptl_peer;
    ack->super.frag_request = 0;
    ack->super.super.frag_addr = NULL;
    ack->super.super.frag_size = 0;
    ack->frag_vec_ptr = ack->frag_vec;
    ack->frag_vec[0].iov_base = (lam_iov_base_ptr_t)hdr;
    ack->frag_vec[0].iov_len = sizeof(mca_ptl_base_ack_header_t);
    ack->frag_vec_cnt = 1;
    frag->frag_acked = (mca_ptl_tcp_peer_send(ptl_peer, ack) == LAM_SUCCESS);
    return frag->frag_acked;
}


/*
 */

void mca_ptl_tcp_recv_frag_process(mca_ptl_tcp_recv_frag_t* frag)
{
    if(frag->frag_msg_cnt >= frag->frag_header.hdr_frag.hdr_frag_length) {
        if(frag->frag_buff != frag->super.super.frag_addr) {
            memcpy(frag->super.super.frag_addr, frag->frag_buff, frag->super.super.frag_size);
        }
        frag->frag_owner->ptl_recv_progress(frag->super.frag_request, &frag->super);
        if(frag->frag_acked == true) {
            mca_ptl_tcp_recv_frag_return(frag);
        }
    }
}

