/*
 * $HEADER$
 */
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "ptl_tcp.h"
#include "ptl_tcp_peer.h"
#include "ptl_tcp_recvfrag.h"


static void mca_ptl_tcp_recv_frag_init(mca_ptl_tcp_recv_frag_t* frag);
static void mca_ptl_tcp_recv_frag_destroy(mca_ptl_tcp_recv_frag_t* frag);
static bool mca_ptl_tcp_recv_frag_header(mca_ptl_tcp_recv_frag_t* frag, int sd, size_t);
static bool mca_ptl_tcp_recv_frag_ack(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_frag(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_match(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_data(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_discard(mca_ptl_tcp_recv_frag_t* frag, int sd);


lam_class_info_t  mca_ptl_tcp_recv_frag_cls = {
    "mca_ptl_tcp_recv_frag_t",
    &mca_ptl_base_recv_frag_cls,
    (class_init_t)mca_ptl_tcp_recv_frag_init,
    (class_destroy_t)mca_ptl_tcp_recv_frag_destroy
};
                                                                                                           

static void mca_ptl_tcp_recv_frag_init(mca_ptl_tcp_recv_frag_t* frag)
{
    SUPER_INIT(frag, &mca_ptl_base_recv_frag_cls);
}


static void mca_ptl_tcp_recv_frag_destroy(mca_ptl_tcp_recv_frag_t* frag)
{
    SUPER_DESTROY(frag, &mca_ptl_base_recv_frag_cls);
}


void mca_ptl_tcp_recv_frag_reinit(mca_ptl_tcp_recv_frag_t* frag, mca_ptl_base_peer_t* peer)
{
    frag->frag_owner = &peer->peer_ptl->super;
    frag->super.frag_request = 0;
    frag->frag_peer = peer;
    frag->frag_addr = 0;
    frag->frag_size = 0;
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
    }

    /* is the entire common header available? */
    frag->frag_hdr_cnt += cnt;
    return (frag->frag_hdr_cnt == size);
}


static bool mca_ptl_tcp_recv_frag_ack(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    if (frag->frag_hdr_cnt < sizeof(mca_ptl_base_ack_header_t))
        if (mca_ptl_tcp_recv_frag_header(frag, sd, sizeof(mca_ptl_base_ack_header_t)) == false)
            return false;
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
        if(NULL == frag->super.frag_request) {
            frag->frag_addr = malloc(frag->frag_header.hdr_frag.hdr_frag_length);
            frag->frag_size = frag->frag_header.hdr_frag.hdr_frag_length;
        } else {
            frag->frag_addr = (unsigned char*)frag->super.super.frag_addr;
            frag->frag_size = frag->super.super.frag_size;
        }
        if(mca_ptl_tcp_recv_frag_data(frag, sd) == false)
            return false;

    } else if(frag->frag_msg_cnt < frag->super.super.frag_size) {
        if(mca_ptl_tcp_recv_frag_data(frag, sd) == false)
            return false;
    }

    if(frag->frag_msg_cnt < frag->frag_header.hdr_frag.hdr_frag_length)
        if(mca_ptl_tcp_recv_frag_discard(frag, sd) == false)
            return false;

    if(NULL != frag->super.frag_request) {
        /* indicate completion status */
        mca_ptl_base_recv_request_progress(frag->super.frag_request, &frag->super);
    }
    return true;
}


static bool mca_ptl_tcp_recv_frag_frag(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    if(frag->frag_hdr_cnt < sizeof(mca_ptl_base_match_header_t))
       if(mca_ptl_tcp_recv_frag_header(frag, sd, sizeof(mca_ptl_base_match_header_t)) == false)
            return false;

    if(frag->frag_msg_cnt == 0) {
        /* determine offset into user buffer or allocate buffer for non-contig data */
    } else if(frag->frag_msg_cnt < frag->super.super.frag_size) {
        if(mca_ptl_tcp_recv_frag_data(frag, sd) == false)
            return false;
    }

    if(frag->frag_msg_cnt < frag->frag_header.hdr_frag.hdr_frag_length)
        if(mca_ptl_tcp_recv_frag_discard(frag, sd) == false)
            return false;

    /* indicate completion status */
    mca_ptl_base_recv_request_progress(frag->super.frag_request, &frag->super);
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
        cnt = recv(sd, (unsigned char*)frag->frag_addr+frag->frag_msg_cnt, frag->frag_size-frag->frag_msg_cnt, 0);
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
    return (frag->frag_msg_cnt >= frag->frag_size);
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
