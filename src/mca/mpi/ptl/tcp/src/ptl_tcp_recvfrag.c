/*
 * $HEADER$
 */
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "ptl_tcp.h"
#include "ptl_tcp_peer.h"
#include "ptl_tcp_recvfrag.h"


lam_class_info_t  mca_ptl_tcp_recv_frag_cls = {
    "mca_ptl_tcp_recv_frag_t",
    &mca_ptl_base_recv_frag_cls,
    (class_init_t)mca_ptl_tcp_recv_frag_init,
    (class_destroy_t)mca_ptl_tcp_recv_frag_destroy
};
                                                                                                           
static bool mca_ptl_tcp_recv_frag_header(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_data(mca_ptl_tcp_recv_frag_t* frag, int sd);
static bool mca_ptl_tcp_recv_frag_discard(mca_ptl_tcp_recv_frag_t* frag, int sd);



void mca_ptl_tcp_recv_frag_init(mca_ptl_tcp_recv_frag_t* frag)
{
    SUPER_INIT(frag, &mca_ptl_base_recv_frag_cls);
}

void mca_ptl_tcp_recv_frag_destroy(mca_ptl_tcp_recv_frag_t* frag)
{
    SUPER_DESTROY(frag, &mca_ptl_base_recv_frag_cls);
}

void mca_ptl_tcp_recv_frag_reinit(mca_ptl_tcp_recv_frag_t* frag, mca_ptl_peer_t* peer)
{
    frag->frag_owner = &peer->peer_ptl->super;
    frag->frag_match = 0;
    frag->frag_peer = peer;
    frag->frag_addr = 0;
    frag->frag_size = 0;
    frag->frag_hdr_cnt = 0;
    frag->frag_msg_cnt = 0;
}

bool mca_ptl_tcp_recv_frag_handler(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    if(frag->frag_hdr_cnt < sizeof(mca_ptl_base_header_t))
        if(mca_ptl_tcp_recv_frag_header(frag, sd) == false)
            return false;

    if(frag->frag_msg_cnt < frag->frag_size)
        if(mca_ptl_tcp_recv_frag_data(frag, sd) == false)
            return false;

    if(frag->frag_msg_cnt < frag->frag_header.hdr_frag_length)
        if(mca_ptl_tcp_recv_frag_discard(frag, sd) == false)
            return false;

    /* done - do something */
    return true; 
}

static bool mca_ptl_tcp_recv_frag_header(mca_ptl_tcp_recv_frag_t* frag, int sd)
{
    /* non-blocking read - continue if interrupted, otherwise wait until data available */
    unsigned char* ptr = (unsigned char*)&frag->frag_header;
    int cnt = -1;
    while(cnt < 0) {
        cnt = recv(sd, ptr + frag->frag_hdr_cnt, sizeof(mca_ptl_base_header_t) - frag->frag_hdr_cnt, 0);
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
                                                                                                                              
    /* is the entire header available? */
    frag->frag_hdr_cnt += cnt;
    if(frag->frag_hdr_cnt < sizeof(mca_ptl_base_header_t))
        return false;

    /* attempt to match a posted recv */
    /* ????? */

    /* match was not made - so allocate buffer for eager send */
    if(NULL == frag->frag_match) {
        frag->frag_addr = (unsigned char*)LAM_MALLOC(frag->frag_header.hdr_frag_length);
        frag->frag_size = frag->frag_header.hdr_frag_length;
    }
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
        void *rbuf = LAM_MALLOC(frag->frag_header.hdr_frag_length - frag->frag_msg_cnt);
        cnt = recv(sd, rbuf, frag->frag_header.hdr_frag_length - frag->frag_msg_cnt, 0);
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
    return (frag->frag_msg_cnt >= frag->frag_header.hdr_frag_length);
}
                                                                                                                            
                                                                                                                            

