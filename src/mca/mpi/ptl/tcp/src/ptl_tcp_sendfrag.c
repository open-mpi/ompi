/*
 * $HEADER$
 */
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "lam/types.h"
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "ptl_tcp.h"
#include "ptl_tcp_peer.h"
#include "ptl_tcp_sendfrag.h"


lam_class_info_t  mca_ptl_tcp_send_frag_cls = {
    "mca_ptl_tcp_send_frag_t",
    &mca_ptl_base_send_frag_cls,
    (class_init_t)mca_ptl_tcp_send_frag_init,
    (class_destroy_t)mca_ptl_tcp_send_frag_destroy
};
                                                                                                           

void mca_ptl_tcp_send_frag_init(mca_ptl_tcp_send_frag_t* frag)
{
    SUPER_INIT(frag, &mca_ptl_base_send_frag_cls);
}


void mca_ptl_tcp_send_frag_destroy(mca_ptl_tcp_send_frag_t* frag)
{
    SUPER_DESTROY(frag, &mca_ptl_base_send_frag_cls);
}

/*
 *  Initialize the fragment based on the current offset into the users
 *  data buffer, and the indicated size.
 */

void mca_ptl_tcp_send_frag_reinit(
    mca_ptl_tcp_send_frag_t* sendfrag,
    mca_ptl_peer_t* ptl_peer,
    mca_ptl_base_send_request_t* sendreq,
    size_t size)
{
    /* message header */
    mca_ptl_base_header_t* hdr = &sendfrag->frag_header;
    hdr->hdr_contextid = sendreq->super.req_communicator->c_contextid;
    hdr->hdr_src_rank = sendreq->super.req_communicator->c_rank;
    hdr->hdr_dst_rank = sendreq->super.req_peer;
    hdr->hdr_user_tag = sendreq->super.req_tag;
    hdr->hdr_msg_type = sendreq->req_send_mode;
    hdr->hdr_msg_length = sendreq->req_length;
    hdr->hdr_msg_offset = sendreq->req_offset;
    hdr->hdr_msg_seq = 0;
    hdr->hdr_frag_seq = 0;

    /* update request */
    if(sendreq->req_offset + size > sendreq->req_length)
        size = sendreq->req_length = sendreq->req_offset;
    hdr->hdr_frag_length = size;
    sendreq->req_offset += size;
    sendreq->req_frags++;

    /* fragment state */
    sendfrag->frag_owner = &ptl_peer->peer_ptl->super;
    sendfrag->super.frag_request = sendreq;
    sendfrag->super.super.frag_addr = sendreq->super.req_addr + hdr->hdr_msg_offset;
    sendfrag->super.super.frag_size = size;

    sendfrag->frag_peer = ptl_peer;
    sendfrag->frag_vec_ptr = sendfrag->frag_vec;
    sendfrag->frag_vec[0].iov_base = (lam_iov_base_ptr_t)hdr;
    sendfrag->frag_vec[0].iov_len = sizeof(mca_ptl_base_header_t);
    sendfrag->frag_vec_cnt = 1;
    if(size > 0) {
        sendfrag->frag_vec[1].iov_base = (lam_iov_base_ptr_t)sendfrag->super.super.frag_addr;
        sendfrag->frag_vec[1].iov_len = sendfrag->super.super.frag_size;
        sendfrag->frag_vec_cnt++;
    }
}


/*
 * The socket is setup as non-blocking, writes are handled asynchronously,
 * with callbacks from the reactor when the socket is ready for writes.
 */

bool mca_ptl_tcp_send_frag_handler(mca_ptl_tcp_send_frag_t* frag, int sd)
{
    int cnt=-1;
    size_t i;

    /* non-blocking write, but continue if interrupted */
    while(cnt < 0) {
        cnt = writev(sd, frag->frag_vec_ptr, frag->frag_vec_cnt);
        if(cnt < 0) {
            switch(errno) {
            case EINTR:
                continue;
            case EWOULDBLOCK:
                return false;
            default:
                {
                lam_output(0, "mca_ptl_tcp_send_frag_handler: writev failedd with errno=%d", errno);
                mca_ptl_tcp_peer_close(frag->frag_peer);
                return false;
                }
            }
        }
    }
                                                                                                     
    /* if the write didn't complete - update the iovec state */
    size_t num_vecs = frag->frag_vec_cnt;
    for(i=0; i<num_vecs; i++) {
        if(cnt >= (int)frag->frag_vec_ptr->iov_len) {
            cnt -= frag->frag_vec_ptr->iov_len;
            frag->frag_vec_ptr++;
            frag->frag_vec_cnt--;
        } else {
            frag->frag_vec_ptr->iov_base = (lam_iov_base_ptr_t)
                (((unsigned char*)frag->frag_vec_ptr->iov_base) + cnt);
            frag->frag_vec_ptr->iov_len -= cnt;
            break;
        }
    }
    return (frag->frag_vec_cnt == 0);
}

