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


static void mca_ptl_tcp_send_frag_construct(mca_ptl_tcp_send_frag_t* frag);
static void mca_ptl_tcp_send_frag_destruct(mca_ptl_tcp_send_frag_t* frag);


lam_class_info_t  mca_ptl_tcp_send_frag_t_class_info = {
    "mca_ptl_tcp_send_frag_t",
    CLASS_INFO(mca_ptl_base_send_frag_t),
    (lam_construct_t)mca_ptl_tcp_send_frag_construct,
    (lam_destruct_t)mca_ptl_tcp_send_frag_destruct
};
                                                                                                           

static void mca_ptl_tcp_send_frag_construct(mca_ptl_tcp_send_frag_t* frag)
{
    OBJ_CONSTRUCT_SUPER(frag, mca_ptl_base_send_frag_t);
}


static void mca_ptl_tcp_send_frag_destruct(mca_ptl_tcp_send_frag_t* frag)
{
    OBJ_DESTRUCT_SUPER(frag, mca_ptl_base_send_frag_t);
}


/*
 *  Initialize the fragment based on the current offset into the users
 *  data buffer, and the indicated size.
 */

void mca_ptl_tcp_send_frag_reinit(
    mca_ptl_tcp_send_frag_t* sendfrag,
    mca_ptl_base_peer_t* ptl_peer,
    mca_ptl_base_send_request_t* sendreq,
    size_t size)
{
    /* message header */
    mca_ptl_base_header_t* hdr = &sendfrag->frag_header;
    if(sendreq->req_frags == 0) {
        hdr->hdr_type = MCA_PTL_HDR_TYPE_MATCH;
        hdr->hdr_flags = MCA_PTL_FLAGS_ACK_IMMEDIATE;
        hdr->hdr_size = sizeof(mca_ptl_base_match_header_t);
        hdr->hdr_frag.hdr_frag_offset = sendreq->req_offset;
        hdr->hdr_frag.hdr_frag_seq = 0;
        hdr->hdr_frag.hdr_src_ptr.pval = sendfrag;
        hdr->hdr_frag.hdr_dst_ptr.pval = 0;
        hdr->hdr_match.hdr_contextid = sendreq->super.req_communicator->c_contextid;
        hdr->hdr_match.hdr_src_rank = sendreq->super.req_communicator->c_rank;
        hdr->hdr_match.hdr_dst_rank = sendreq->super.req_peer;
        hdr->hdr_match.hdr_user_tag = sendreq->super.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_length;
        hdr->hdr_match.hdr_msg_seq = 0;
    } else {
        hdr->hdr_type = MCA_PTL_HDR_TYPE_FRAG;
        hdr->hdr_flags = 0;
        hdr->hdr_size = sizeof(mca_ptl_base_frag_header_t);
        hdr->hdr_frag.hdr_frag_offset = sendreq->req_offset;
        hdr->hdr_frag.hdr_frag_seq = 0;
        hdr->hdr_frag.hdr_src_ptr.pval = sendfrag;
        hdr->hdr_frag.hdr_dst_ptr = sendreq->req_peer_request;
    }

    /* update request */
    if(sendreq->req_offset + size > sendreq->req_length)
        size = sendreq->req_length = sendreq->req_offset;
    hdr->hdr_frag.hdr_frag_length = size;
    sendreq->req_offset += size;
    sendreq->req_frags++;

    /* fragment state */
    sendfrag->frag_owner = &ptl_peer->peer_ptl->super;
    sendfrag->super.frag_request = sendreq;
    sendfrag->super.super.frag_addr = sendreq->super.req_addr + hdr->hdr_frag.hdr_frag_offset;
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
                lam_output(0, "mca_ptl_tcp_send_frag_handler: writev failed with errno=%d", errno);
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

    /* done with this frag? */
    return (frag->frag_vec_cnt == 0);
}

