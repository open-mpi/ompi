/* 
 * $HEADER$
 */
/**
 * @file
 */

#ifndef MCA_PTL_TCP_RECV_FRAG_H
#define MCA_PTL_TCP_RECV_FRAG_H

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "ptl_tcp.h"


extern lam_class_t mca_ptl_tcp_recv_frag_t_class;


/**
 *  TCP received fragment derived type.
 */
struct mca_ptl_tcp_recv_frag_t {
    mca_ptl_base_recv_frag_t super; /**< base receive fragment descriptor */
    size_t frag_hdr_cnt;            /**< number of header bytes received */
    size_t frag_msg_cnt;            /**< number of msg bytes received */
    bool frag_ack_pending;          /**< is an ack pending for this fragment */
};
typedef struct mca_ptl_tcp_recv_frag_t mca_ptl_tcp_recv_frag_t;


static inline mca_ptl_tcp_recv_frag_t* mca_ptl_tcp_recv_frag_alloc(int* rc)
{
    return (mca_ptl_tcp_recv_frag_t*)lam_free_list_get(&mca_ptl_tcp_module.tcp_recv_frags, rc);
}

bool mca_ptl_tcp_recv_frag_handler(mca_ptl_tcp_recv_frag_t*, int sd);
void mca_ptl_tcp_recv_frag_init(mca_ptl_tcp_recv_frag_t* frag, struct mca_ptl_base_peer_t* peer);
bool mca_ptl_tcp_recv_frag_send_ack(mca_ptl_tcp_recv_frag_t* frag);


static inline void mca_ptl_tcp_recv_frag_progress(mca_ptl_tcp_recv_frag_t* frag)
{
    if(frag->frag_msg_cnt >= frag->super.super.frag_header.hdr_frag.hdr_frag_length) {
        if(frag->super.frag_is_buffered) {
            struct iovec iov;
            iov.iov_base = frag->super.super.frag_addr;
            iov.iov_len = frag->super.super.frag_size;
            lam_convertor_unpack(&frag->super.super.frag_convertor, &iov, 1);
        }
        frag->super.super.frag_owner->ptl_recv_progress(frag->super.frag_request, &frag->super);
        if(frag->frag_ack_pending == false) {
            mca_ptl_tcp_recv_frag_return(frag->super.super.frag_owner, frag);
        }
    }
}

#endif

