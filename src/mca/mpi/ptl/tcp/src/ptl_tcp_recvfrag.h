/* @file
 *
 * $HEADER$
 */

#ifndef MCA_PTL_TCP_RECV_FRAG_H
#define MCA_PTL_TCP_RECV_FRAG_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_recvfrag.h"
#include "ptl_tcp.h"


extern lam_class_t mca_ptl_tcp_recv_frag_t_class;


struct mca_ptl_tcp_recv_frag_t {
    mca_ptl_base_recv_frag_t super;
    mca_ptl_base_ack_header_t frag_ack;
    unsigned char* frag_addr;
    size_t frag_size;
    size_t frag_hdr_cnt;
    size_t frag_msg_cnt;
    bool frag_acked;
#define frag_peer super.super.frag_peer
#define frag_owner super.super.frag_owner
#define frag_header super.super.frag_header
};
typedef struct mca_ptl_tcp_recv_frag_t mca_ptl_tcp_recv_frag_t;


static inline mca_ptl_tcp_recv_frag_t* mca_ptl_tcp_recv_frag_alloc(int* rc)
{
    return (mca_ptl_tcp_recv_frag_t*)lam_free_list_get(&mca_ptl_tcp_module.tcp_recv_frags, rc);
}

static inline void mca_ptl_tcp_recv_frag_return(mca_ptl_tcp_recv_frag_t* frag)
{
    if(frag->frag_addr != frag->super.super.frag_addr)
        free(frag->frag_addr);
    lam_free_list_return(&mca_ptl_tcp_module.tcp_recv_frags, (lam_list_item_t*)frag);
}

bool mca_ptl_tcp_recv_frag_handler(mca_ptl_tcp_recv_frag_t*, int sd);
void mca_ptl_tcp_recv_frag_reinit(mca_ptl_tcp_recv_frag_t* frag, struct mca_ptl_base_peer_t* peer);
bool mca_ptl_tcp_recv_frag_cts(mca_ptl_tcp_recv_frag_t* frag);
void mca_ptl_tcp_recv_frag_process(mca_ptl_tcp_recv_frag_t* frag);


#endif

