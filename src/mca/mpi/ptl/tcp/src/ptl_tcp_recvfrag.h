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


extern lam_class_info_t mca_ptl_tcp_recv_frag_cls;


struct mca_ptl_tcp_recv_frag_t {
    mca_ptl_base_recv_frag_t super;
    unsigned char* frag_addr;
    size_t frag_size;
    size_t frag_hdr_cnt;
    size_t frag_msg_cnt;
#define frag_peer super.super.frag_peer
#define frag_owner super.super.frag_owner
#define frag_header super.super.frag_header
};
typedef struct mca_ptl_tcp_recv_frag_t mca_ptl_tcp_recv_frag_t;


void mca_ptl_tcp_recv_frag_init(mca_ptl_tcp_recv_frag_t*);
void mca_ptl_tcp_recv_frag_destroy(mca_ptl_tcp_recv_frag_t*);
bool mca_ptl_tcp_recv_frag_handler(mca_ptl_tcp_recv_frag_t*, int sd);
void mca_ptl_tcp_recv_frag_reinit(mca_ptl_tcp_recv_frag_t* frag, struct mca_ptl_peer_t* peer);


#endif

