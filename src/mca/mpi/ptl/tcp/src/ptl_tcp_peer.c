#include <stdlib.h>
#include <unistd.h>
#include "ptl_tcp_peer.h"


lam_class_info_t  mca_ptl_addr_cls = {
    "mca_ptl_addr_t", 
    &lam_list_cls,
    (class_init_t)mca_ptl_tcp_peer_init, 
    (class_destroy_t)mca_ptl_tcp_peer_destroy
};


void mca_ptl_tcp_peer_init(mca_ptl_peer_t* ptl_peer)
{
    SUPER_INIT(ptl_peer, &lam_list_cls);
    ptl_peer->tcp_state = MCA_PTL_TCP_CLOSED;
    ptl_peer->tcp_sd = -1;
}

void mca_ptl_tcp_peer_destroy(mca_ptl_peer_t* ptl_peer)
{
    mca_ptl_tcp_peer_close(ptl_peer);
    SUPER_DESTROY(ptl_peer, &lam_list_cls);
}

void mca_ptl_tcp_peer_close(mca_ptl_peer_t* ptl_peer)
{
    if(ptl_peer->tcp_sd >= 0) {
        close(ptl_peer->tcp_sd);
        ptl_peer->tcp_sd = -1;
    }
    ptl_peer->tcp_state = MCA_PTL_TCP_CLOSED;
}

