/* @file
 *
 * $HEADER$
 */

#ifndef MCA_PTL_TCP_PEER_H
#define MCA_PTL_TCP_PEER_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "lam/lfc/list.h"
#include "lam/util/reactor.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"
#include "ptl_tcp_recvfrag.h"
#include "ptl_tcp_sendfrag.h"


typedef enum { 
    MCA_PTL_TCP_CLOSED, 
    MCA_PTL_TCP_CONNECTING,
    MCA_PTL_TCP_CONNECT_ACK,
    MCA_PTL_TCP_CONNECTED,
    MCA_PTL_TCP_FAILED
} mca_ptl_tcp_state_t;
    

extern lam_class_info_t mca_ptl_tcp_peer_cls;

/**
 *  An abstraction that represents a connection to a peer process.
 *  An instance of mca_ptl_base_peer_t is associated w/ each process 
 *  and PTL pair at startup. However, connections to the peer
 *  are established dynamically on an as-needed basis:
*/
                                                                                                                            
struct mca_ptl_base_peer_t {
    lam_list_item_t            super;
    struct mca_ptl_tcp_t*      peer_ptl;
    struct mca_ptl_tcp_proc_t* peer_proc;
    struct mca_ptl_tcp_addr_t* peer_addr;
    int                        peer_sd;
    mca_ptl_tcp_send_frag_t*   peer_send_frag;
    mca_ptl_tcp_recv_frag_t*   peer_recv_frag;
    mca_ptl_tcp_state_t        peer_state;
    size_t                     peer_retries;
    lam_list_t                 peer_frags;
    lam_mutex_t                peer_lock;
};
typedef struct mca_ptl_base_peer_t mca_ptl_base_peer_t;


void mca_ptl_tcp_peer_close(mca_ptl_base_peer_t*);
int  mca_ptl_tcp_peer_send(mca_ptl_base_peer_t*, mca_ptl_tcp_send_frag_t*);
bool mca_ptl_tcp_peer_accept(mca_ptl_base_peer_t*, struct sockaddr_in*, int);

#endif

