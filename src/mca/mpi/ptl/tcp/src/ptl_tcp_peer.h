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


typedef enum { 
    MCA_PTL_TCP_CLOSED, 
    MCA_PTL_TCP_CONNECTING,
    MCA_PTL_TCP_CONNECT_ACK,
    MCA_PTL_TCP_CONNECTED,
    MCA_PTL_TCP_FAILED
} mca_ptl_tcp_state_t;
    

extern lam_class_info_t mca_ptl_peer_cls;

/**
 *  An abstraction that represents a connection to a peer process.
 *  An instance of mca_ptl_peer_t is associated w/ each process 
 *  in the group at startup. However, connections to the peer
 *  are established dynamically on an as-needed basis:
*/
                                                                                                                            
struct mca_ptl_peer_t {
    lam_list_item_t     super;
    struct lam_proc_t  *tcp_proc;
    struct sockaddr_in  tcp_peer;
    int                 tcp_sd;
    mca_ptl_tcp_state_t tcp_state;
};
typedef struct mca_ptl_peer_t mca_ptl_peer_t;


void mca_ptl_tcp_peer_init(mca_ptl_peer_t*);
void mca_ptl_tcp_peer_destroy(mca_ptl_peer_t*);
void mca_ptl_tcp_peer_close(mca_ptl_peer_t*);

#endif

