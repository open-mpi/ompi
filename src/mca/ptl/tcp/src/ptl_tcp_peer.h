/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_TCP_PEER_H
#define MCA_PTL_TCP_PEER_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "lfc/lam_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "ptl_tcp_recvfrag.h"
#include "ptl_tcp_sendfrag.h"


typedef enum { 
    MCA_PTL_TCP_CLOSED, 
    MCA_PTL_TCP_CONNECTING,
    MCA_PTL_TCP_CONNECT_ACK,
    MCA_PTL_TCP_CONNECTED,
    MCA_PTL_TCP_FAILED
} mca_ptl_tcp_state_t;
    

/**
 *  An abstraction that represents a connection to a peer process.
 *  An instance of mca_ptl_base_peer_t is associated w/ each process 
 *  and PTL pair at startup. However, connections to the peer
 *  are established dynamically on an as-needed basis:
*/
                                                                                                                            
struct mca_ptl_base_peer_t {
    lam_list_item_t            super;
    struct mca_ptl_tcp_t*      peer_ptl;         /**< PTL instance that created this connection */
    struct mca_ptl_tcp_proc_t* peer_proc;        /**< proc structure corresponding to peer */
    struct mca_ptl_tcp_addr_t* peer_addr;        /**< address of peer */
    int                        peer_sd;          /**< socket connection to peer */
    mca_ptl_tcp_send_frag_t*   peer_send_frag;   /**< current send frag being processed */
    mca_ptl_tcp_recv_frag_t*   peer_recv_frag;   /**< current recv frag being processed */
    mca_ptl_tcp_state_t        peer_state;       /**< current state of the connection */
    size_t                     peer_retries;     /**< number of connection retries attempted */
    lam_list_t                 peer_frags;       /**< list of pending frags to send */
    lam_mutex_t                peer_lock;        /**< lock for concurrent access to peer state */
    lam_event_t                peer_send_event;  /**< event for async processing of send frags */
    lam_event_t                peer_recv_event;  /**< event for async processing of recv frags */
};
typedef struct mca_ptl_base_peer_t mca_ptl_base_peer_t;

extern lam_class_t mca_ptl_tcp_peer_t_class;
typedef struct mca_ptl_base_peer_t mca_ptl_tcp_peer_t;

void mca_ptl_tcp_peer_close(mca_ptl_base_peer_t*);
int  mca_ptl_tcp_peer_send(mca_ptl_base_peer_t*, mca_ptl_tcp_send_frag_t*);
bool mca_ptl_tcp_peer_accept(mca_ptl_base_peer_t*, struct sockaddr_in*, int);

#endif

