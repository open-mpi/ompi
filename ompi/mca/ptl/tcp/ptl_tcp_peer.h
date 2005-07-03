/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_TCP_PEER_H
#define MCA_PTL_TCP_PEER_H

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#include "class/ompi_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * State of TCP peer connection.
 */

typedef enum { 
    MCA_PTL_TCP_CLOSED, 
    MCA_PTL_TCP_CONNECTING,
    MCA_PTL_TCP_CONNECT_ACK,
    MCA_PTL_TCP_CONNECTED,
    MCA_PTL_TCP_SHUTDOWN,
    MCA_PTL_TCP_FAILED
} mca_ptl_tcp_state_t;
    

/**
 *  An abstraction that represents a connection to a peer process.
 *  An instance of mca_ptl_base_peer_t is associated w/ each process 
 *  and PTL pair at startup. However, connections to the peer
 *  are established dynamically on an as-needed basis:
*/
struct mca_ptl_base_peer_t {
    ompi_list_item_t                super;
    struct mca_ptl_tcp_module_t*    peer_ptl;         /**< PTL instance that created this connection */
    struct mca_ptl_tcp_proc_t*      peer_proc;        /**< proc structure corresponding to peer */
    struct mca_ptl_tcp_addr_t*      peer_addr;        /**< address of peer */
    int                             peer_sd;          /**< socket connection to peer */
    struct mca_ptl_tcp_send_frag_t* peer_send_frag;   /**< current send frag being processed */
    struct mca_ptl_tcp_recv_frag_t* peer_recv_frag;   /**< current recv frag being processed */
    mca_ptl_tcp_state_t             peer_state;       /**< current state of the connection */
    size_t                          peer_retries;     /**< number of connection retries attempted */
    ompi_list_t                     peer_frags;       /**< list of pending frags to send */
    ompi_mutex_t                    peer_send_lock;   /**< lock for concurrent access to peer state */
    ompi_mutex_t                    peer_recv_lock;   /**< lock for concurrent access to peer state */
    ompi_event_t                    peer_send_event;  /**< event for async processing of send frags */
    ompi_event_t                    peer_recv_event;  /**< event for async processing of recv frags */
    bool                            peer_nbo;         /**< convert headers to network byte order? */
};
typedef struct mca_ptl_base_peer_t mca_ptl_base_peer_t;

extern opal_class_t mca_ptl_tcp_peer_t_class;
typedef struct mca_ptl_base_peer_t mca_ptl_tcp_peer_t;

void mca_ptl_tcp_set_socket_options(int sd);
void mca_ptl_tcp_peer_close(mca_ptl_base_peer_t*);
int  mca_ptl_tcp_peer_send(mca_ptl_base_peer_t*, struct mca_ptl_tcp_send_frag_t*, int);
bool mca_ptl_tcp_peer_accept(mca_ptl_base_peer_t*, struct sockaddr_in*, int);
void mca_ptl_tcp_peer_shutdown(mca_ptl_base_peer_t*);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

