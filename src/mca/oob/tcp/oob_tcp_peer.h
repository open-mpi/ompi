/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * Contains the data structure which describes each connection
 */

#ifndef _MCA_OOB_TCP_PEER_H_
#define _MCA_OOB_TCP_PEER_H_

#include "ompi_config.h"
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#include <string.h>

#include "class/ompi_list.h"
#include "threads/mutex.h"
#include "mca/ns/ns_types.h"
#include "oob_tcp_msg.h"
#include "oob_tcp_addr.h"

/**
 * the state of the connection
 */
typedef enum {
    MCA_OOB_TCP_CLOSED,
    MCA_OOB_TCP_RESOLVE,
    MCA_OOB_TCP_CONNECTING,
    MCA_OOB_TCP_CONNECT_ACK,
    MCA_OOB_TCP_CONNECTED,
    MCA_OOB_TCP_FAILED
} mca_oob_tcp_state_t;


/**
 * This structure describes a peer
 */
struct mca_oob_tcp_peer_t {
    ompi_list_item_t super;           /**< allow this to be on a list */
    orte_process_name_t peer_name;    /**< the name of the peer */
    mca_oob_tcp_state_t peer_state;   /**< the state of the connection */
    int peer_retries;                 /**< number of times connection attempt has failed */
    mca_oob_tcp_addr_t* peer_addr;    /**< the addresses of the peer process */
    int peer_sd;                      /**< socket descriptor of the connection */
    ompi_event_t peer_send_event;     /**< registration with event thread for send events */
    ompi_event_t peer_recv_event;     /**< registration with event thread for recv events */
    ompi_event_t peer_timer_event;    /**< timer for retrying connection failures */
    ompi_mutex_t peer_lock;           /**< protect critical data structures */
    ompi_list_t peer_send_queue;      /**< list of messages to send */
    mca_oob_tcp_msg_t *peer_send_msg; /**< current send in progress */
    mca_oob_tcp_msg_t *peer_recv_msg; /**< current recv in progress */
};
/**
 * Convenience Typedef
 */
typedef struct mca_oob_tcp_peer_t mca_oob_tcp_peer_t;

/*
 * Class declaration.
 */

OBJ_CLASS_DECLARATION(mca_oob_tcp_peer_t);

/**
 * Get a new peer data structure
 */ 
#define MCA_OOB_TCP_PEER_ALLOC(peer, rc) \
    { \
    ompi_list_item_t* item; \
    OMPI_FREE_LIST_GET(&mca_oob_tcp_component.tcp_peer_free, item, rc); \
    peer = (mca_oob_tcp_peer_t*)item; \
    }

/**
 * Return a peer data structure
 */
#define MCA_OOB_TCP_PEER_RETURN(peer) \
    { \
    mca_oob_tcp_peer_shutdown(peer); \
    ompi_hash_table_remove_proc(&mca_oob_tcp_component.tcp_peers, &peer->peer_name); \
    OMPI_FREE_LIST_RETURN(&mca_oob_tcp_component.tcp_peer_free, (ompi_list_item_t*)peer); \
    }

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Lookup a peer in the cache - if it doesn't exists
 * create one and cache it.
 *
 * @param peer_name the name of the peer
 * @retval pointer to the peer's (possibly newly created) struture
 * @retval NULL if there was a problem
 */
mca_oob_tcp_peer_t *mca_oob_tcp_peer_lookup(const orte_process_name_t* peer_name);

/**
 * Start sending a message to the specified peer. The routine
 * can return before the send completes.
 *
 * @param peer  The peer process.
 * @param msg   The message to send.
 * @retval      OMPI_SUCCESS or error code on failure.
 */
int mca_oob_tcp_peer_send(mca_oob_tcp_peer_t* peer, mca_oob_tcp_msg_t* msg);

/**
 * Connection request for this peer. Check the status of our connection
 * before accepting the peers.
 * 
 * @param peer  The peer process.
 * @param sd    Incoming connection request.
 */
bool mca_oob_tcp_peer_accept(mca_oob_tcp_peer_t* peer, int sd);

/**
 * Cleanup/close the connection to the peer.
 *
 * @param peer  The peer process.
 */
void mca_oob_tcp_peer_close(mca_oob_tcp_peer_t* peer);
void mca_oob_tcp_peer_shutdown(mca_oob_tcp_peer_t* peer);

/**
 * The peers address has been resolved.
 */
void mca_oob_tcp_peer_resolved(mca_oob_tcp_peer_t* peer, mca_oob_tcp_addr_t* addr);

/*
 * Send the process identifier to the peer - so that 
 * temporary names can be updated to actuals.
 */
int mca_oob_tcp_peer_send_ident(mca_oob_tcp_peer_t* peer);

/*
 * Remove any references to the message from the peers send/recv queue.
 */
void mca_oob_tcp_peer_dequeue_msg(mca_oob_tcp_peer_t* peer, mca_oob_tcp_msg_t* msg);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* _MCA_OOB_TCP_PEER_H */

