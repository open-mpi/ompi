/*
 * $HEADER$
 */
/** @file:
 *
 * Contains the data structure which describes each connection
 */

#ifndef _MCA_OOB_TCP_PEER_H_
#define _MCA_OOB_TCP_PEER_H_

#include "mca/ns/ns.h"
#include "class/ompi_list.h"
#include "class/ompi_rb_tree.h"
#include <netinet/in.h>
#include "threads/mutex.h"
#include <string.h>
#include "oob_tcp.h"
#include "oob_tcp_msg.h"

/**
 * the state of the connection
 */
typedef enum {
    MCA_OOB_TCP_CLOSED,
    MCA_OOB_TCP_CONNECTING,
    MCA_OOB_TCP_CONNECT_ACK,
    MCA_OOB_TCP_CONNECTED,
    MCA_OOB_TCP_FAILED
} mca_oob_tcp_state_t;


/**
 * This structire describes a peer
 */
struct mca_oob_tcp_peer_t {
    ompi_list_item_t super;           /**< allow this to be on a list */
    ompi_process_name_t peer_name;    /**< the name of the peer */
    mca_oob_tcp_state_t peer_state;   /**< the state of the connection */
    int peer_retries;                 /**< number of times connection attempt has failed */
    struct sockaddr_in peer_addr;     /**< the address of the peer process */
    int peer_sd;                      /**< socket descriptor of the connection */
    ompi_event_t peer_send_event;     /**< registration with event thread for send events */
    ompi_event_t peer_recv_event;     /**< registration with event thread for recv events */
    ompi_mutex_t peer_lock;           /**< make sure only one thread accesses critical data structures */
    ompi_list_t peer_send_queue;      /**< list of messages to send */
    ompi_list_t peer_recv_queue;      /**< list of pending receives */
    mca_oob_tcp_msg_t *peer_send_msg; /**< current send in progress */
    mca_oob_tcp_msg_t *peer_recv_msg; /**< current recv in progress */
};
typedef struct mca_oob_tcp_peer_t mca_oob_tcp_peer_t;

                                                                                                                 
#define MCA_OOB_TCP_PEER_ALLOC(peer, rc) \
    { \
    ompi_list_item_t* item; \
    OMPI_FREE_LIST_GET(&mca_oob_tcp_module.tcp_peer_free, item, rc); \
    peer = (mca_oob_tcp_peer_t*)item; \
    }
                                                                                                                 
#define MCA_OOB_TCP_PEER_RETURN(peer) \
    { \
    OMPI_FREE_LIST_RETURN(&mca_oob_tcp_module.tcp_peer_free, (ompi_list_item_t*)peer); \
    }
                                                                                                                 

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Lookup a peer in the cache - if it doesn't exists
 * create one and cache it.
 *
 * @param peer_name the name of the peer
 * 
 * @retval pointer to the newly created struture
 * @retval NULL if there was a problem
 */
mca_oob_tcp_peer_t *mca_oob_tcp_peer_lookup(const ompi_process_name_t* peer_name);

/**
 * Start sending a message to the specified peer. The routine
 * can return before the send completes.
 *
 * @param peer  The peer process.
 * @param msg   The message to send.
 * @retval      OMPI_SUCCESS or error code on failure.
 */

int mca_oob_tcp_peer_send(mca_oob_tcp_peer_t* peer, mca_oob_tcp_msg_t* msg);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* _MCA_OOB_TCP_PEER_H */

