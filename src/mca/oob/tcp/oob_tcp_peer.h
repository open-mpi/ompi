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
#include <netinet/in.h>
#include "threads/mutex.h"
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
 * tcp interface
 */
struct mca_oob_tcp_addr_t {
    int                ifindex; /**< oob interface index */
    struct sockaddr_in ifaddr;  /**< oob interface address */
    struct sockaddr_in ifmask;  /**< oob interface netmask */
};
typedef struct mca_oob_tcp_addr_t mca_oob_tcp_addr_t;

/**
 * This structire describes a peer
 */
struct mca_oob_tcp_peer_t {
    ompi_list_item_t super;         /**< allow this to be on a list */
    ompi_process_name_t peer_name;  /**< the name of the peer */
    mca_oob_tcp_state_t peer_state; /**< the state of the connection */
    int peer_sd;                    /**< socket descriptor of the connection */
    mca_oob_tcp_addr_t peer_addr;   /**< the address of the peer process */
    ompi_mutex_t peer_lock;         /**< make sure only one thread accesses it at a time */
    ompi_list_t peer_send;            /**< list of items to send */
    ompi_list_t peer_recieved;        /**< list of items to recieve */
};
typedef struct mca_oob_tcp_peer_t mca_oob_tcp_peer_t;
    
/**
 * The function to compare 2 peers. Used for the rb tree
 *
 * @param peer1 the first peer
 * @param peer2 the second peer
 *
 * @retval <0 if peer1 < peer2
 * @retval >0 if peer1 > peer2
 * @retval 0 if peer1 == peer2
 */
int mca_oob_tcp_peer_comp(void * key1, void * key2);

/**
 * Creates a peer structure and adds to the tree and list.
 *
 * @param peer_name the name of the peer
 * 
 * @retval pointer to the newly created struture
 * @retval NULL if there was a problem
 */
mca_oob_tcp_peer_t * mca_oob_tcp_add_peer(ompi_process_name_t peer_name);

/**
 * Deletes a peer structure from the tree and lists and frees its memory
 *
 * @param peer_name the name of the peer
 *
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERROR
 */
int mca_oob_tcp_del_peer(ompi_process_name_t peer_name);

#endif /* _MCA_OOB_TCP_PEER_H */

