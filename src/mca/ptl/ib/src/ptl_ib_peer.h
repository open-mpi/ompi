#ifndef MCA_PTL_IB_PEER_H
#define MCA_PTL_IB_PEER_H

#include "class/ompi_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "ptl_ib_recvfrag.h"
#include "ptl_ib_sendfrag.h"

/**
 * State of IB peer connection.
 */

typedef enum {
    MCA_PTL_IB_CLOSED,
    MCA_PTL_IB_CONNECTING,
    MCA_PTL_IB_CONNECT_ACK,
    MCA_PTL_IB_CONNECTED,
    MCA_PTL_IB_FAILED
} mca_ptl_ib_state_t;

/**
 * An abstraction that represents a connection to a peer process.
 * An instance of mca_ptl_base_peer_t is associated w/ each process
 * and PTL pair at startup. However, connections to the peer
 * are established dynamically on an as-needed basis:
 */

struct mca_ptl_base_peer_t {
    ompi_list_item_t          super;
    struct mca_ptl_ib_t*      peer_ptl;         /**< PTL instance that created this connection */
    struct mca_ptl_ib_proc_t* peer_proc;        /**< proc structure corresponding to peer */
    struct mca_ptl_ib_addr_t* peer_addr;        /**< address of peer */
    int                       peer_sd;          /**< socket connection to peer */
    mca_ptl_ib_send_frag_t*   peer_send_frag;   /**< current send frag being processed */
    mca_ptl_ib_recv_frag_t*   peer_recv_frag;   /**< current recv frag being processed */
    mca_ptl_ib_state_t        peer_state;       /**< current state of the connection */
    size_t                    peer_retries;     /**< number of connection retries attempted */
    ompi_list_t               peer_frags;       /**< list of pending frags to send */
    ompi_mutex_t              peer_send_lock;   /**< lock for concurrent access to peer state */
    ompi_mutex_t              peer_recv_lock;   /**< lock for concurrent access to peer state */
    ompi_event_t              peer_send_event;  /**< event for async processing of send frags */
    ompi_event_t              peer_recv_event;  /**< event for async processing of recv frags */
};
typedef struct mca_ptl_base_peer_t mca_ptl_base_peer_t;
typedef struct mca_ptl_base_peer_t mca_ptl_ib_peer_t;

/*
extern ompi_class_t mca_ptl_ib_peer_t_class;
*/

OBJ_CLASS_DECLARATION(mca_ptl_ib_peer_t);


#endif
