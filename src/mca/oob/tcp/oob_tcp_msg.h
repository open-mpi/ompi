/*
 * $HEADER$
 */
/** @file:
 * 
 * contains the data structure we will use to describe a message
 */


#ifndef _MCA_OOB_TCP_MESSAGE_H_
#define _MCA_OOB_TCP_MESSAGE_H_

#include "class/ompi_list.h"
#include "mca/oob/tcp/oob_tcp_peer.h"
#include "mca/oob/oob.h"

struct mca_oob_tcp_peer_t;


/**
 * describes each message being progressed.
 */
struct mca_oob_tcp_msg_t {
    ompi_list_item_t super;           /**< make it so we can put this on a list */
    size_t msg_state;                 /**< the amount sent or recieved */
    const struct iovec * msg_user;    /**< the data of the message */
    struct iovec * msg_iov;           /**< copy of iovec array - not data */
    struct iovec * msg_rwptr;         /**< current read/write pointer into msg_iov */
    int msg_rwcnt;                    /**< number of iovecs left for read/write */
    int msg_count;                    /**< the number of items in the iovec array */
    mca_oob_callback_fn_t msg_cbfunc; /**< the callback function for the send/recieve */    
    void *msg_cbdata;                 /**< the data for the callback fnuction */
    bool  msg_complete;
};
typedef struct mca_oob_tcp_msg_t mca_oob_tcp_msg_t;

OBJ_CLASS_DECLARATION(mca_oob_tcp_msg_t);

/**
 * Get a new structure for use with a message
 */
#define MCA_OOB_TCP_MSG_ALLOC(msg, rc) \
    { \
    ompi_list_item_t* item; \
    OMPI_FREE_LIST_GET(&mca_oob_tcp_module.tcp_msgs, item, rc); \
    msg = (mca_oob_tcp_msg_t*)item; \
    }

/**
 * return a message structure that is no longer needed
 */
#define MCA_OOB_TCP_MSG_RETURN(msg) \
    { \
    OMPI_FREE_LIST_RETURN(&mca_oob_tcp_module.tcp_msgs, (ompi_list_item_t*)msg); \
    }

/**
 *  Wait for a msg to complete.
 *  @param  msg (IN)   Message to wait on.
 *  @param  size (OUT) Number of bytes delivered.
 *  @retval OMPI_SUCCESS or error code on failure.
 */
int mca_oob_tcp_msg_wait(mca_oob_tcp_msg_t* msg, int* size);

/**
 *  Signal that a message has completed. Wakes up any pending threads (for blocking send)
 *  or invokes callbacks for non-blocking case.
 *  @param  msg (IN)   Message send/recv that has completed.
 *  @retval OMPI_SUCCESS or error code on failure.
 */
int mca_oob_tcp_msg_complete(mca_oob_tcp_msg_t* msg);

/**
 *  Called asynchronously to progress sending a message from the event library thread.
 *  @param  msg (IN)   Message send that is in progress. 
 *  @param  sd (IN)    Socket descriptor to use for send.
 *  @retval bool       Bool flag indicating wether operation has completed.
 */
bool mca_oob_tcp_msg_send_handler(mca_oob_tcp_msg_t* msg, int sd);

/**
 *  Called asynchronously to progress sending a message from the event library thread.
 *  @param  msg (IN)   Message send that is in progress. 
 *  @param  sd (IN)    Socket descriptor to use for send.
 *  @retval bool       Bool flag indicating wether operation has completed.
 */

bool mca_oob_tcp_msg_recv_handler(mca_oob_tcp_msg_t* msg, int sd);

/**
 *  Initialize a message for send/recv.
 *  @param  msg (IN)   Message send that is in progress. 
 *  @param  peer (IN)  Peer to send/recv message to/from.
 */

void mca_oob_tcp_msg_init(mca_oob_tcp_msg_t* msg, struct mca_oob_tcp_peer_t*);

#endif /* _MCA_OOB_TCP_MESSAGE_H_ */

