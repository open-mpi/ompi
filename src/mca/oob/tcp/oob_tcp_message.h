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

/**
 * the describes each message
 */
struct mca_oob_tcp_message_t {
    ompi_list_item_t super;  /**< make it so we can put this on a list */
    size_t message_state;    /**< the amount sent or recieved */
    struct iovec * message_data; /**< the data of the message */
    struct iovec * message_tmp;  /**< in case we have to make a copy of the iovecs */
    int message_count;           /**< the number of items in the iovect array */
    mca_oob_callback_fn_t cbfunc;/**< the callback function for the send/recieve */    
    void * cbdata;               /**< the data for the callback fnuction */
};


#endif /* _MCA_OOB_TCP_MESSAGE_H_ */
