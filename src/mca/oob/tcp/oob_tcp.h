/*
 * $HEADER$
 */
/** @file:
 *
 * Defines the functions for the tcp module.
 */

#ifndef _MCA_OOB_TCP_H_
#define _MCA_OOB_TCP_H_

#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "class/ompi_free_list.h"
#include "class/ompi_rb_tree.h"
#include "event/event.h"
#include "threads/mutex.h"
#include "threads/condition.h"
#include "mca/oob/tcp/oob_tcp_peer.h"
#include "mca/oob/tcp/oob_tcp_msg.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * standard module functions
 */
int mca_oob_tcp_open(void);
int mca_oob_tcp_close(void);
mca_oob_t* mca_oob_tcp_init(bool *allow_multi_user_threads, bool *have_hidden_threads);
int mca_oob_tcp_finalize(void);

/*
 * Convert process name from network to host byte order.
 *
 * @param name
 */

#define OMPI_PROCESS_NAME_NTOH(n) \
    n.cellid = ntohl(n.cellid); \
    n.jobid = ntohl(n.jobid); \
    n.vpid = ntohl(n.vpid); 

/*
 * Convert process name from host to network byte order.
 *
 * @param name
 */

#define OMPI_PROCESS_NAME_HTON(n) \
    n.cellid = htonl(n.cellid); \
    n.jobid = htonl(n.jobid); \
    n.vpid = htonl(n.vpid);  


/**
* Compare two process names for equality.
*
* @param  n1  Process name 1.
* @param  n2  Process name 2.
* @return     (-1 for n1<n2 0 for equality, 1 for n1>n2)
*
* Note that the definition of < or > is somewhat arbitrary -
* just needs to be consistently applied to maintain an ordering
* when process names are used as indices.
*/
                                                                                                                      
static int ompi_process_name_compare(const ompi_process_name_t* n1, const ompi_process_name_t* n2)
{
   if(n1->cellid < n2->cellid)
       return -1;
   else if(n1->cellid > n2->cellid)
       return 1;
   else if(n1->jobid < n2->jobid)
       return -1;
   else if(n1->jobid > n2->jobid)
       return 1;
   else if(n1->vpid < n2->vpid)
       return -1;
   else if(n1->vpid > n2->vpid)
       return 1;
   return(0);
}
                                                                                                                      
/**
 *  Similiar to unix writev(2).
 *
 * @param peer (IN)   Opaque name of peer process.
 * @param msg (IN)    Array of iovecs describing user buffers and lengths.
 * @param count (IN)  Number of elements in iovec array.
 * @param tag (IN)    User defined tag for matching send/recv.
 * @param flags (IN)  Currently unused.
 * @return            OMPI error code (<0) on error number of bytes actually sent.
 */

int mca_oob_tcp_send(
    const ompi_process_name_t* peer, 
    const struct iovec *msg, 
    int count, 
    int tag,
    int flags);

/**
 * Similiar to unix readv(2)
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param tag (IN)     User defined tag for matching send/recv.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to the number of bytes provided in the
 *                     iovec array without removing the message from the queue.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_tcp_recv(
    ompi_process_name_t* peer, 
    const struct iovec *iov, 
    int count, 
    int tag,
    int flags);


/*
 * Non-blocking versions of send/recv.
 */

/**
 * Non-blocking version of mca_oob_send().
 *
 * @param peer (IN)    Opaque name of peer process.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param tag (IN)     User defined tag for matching send/recv.
 * @param flags (IN)   Currently unused.
 * @param cbfunc (IN)  Callback function on send completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error number of bytes actually sent.
 *
 */

int mca_oob_tcp_send_nb(
    const ompi_process_name_t* peer, 
    const struct iovec* iov, 
    int count,
    int tag,
    int flags, 
    mca_oob_callback_fn_t cbfunc, 
    void* cbdata);

/**
 * Non-blocking version of mca_oob_recv().
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param tag (IN)     User defined tag for matching send/recv.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
 * @param cbfunc (IN)  Callback function on recv completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_tcp_recv_nb(
    ompi_process_name_t* peer, 
    const struct iovec* iov, 
    int count, 
    int tag,
    int flags,
    mca_oob_callback_fn_t cbfunc, 
    void* cbdata);


/**
 *  OOB TCP Component
*/
struct mca_oob_tcp_component_t {
    mca_oob_base_component_1_0_0_t super;  /**< base OOB component */
    int              tcp_listen_sd;        /**< listen socket for incoming connection requests */
    unsigned short   tcp_listen_port;      /**< listen port */
    ompi_list_t      tcp_peer_list;        /**< list of peers sorted in mru order */
    ompi_rb_tree_t   tcp_peer_tree;        /**< tree of peers sorted by name */
    ompi_free_list_t tcp_peer_free;        /**< free list of peers */
    size_t           tcp_peer_limit;       /**< max size of tcp peer cache */
    int              tcp_peer_retries;     /**< max number of retries before declaring peer gone */
    ompi_free_list_t tcp_msgs;             /**< free list of messages */
    ompi_event_t     tcp_send_event;       /**< event structure for sends */
    ompi_event_t     tcp_recv_event;       /**< event structure for recvs */
    ompi_mutex_t     tcp_lock;             /**< lock for accessing module state */
    ompi_list_t      tcp_msg_post;         /**< list of recieves user has posted */
    ompi_list_t      tcp_msg_recv;         /**< list of recieved messages */
    ompi_mutex_t     tcp_match_lock;       /**< lock held while searching/posting messages */
};
typedef struct mca_oob_tcp_component_t mca_oob_tcp_component_t;

extern mca_oob_tcp_component_t mca_oob_tcp_component;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_OOB_TCP_H_ */

