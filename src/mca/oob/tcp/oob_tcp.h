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
#include "mca/base/base.h"
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


#define OMPI_NAME_COMPONENTS(n)  (n).cellid,(n).jobid,(n).vpid


/*
 * standard component functions
 */
int        mca_oob_tcp_component_open(void);
int        mca_oob_tcp_component_close(void);
mca_oob_t* mca_oob_tcp_component_init(int* priority, bool *allow_multi_user_threads, bool *have_hidden_threads);

/**
 * Hook function to allow the selected oob components
 * to register their contact info with the registry
*/

int mca_oob_tcp_init(void);

/**
 * Cleanup resources during shutdown.
 */
int mca_oob_tcp_fini(void);

/**
 * Convert process name from network to host byte order.
 *
 * @param name
 */
#define OMPI_PROCESS_NAME_NTOH(n) \
    n.cellid = ntohl(n.cellid); \
    n.jobid = ntohl(n.jobid); \
    n.vpid = ntohl(n.vpid); 

/**
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
int mca_oob_tcp_process_name_compare(const ompi_process_name_t* n1, const ompi_process_name_t* n2);
                                                                                                                      
/**
 *  Obtain contact information for this host (e.g. <ipaddress>:<port>)
 */

char* mca_oob_tcp_get_addr(void);

/**
 *  Setup cached addresses for the peers.
 */

int mca_oob_tcp_set_addr(const ompi_process_name_t*, const char*);

/**
 *  A routine to ping a given process name to determine if it is reachable.
 *
 *  @param  name  The peer name.
 *  @param  tv    The length of time to wait on a connection/response.
 *
 *  Note that this routine blocks up to the specified timeout waiting for a
 *  connection / response from the specified peer. If the peer is unavailable
 *  an error status is returned.
 */
                                                                                                       
int mca_oob_tcp_ping(const ompi_process_name_t* name, const struct timeval* tv);

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
    ompi_process_name_t* peer, 
    struct iovec *msg, 
    int count, 
    int tag,
    int flags);

/**
 * Similiar to unix readv(2)
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param tag (IN)     User defined tag for matching send/recv.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to the number of bytes provided in the
 *                     iovec array without removing the message from the queue.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_tcp_recv(
    ompi_process_name_t* peer, 
    struct iovec * msg, 
    int count, 
    int* tag,
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
    ompi_process_name_t* peer, 
    struct iovec* msg, 
    int count,
    int tag,
    int flags, 
    mca_oob_callback_fn_t cbfunc, 
    void* cbdata);

/**
 * Non-blocking version of mca_oob_recv().
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
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
    struct iovec* msg, 
    int count, 
    int tag,
    int flags,
    mca_oob_callback_fn_t cbfunc, 
    void* cbdata);

/**
 * Attempt to map a peer name to its corresponding address.
 */

int mca_oob_tcp_resolve(mca_oob_tcp_peer_t*);

/**
 *  Parse a URI string into an IP address and port number.
 */
int mca_oob_tcp_parse_uri(
    const char* uri, 
    struct sockaddr_in* inaddr
);


/**
 *  OOB TCP Component
*/
struct mca_oob_tcp_component_t {
    mca_oob_base_component_1_0_0_t super;  /**< base OOB component */
    int                tcp_listen_sd;        /**< listen socket for incoming connection requests */
    unsigned short     tcp_listen_port;      /**< listen port */
    ompi_list_t        tcp_subscriptions;    /**< list of registry subscriptions */
    ompi_list_t        tcp_peer_list;        /**< list of peers sorted in mru order */
    ompi_rb_tree_t     tcp_peer_tree;        /**< tree of peers sorted by name */
    ompi_rb_tree_t     tcp_peer_names;       /**< cache of peer contact info sorted by name */
    ompi_free_list_t   tcp_peer_free;        /**< free list of peers */
    int                tcp_peer_limit;       /**< max size of tcp peer cache */
    int                tcp_peer_retries;     /**< max number of retries before declaring peer gone */
    ompi_free_list_t   tcp_msgs;             /**< free list of messages */
    ompi_event_t       tcp_send_event;       /**< event structure for sends */
    ompi_event_t       tcp_recv_event;       /**< event structure for recvs */
    ompi_mutex_t       tcp_lock;             /**< lock for accessing module state */
    ompi_list_t        tcp_msg_post;         /**< list of recieves user has posted */
    ompi_list_t        tcp_msg_recv;         /**< list of recieved messages */
    ompi_mutex_t       tcp_match_lock;       /**< lock held while searching/posting messages */
    int                tcp_debug;            /**< debug level */
};

/**
 * Convenience Typedef
 */
typedef struct mca_oob_tcp_component_t mca_oob_tcp_component_t;

extern mca_oob_tcp_component_t mca_oob_tcp_component;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_OOB_TCP_H_ */

