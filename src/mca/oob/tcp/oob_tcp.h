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
struct mca_oob_base_module_1_0_0_t* mca_oob_tcp_init(bool *allow_multi_user_threads,
                                                     bool *have_hidden_threads);
int mca_oob_tcp_finalize(void);


/**
 *  Similiar to unix writev(2).
 *
 * @param peer (IN)   Opaque name of peer process.
 * @param msg (IN)    Array of iovecs describing user buffers and lengths.
 * @param count (IN)  Number of elements in iovec array.
 * @param flags (IN)  Currently unused.
 * @return            OMPI error code (<0) on error number of bytes actually sent.
 */

int mca_oob_tcp_send(const ompi_process_name_t* peer, const struct iovec *msg, int count, int flags);

/**
 * Similiar to unix readv(2)
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to the number of bytes provided in the
 *                     iovec array without removing the message from the queue.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_tcp_recv(ompi_process_name_t* peer, const struct iovec *iov, int count, int flags);


/*
 * Non-blocking versions of send/recv.
 */

/**
 * Non-blocking version of mca_oob_send().
 *
 * @param peer (IN)    Opaque name of peer process.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   Currently unused.
 * @param cbfunc (IN)  Callback function on send completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error number of bytes actually sent.
 *
 */

int mca_oob_tcp_send_nb(const ompi_process_name_t* peer, const struct iovec* iov, int count,
                    int flags, mca_oob_callback_fn_t cbfunc, void* cbdata);

/**
 * Non-blocking version of mca_oob_recv().
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
 * @param cbfunc (IN)  Callback function on recv completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_tcp_recv_nb(ompi_process_name_t* peer, const struct iovec* iov, int count, int flags,
                    mca_oob_callback_fn_t cbfunc, void* cbdata);


/**
 *  OOB TCP Component
*/
struct mca_oob_tcp_component_t {
    mca_oob_base_component_1_0_0_t super;  /**< base OOB component */
    int tcp_listen_sd;                     /**< listen socket for incoming connection requests */
    unsigned short   tcp_listen_port;      /**< listen port */
    ompi_list_t      tcp_peer_list;        /**< list of peers sorted in mru order */
    ompi_rb_tree_t   tcp_peer_tree;        /**< tree of peers sorted by name */
    ompi_free_list_t tcp_peer_free;        /**< free list of peers */
    ompi_free_list_t tcp_msgs;             /**< free list of messages */
    ompi_event_t     tcp_send_event;       /**< event structure for sends */
    ompi_event_t     tcp_recv_event;       /**< event structure for recvs */
    ompi_mutex_t     tcp_lock;             /**< lock for accessing module state */
    ompi_condition_t tcp_condition;        /**< condition variable for blocking sends */
    size_t           tcp_cache_size;       /**< max size of tcp peer cache */
    ompi_list_t      tcp_post_recv;        /**< list of the recvs the user has posted */
    ompi_list_t      tcp_msg_recv;         /**< list of recieved messages */
};
typedef struct mca_oob_tcp_component_t mca_oob_tcp_component_t;

extern mca_oob_tcp_component_t mca_oob_tcp_component;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_OOB_TCP_H_ */

