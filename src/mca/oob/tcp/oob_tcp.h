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
#include "class/ompi_rb_tree.h"
#include "mca/oob/tcp/oob_tcp_peer.h"
#include "mca/oob/tcp/oob_tcp_message.h"

/*
 * the list of peers
 */
ompi_list_t mca_oob_tcp_peer_list;
/*
 * the tree of peers
 */
ompi_rb_tree_t mca_oob_tcp_peer_tree;


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * standard module functions
 */
int mca_oob_tcp_open(void);
int mca_oob_tcp_close(void);
struct mca_oob_1_0_0_t* mca_oob_tcp_init(bool *allow_multi_user_threads,
                                         bool *have_hidden_threads);
int mca_oob_tcp_finalize(void);


/**
 *  Similiar to unix send(2).
 *
 * @param peer (IN)   Opaque name of peer process.
 * @param msg (IN)    Array of iovecs describing user buffers and lengths.
 * @param count (IN)  Number of elements in iovec array.
 * @param flags (IN)  Currently unused.
 * @return            OMPI error code (<0) on error number of bytes actually sent.
 */

int mca_oob_tcp_send(const ompi_process_name_t* peer, const struct iovec *msg, int count, int flags);

/**
 * Similiar to unix recv(2)
 *
 * @param peer (IN)    Opaque name of peer process or OOB_NAME_ANY for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   May be OOB_PEEK to return up to the number of bytes provided in the
 *                     iovec array without removing the message from the queue.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_tcp_recv(ompi_process_name_t* peer, const struct iovec *msg, int count, int flags);


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

int mca_oob_tcp_send_nb(const ompi_process_name_t* peer, const struct iovec* msg, int count,
                    int flags, mca_oob_callback_fn_t cbfunc, void* cbdata);

/**
 * Non-blocking version of mca_oob_recv().
 *
 * @param peer (IN)    Opaque name of peer process or OOB_NAME_ANY for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   May be OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
 * @param cbfunc (IN)  Callback function on recv completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_tcp_recv_nb(ompi_process_name_t* peer, const struct iovec* msg, int count, int flags,
                    mca_oob_callback_fn_t cbfunc, void* cbdata);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_OOB_TCP_H_ */

