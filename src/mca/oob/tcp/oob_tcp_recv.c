#include "mca/oob/tcp/oob_tcp.h"

/*
 * Similiar to unix recv(2)
 *
 * @param peer (IN)    Opaque name of peer process or OOB_NAME_ANY for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   May be OOB_PEEK to return up to the number of bytes provided in the
 *                     iovec array without removing the message from the queue.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_tcp_recv(ompi_process_name_t* peer, const struct iovec *msg, int count, int flags)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}

/*
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
                                     mca_oob_callback_fn_t cbfunc, void* cbdata)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}



