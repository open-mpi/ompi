#include "mca/oob/tcp/oob_tcp.h"

/*
 * Similiar to unix send(2).
 *
 * @param peer (IN)   Opaque name of peer process.
 * @param msg (IN)    Array of iovecs describing user buffers and lengths.
 * @param count (IN)  Number of elements in iovec array.
 * @param flags (IN)  Currently unused.
 * @return            OMPI error code (<0) on error number of bytes actually sent.
 */

int mca_oob_tcp_send(const ompi_process_name_t* peer, const struct iovec *msg, int count, int flags)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}

/*
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
                                    int flags, mca_oob_callback_fn_t cbfunc, void* cbdata)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}

