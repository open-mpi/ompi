#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include <string.h>
#include <netinet/in.h>

/*
 * Non-blocking version of mca_oob_recv_nb().
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
 * @param cbfunc (IN)  Callback function on recv completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */
int mca_oob_recv_nb(ompi_process_name_t* peer, struct iovec* msg, int count, int* tag, int flags,
                    mca_oob_callback_fn_t cbfunc, void* cbdata)
{
    return(mca_oob.oob_recv_nb(peer, msg, count, tag, flags, cbfunc, cbdata));
}

