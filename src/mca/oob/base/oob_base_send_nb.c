#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include <string.h>
#include <netinet/in.h>
    

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

int mca_oob_send_nb(ompi_process_name_t* peer, struct iovec* msg, int count, int tag,
                    int flags, mca_oob_callback_fn_t cbfunc, void* cbdata)
{
    return(mca_oob.oob_send_nb(peer, msg, count, tag, flags, cbfunc, cbdata));
}

