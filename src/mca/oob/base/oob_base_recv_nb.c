/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "include/constants.h"

#include "dps/dps.h"
#include "mca/ns/ns_types.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include <string.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

/*
 * Internal type to handle non-blocking packed receive.
 */

struct mca_oob_recv_cbdata {
    struct iovec cbiov;
    mca_oob_callback_packed_fn_t cbfunc;
    void* cbdata;
};
typedef struct mca_oob_recv_cbdata mca_oob_recv_cbdata_t;


static void mca_oob_recv_callback(
    int status,
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    int tag,
    void* cbdata);

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
int mca_oob_recv_nb(orte_process_name_t* peer, struct iovec* msg, int count, int tag, int flags,
                    mca_oob_callback_fn_t cbfunc, void* cbdata)
{
    return(mca_oob.oob_recv_nb(peer, msg, count, tag, flags, cbfunc, cbdata));
}


/*
 * Cancel non-blocking recv.j
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
 * @param tag (IN)     User defined tag for message matching.
 * @return             OMPI success or error code (<0) on error.
 */
int mca_oob_recv_cancel(orte_process_name_t* peer, int tag)
{
    return(mca_oob.oob_recv_cancel(peer, tag));
}


/**
* Non-blocking version of mca_oob_recv_packed().
*
* @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
* @param buffer (IN)  Array of iovecs describing user buffers and lengths.
* @param count (IN)   Number of elements in iovec array.
* @param tag (IN)     User defined tag for matching send/recv.
* @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
* @param cbfunc (IN)  Callback function on recv completion.
* @param cbdata (IN)  User data that is passed to callback function.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*
* The user supplied callback function is called asynchronously when a message is received
* that matches the call parameters.
*/

int mca_oob_recv_packed_nb(
    orte_process_name_t* peer,
    int tag,
    int flags,
    mca_oob_callback_packed_fn_t cbfunc,
    void* cbdata)
{
    mca_oob_recv_cbdata_t *oob_cbdata = malloc(sizeof(mca_oob_recv_cbdata_t));
    int rc;

    if(NULL == oob_cbdata)
        return OMPI_ERR_OUT_OF_RESOURCE;

    memset(oob_cbdata, 0, sizeof(mca_oob_recv_cbdata_t));
    oob_cbdata->cbfunc = cbfunc;
    oob_cbdata->cbdata = cbdata;
    
    rc = mca_oob.oob_recv_nb(peer, &oob_cbdata->cbiov, 1, tag, flags|MCA_OOB_ALLOC, mca_oob_recv_callback, oob_cbdata);
    if(rc != OMPI_SUCCESS) {
        free(oob_cbdata);
    }
    return rc;
}

/**
*  Callback function on non-blocking recv completion.
*
*  @param status (IN)  Completion status - equivalent to the return value from blocking send/recv.
*  @param peer (IN)    Opaque name of peer process.
*  @param msg (IN)     Array of iovecs describing user buffers and lengths.
*  @param count (IN)   Number of elements in iovec array.
*  @param tag (IN)     User defined tag for matching send/recv.
*  @param cbdata (IN)  User data.
*/

static void mca_oob_recv_callback(
    int status,
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    int tag,
    void* cbdata)
{
    mca_oob_recv_cbdata_t *oob_cbdata = cbdata;
    orte_buffer_t buffer;

    /* validate status */
    if(status < 0) {
        oob_cbdata->cbfunc(status, peer, NULL, tag, oob_cbdata->cbdata);
        free(oob_cbdata);
        return;
    }

    /* init a buffer with the received message */
    OBJ_CONSTRUCT(&buffer, orte_buffer_t);
    orte_dps.load(&buffer,msg[0].iov_base,msg[0].iov_len);

    /* call users callback function */
    oob_cbdata->cbfunc(status, peer, &buffer, tag, oob_cbdata->cbdata);

    /* cleanup */
    OBJ_DESTRUCT(&buffer);
    free(oob_cbdata);
}

