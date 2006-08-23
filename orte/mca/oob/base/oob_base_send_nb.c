/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/orte_constants.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/dss/dss.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include <string.h>

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif


/*
 * internal struct for non-blocking packed sends
 */

struct mca_oob_send_cbdata {
    orte_buffer_t* cbbuf;
    struct iovec cbiov;
    mca_oob_callback_packed_fn_t cbfunc;
    void* cbdata;
};
typedef struct mca_oob_send_cbdata mca_oob_send_cbdata_t;

static void mca_oob_send_callback(
    int status,
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    int tag,
    void* cbdata);

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

int mca_oob_send_nb(orte_process_name_t* peer, struct iovec* msg, int count, int tag,
                    int flags, mca_oob_callback_fn_t cbfunc, void* cbdata)
{
    return(mca_oob.oob_send_nb(peer, msg, count, tag, flags, cbfunc, cbdata));
}


/**
*  Non-blocking version of mca_oob_send_packed().
*
*  @param peer (IN)    Opaque name of peer process.
*  @param buffer (IN)  Opaque buffer handle.
*  @param tag (IN)     User defined tag for matching send/recv.
*  @param flags (IN)   Currently unused.
*  @param cbfunc (IN)  Callback function on send completion.
*  @param cbdata (IN)  User data that is passed to callback function.
*  @return             OMPI error code (<0) on error number of bytes actually sent.
*
*  The user supplied callback function is called when the send completes. Note that
*  the callback may occur before the call to mca_oob_send returns to the caller,
*  if the send completes during the call.
*
*/

int mca_oob_send_packed_nb(
    orte_process_name_t* peer,
    orte_buffer_t* buffer,
    int tag,
    int flags,
    mca_oob_callback_packed_fn_t cbfunc,
    void* cbdata)
{
    mca_oob_send_cbdata_t *oob_cbdata;
    void *dataptr;
    orte_std_cntr_t datalen;
    int rc;

    /* first build iovec from buffer information */
    rc = orte_dss.unload(buffer, &dataptr, &datalen);
    if (rc != ORTE_SUCCESS) {
        return rc;
    }
    orte_dss.load(buffer, dataptr, datalen);

    /* allocate a struct to pass into callback */
    if(NULL == (oob_cbdata = (mca_oob_send_cbdata_t*)malloc(sizeof(mca_oob_send_cbdata_t)))) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    oob_cbdata->cbbuf = buffer;
    oob_cbdata->cbfunc = cbfunc;
    oob_cbdata->cbdata = cbdata;
    oob_cbdata->cbiov.iov_base = (IOVBASE_TYPE*)dataptr;
    oob_cbdata->cbiov.iov_len = datalen;

    /* queue up the request */
    rc = mca_oob.oob_send_nb(peer, &oob_cbdata->cbiov, 1, tag, flags, mca_oob_send_callback, oob_cbdata);
    if(rc < 0) {
        free(oob_cbdata);
    }
    return rc;
}

/**
*  Callback function on send completion for buffer PACKED message only.
*  i.e. only mca_oob_send_packed_nb and mca_oob_recv_packed_nb USE this.
*
*  @param status (IN)  Completion status - equivalent to the return value from blocking send/recv.
*  @param peer (IN)    Opaque name of peer process.
*  @param buffer (IN)  For sends, this is a pointer to a prepacked buffer
                       For recvs, OOB creates and returns a buffer
*  @param tag (IN)     User defined tag for matching send/recv.
*  @param cbdata (IN)  User data.
*/

static void mca_oob_send_callback(
    int status,
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    int tag,
    void* cbdata)
{
    /* validate status */
    mca_oob_send_cbdata_t *oob_cbdata = (mca_oob_send_cbdata_t*)cbdata;
    if(status < 0) {
        oob_cbdata->cbfunc(status, peer, NULL, tag, oob_cbdata->cbdata);
        free(oob_cbdata);
        return;
    }

    oob_cbdata->cbfunc(status, peer, oob_cbdata->cbbuf, tag, oob_cbdata->cbdata);
    free(oob_cbdata);
}

