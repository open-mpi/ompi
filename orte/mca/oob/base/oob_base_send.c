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
#include <string.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/orte_constants.h"

#include "orte/dss/dss.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include "opal/util/output.h"
/*
*  Similiar to unix send(2).
*
* @param peer (IN)   Opaque name of peer process.
* @param msg (IN)    Array of iovecs describing user buffers and lengths.
* @param count (IN)  Number of elements in iovec array.
* @param flags (IN)  Currently unused.
* @return            OMPI error code (<0) on error or number of bytes actually sent.
*/

int mca_oob_send(orte_process_name_t* peer, struct iovec *msg, int count, int tag, int flags)
{
    return(mca_oob.oob_send(peer, msg, count, tag, flags));
}

/*
*  Similiar to unix send(2) and mca_oob_send.
*
* @param peer (IN)   Opaque name of peer process.
* @param buffer (IN) Prepacked OMPI_BUFFER containing data to send
* @param flags (IN)  Currently unused.
* @return            OMPI error code (<0) on error or number of bytes actually sent.
*/

int mca_oob_send_packed (orte_process_name_t* peer, orte_buffer_t* buffer, int tag, int flags)
{
    void *dataptr;
    orte_std_cntr_t datalen;
    struct iovec msg[1];
    int rc;

    /* first build iovec from buffer information */
    rc = orte_dss.unload(buffer, &dataptr, &datalen);
    if(rc != ORTE_SUCCESS) {
        return rc;
    }
    orte_dss.load(buffer, dataptr, datalen);

    msg[0].iov_base = (IOVBASE_TYPE*)dataptr;
    msg[0].iov_len  = datalen;

    return(mca_oob.oob_send(peer, msg, 1, tag, flags));
}

