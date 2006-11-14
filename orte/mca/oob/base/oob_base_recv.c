/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#include <string.h>


/*
* Similiar to unix recv(2)
*
* @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
* @param msg (IN)     Array of iovecs describing user buffers and lengths.
* @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
* @param count (IN)   Number of elements in iovec array.
* @param tag (IN)     User defined tag for matching send/recv.
* @param flags (IN)   May be MCA_OOB_PEEK to return up to the number of bytes provided in the
*                     iovec array without removing the message from the queue.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/
int mca_oob_recv(orte_process_name_t* peer, struct iovec *msg, int count, int tag, int flags)
{
    return(mca_oob.oob_recv(peer, msg, count, tag, flags));
}

/*
* Similiar to unix recv(2)
*
* @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
* @param buffer (OUT) Buffer that the OOB creates to recv this message...
* @param tag (IN)     User defined tag for matching send/recv.
*                     iovec array without removing the message from the queue.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/
int mca_oob_recv_packed(orte_process_name_t* peer, orte_buffer_t *buf, int tag)
{
    int rc;
    struct iovec msg[1];

    /* setup iov */
    msg[0].iov_base = NULL;
    msg[0].iov_len  = 0;

    rc = mca_oob.oob_recv(peer, msg, 1, tag, MCA_OOB_ALLOC);
    if(rc < 0)
        return rc;

    /* initialize buffer */
    return orte_dss.load(buf, msg[0].iov_base, msg[0].iov_len);
}

