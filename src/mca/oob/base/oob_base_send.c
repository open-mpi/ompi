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
#include <string.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "include/constants.h"

#include "dps/dps.h"
#include "mca/ns/ns_types.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "util/output.h"
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
    size_t datalen;
    struct iovec msg[1];
    int rc;

    /* first build iovec from buffer information */
    rc = orte_dps.unload(buffer, &dataptr, &datalen);
    if(rc != OMPI_SUCCESS)
        return rc;

    msg[0].iov_base = dataptr;
    msg[0].iov_len  = datalen;

    return(mca_oob.oob_send(peer, msg, 1, tag, flags));
}
 
