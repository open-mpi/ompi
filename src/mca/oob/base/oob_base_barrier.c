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
#include "util/proc_info.h"

#include "mca/ns/ns.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"


int mca_oob_barrier(void)
{
    orte_process_name_t* peers;
    size_t i, npeers, self;
    struct iovec iov;
    int foo = 0;

    int rc = orte_ns.get_peers(&peers,&npeers,&self);
    if(rc != OMPI_SUCCESS)
        return rc;

    iov.iov_base = (void*)&foo;
    iov.iov_len = sizeof(foo);

    /* All non-root send & receive zero-length message. */
    if (0 < self) {
        int tag=-1;
        rc = mca_oob_send(&peers[0],&iov,1,tag,0);
        if (rc < 0) {
            return rc;
        }
 
        rc = mca_oob_recv(&peers[0],&iov,1,tag,0);
        if (rc < 0) {
            return rc;
        }
    }

    /* The root collects and broadcasts the messages. */
    else {
        int tag=-1;
        for (i = 1; i < npeers; i++) {
            rc = mca_oob_recv(&peers[i],&iov,1,tag,0);
            if (rc < 0) {
                return rc;
            }
        }

        for (i = 1; i < npeers; i++) {
            rc = mca_oob_send(&peers[i],&iov,1,tag,0);
            if (rc < 0) {
                return rc;
            }
        }
    }
    return OMPI_SUCCESS;
}


