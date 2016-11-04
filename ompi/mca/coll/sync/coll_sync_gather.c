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
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "coll_sync.h"


/*
 *	gather
 *
 *	Function:	- gather
 *	Accepts:	- same arguments as MPI_Gather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_sync_gather(const void *sbuf, int scount,
                         struct ompi_datatype_t *sdtype,
                         void *rbuf, int rcount,
                         struct ompi_datatype_t *rdtype,
                         int root, struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module)
{
    mca_coll_sync_module_t *s = (mca_coll_sync_module_t*) module;

    if (s->in_operation) {
        return s->c_coll.coll_gather(sbuf, scount, sdtype,
                                    rbuf, rcount, rdtype, root, comm,
                                    s->c_coll.coll_gather_module);
    } else {
        COLL_SYNC(s, s->c_coll.coll_gather(sbuf, scount, sdtype,
                                           rbuf, rcount, rdtype, root, comm,
                                           s->c_coll.coll_gather_module));
    }
}
