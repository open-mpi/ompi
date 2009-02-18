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
 *	alltoallv
 *
 *	Function:	- MPI_Alltoallv
 *	Accepts:	- same as MPI_Alltoallv()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int mca_coll_sync_alltoallv(void *sbuf, int *scounts, int *sdisps,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, int *rcounts, int *rdisps,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    mca_coll_sync_module_t *s = (mca_coll_sync_module_t*) module;

    if (s->in_operation) {
        return s->c_coll.coll_alltoallv(sbuf, scounts, sdisps, sdtype,
                                       rbuf, rcounts, rdisps, rdtype, comm,
                                        s->c_coll.coll_alltoallv_module);
    } else {
        COLL_SYNC(s, s->c_coll.coll_alltoallv(sbuf, scounts, sdisps, sdtype,
                                              rbuf, rcounts, rdisps, rdtype, 
                                              comm,
                                              s->c_coll.coll_alltoallv_module));
    }
}
