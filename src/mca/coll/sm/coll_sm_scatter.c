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

#include <stdio.h>

#include "mpi.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_sm.h"


/*                                                                             
 *      scatter
 *
 *      Function:       - shared memory reduce
 *      Accepts:        - same as MPI_Scatter()
 *      Returns:        - MPI_SUCCESS or error code
 */
int 
mca_coll_sm_scatter(void *sbuf, int scount,
		    struct ompi_datatype_t *sdtype, void *rbuf,
		    int rcount, struct ompi_datatype_t *rdtype,
		    int root, struct ompi_communicator_t *comm)
{
    return MPI_SUCCESS;
}
