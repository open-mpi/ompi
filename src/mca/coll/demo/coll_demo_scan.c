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

#include "mpi.h"
#include "include/constants.h"
#include "util/output.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_demo.h"


/*
 *	scan
 *
 *	Function:	- scan
 *	Accepts:	- same arguments as MPI_Scan()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_scan_intra(void *sbuf, void *rbuf, int count,
                             struct ompi_datatype_t *dtype, 
                             struct ompi_op_t *op, 
                             struct ompi_communicator_t *comm)
{
    ompi_output_verbose(10, mca_coll_base_output, "In demo scan_intra");
    return comm->c_coll_basic_module->coll_scan(sbuf, rbuf, count,
                                                dtype, op, comm);
}


/*
 * NOTE: There is no exscan defined for intercommunicators (see MPI-2).
 */
