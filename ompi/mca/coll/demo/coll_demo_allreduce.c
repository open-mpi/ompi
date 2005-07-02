/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"

#include "mpi.h"
#include "include/constants.h"
#include "util/output.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_demo.h"


/*
 *	allreduce_intra
 *
 *	Function:	- allreduce
 *	Accepts:	- same as MPI_Allreduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_allreduce_intra(void *sbuf, void *rbuf, int count,
                                  struct ompi_datatype_t *dtype, 
                                  struct ompi_op_t *op,
                                  struct ompi_communicator_t *comm)
{
    ompi_output_verbose(10, mca_coll_base_output, "In demo allreduce_intra");
    return comm->c_coll_basic_module->coll_allreduce(sbuf, rbuf, count, dtype,
                                                     op, comm);
}


/*
 *	allreduce_inter
 *
 *	Function:	- allreduce using other MPI collectives
 *	Accepts:	- same as MPI_Allreduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_allreduce_inter(void *sbuf, void *rbuf, int count,
                                  struct ompi_datatype_t *dtype,
                                  struct ompi_op_t *op,
                                  struct ompi_communicator_t *comm)
{
    ompi_output_verbose(10, mca_coll_base_output, "In demo allreduce_inter");
    return comm->c_coll_basic_module->coll_allreduce(sbuf, rbuf, count, dtype,
                                                     op, comm);
}
