/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_demo.h"

#include <stdio.h>
#include <errno.h>

#include "mpi.h"
#include "include/constants.h"
#include "util/output.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_demo.h"


/*
 *	reduce_scatter
 *
 *	Function:	- reduce then scatter
 *	Accepts:	- same as MPI_Reduce_scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_reduce_scatter_intra(void *sbuf, void *rbuf, int *rcounts,
                                       struct ompi_datatype_t *dtype,
                                       struct ompi_op_t *op,
                                       struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo scatter_intra");
  return comm->c_coll_basic_module->coll_reduce_scatter(sbuf, rbuf, rcounts,
                                                        dtype, op, comm);
}


/*
 *	reduce_scatter_inter
 *
 *	Function:	- reduce/scatter operation
 *	Accepts:	- same arguments as MPI_Reduce_scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_demo_reduce_scatter_inter(void *sbuf, void *rbuf, int *rcounts,
                                       struct ompi_datatype_t *dtype,
                                       struct ompi_op_t *op,
                                       struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In demo scatter_inter");
  return comm->c_coll_basic_module->coll_reduce_scatter(sbuf, rbuf, rcounts,
                                                        dtype, op, comm);
}
