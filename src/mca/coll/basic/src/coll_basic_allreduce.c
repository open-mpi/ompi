/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include "mpi.h"
#include "include/constants.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	allreduce_intra
 *
 *	Function:	- allreduce using other MPI collectives
 *	Accepts:	- same as MPI_Allreduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_allreduce_intra(void *sbuf, void *rbuf, int count,
                                   struct ompi_datatype_t *dtype, 
                                   struct ompi_op_t *op,
                                   struct ompi_communicator_t *comm)
{
  int err;

  /* Reduce to 0 and broadcast. */

  err = comm->c_coll.coll_reduce(sbuf, rbuf, count, dtype, op, 0, comm);
  if (MPI_SUCCESS != err)
    return err;

  return comm->c_coll.coll_bcast(rbuf, count, dtype, 0, comm);
}


/*
 *	allreduce_inter
 *
 *	Function:	- allreduce using other MPI collectives
 *	Accepts:	- same as MPI_Allreduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_allreduce_inter(void *sbuf, void *rbuf, int count,
                                   struct ompi_datatype_t *dtype,
                                   struct ompi_op_t *op,
                                   struct ompi_communicator_t *comm)
{
  /* Need to implement this */

  return OMPI_ERR_NOT_IMPLEMENTED;
}
