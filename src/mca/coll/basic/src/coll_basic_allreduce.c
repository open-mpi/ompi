/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic.h"

#include "lam/constants.h"
#include "mpi.h"
#include "mpi/communicator/communicator.h"
#include "mca/mpi/coll/coll.h"
#include "mca/mpi/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	allreduce
 *
 *	Function:	- allreduce using other MPI collectives
 *	Accepts:	- same as MPI_Allreduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_allreduce(void *sbuf, void *rbuf, int count,
                             MPI_Datatype dtype, MPI_Op op,
                             MPI_Comm comm)
{
#if 1
  return LAM_ERR_NOT_IMPLEMENTED;
#else
  int err;

  /* Reduce to 0 and broadcast. */

  err = comm->c_coll.coll_reduce_intra(sbuf, rbuf, count, dtype, 
                                       op, 0, comm);
  if (MPI_SUCCESS != err)
    return err;

  return comm->c_coll.coll_bcast_intra(rbuf, count, dtype, 0, comm);
#endif
}
