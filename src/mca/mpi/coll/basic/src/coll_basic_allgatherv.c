/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic_config.h"

#include "lam/constants.h"
#include "mpi.h"
#include "mpi/communicator/communicator.h"
#include "mca/mpi/coll/coll.h"
#include "coll_basic.h"


/*
 *	allgatherv
 *
 *	Function:	- allgather using other MPI collectives
 *	Accepts:	- same as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_allgatherv(void *sbuf, int scount, 
                              MPI_Datatype sdtype, void * rbuf, 
                              int *rcounts, int *disps, 
                              MPI_Datatype rdtype, 
                              MPI_Comm comm)
{
  int i, size;
  int err;

  /* Collect all values at each process, one at a time. */

  MPI_Comm_size(comm, &size);
  for (i = 0; i < size; ++i) {
    err = comm->c_coll.coll_gatherv_intra(sbuf, scount, sdtype, rbuf,
                                          rcounts, disps, rdtype, i, comm);
    if (MPI_SUCCESS != err)
      return err;
  }

  return MPI_SUCCESS;
}
