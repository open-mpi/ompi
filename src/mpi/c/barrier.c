/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mca/coll/coll.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Barrier = PMPI_Barrier
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Barrier";


int MPI_Barrier(MPI_Comm comm)
{
    int err;
    mca_coll_base_barrier_fn_t func;

    if (MPI_PARAM_CHECK) {
      if (MPI_COMM_NULL == comm) {
	return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                     FUNC_NAME);
      }
    }

    /* Obvious case */

    if (ompi_comm_size(comm) <= 1) {
	return MPI_SUCCESS;
    }

    /* VPS: Need to change this to another pointer, because we wont have
       two pointers - intra and inter - cached in the new design */
    func = comm->c_coll.coll_barrier_intra;

    if (NULL == func) {
      return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_OTHER, FUNC_NAME);
    }

    err = func(comm);
    
    OMPI_ERRHANDLER_RETURN(err, comm, MPI_ERR_UNKNOWN, FUNC_NAME);
}
