/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Errhandler_free = PMPI_Errhandler_free
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


int MPI_Errhandler_free(MPI_Errhandler *errhandler)
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (NULL == errhandler ||
        ompi_errhandler_is_intrinsic(*errhandler)) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_Errhandler_free");
    }
  }

  /* We have a valid errhandler, release it */

  OBJ_RELEASE(*errhandler);
  *errhandler = MPI_ERRHANDLER_NULL;

  /* All done */

  return MPI_SUCCESS;
}
