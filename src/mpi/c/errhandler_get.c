/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Errhandler_get = PMPI_Errhandler_get
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Errhandler_get";


OMPI_EXPORT
int MPI_Errhandler_get(MPI_Comm comm, MPI_Errhandler *errhandler) 
{
  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
  }

  /* This is a deprecated -- just turn around and call the real
     function */

  return MPI_Comm_get_errhandler(comm, errhandler);
}
