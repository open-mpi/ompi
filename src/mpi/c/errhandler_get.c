/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Errhandler_get = PMPI_Errhandler_get
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


int MPI_Errhandler_get(MPI_Comm comm, MPI_Errhandler *errhandler) 
{
  /* This is a deprecated -- just turn around and call the real
     function */

  return MPI_Comm_get_errhandler(comm, errhandler);
}
