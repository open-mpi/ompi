/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Errhandler_c2f = PMPI_Errhandler_c2f
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


MPI_Fint MPI_Errhandler_c2f(MPI_Errhandler errhandler)
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (NULL == errhandler ||
        MPI_ERRHANDLER_NULL == errhandler ||
        LAM_ERRHANDLER_TYPE_COMM != errhandler->eh_mpi_object_type) {
      return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_Errhandler_c2f");
    }
  }

  /* All done */

  return (MPI_Fint) errhandler->eh_f_to_c_index;
}
