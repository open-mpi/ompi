/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Test_cancelled = PMPI_Test_cancelled
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Test_cancelled";


int MPI_Test_cancelled(MPI_Status *status, int *flag) 
{
  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    if (NULL == flag || NULL == status) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
    }
  }

  /* JMS clearly something else needs to be done here... */
  *flag = 0;

  /* This function is not yet implemented */

  return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, FUNC_NAME);
}
