/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_set_errhandler = PMPI_Comm_set_errhandler
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Comm_set_errhandler";


int MPI_Comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler) 
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    if (ompi_comm_invalid(comm)) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM,
                                    FUNC_NAME);
    } else if (NULL == errhandler ||
               MPI_ERRHANDLER_NULL == errhandler ||
               OMPI_ERRHANDLER_TYPE_COMM != errhandler->eh_mpi_object_type) {
      return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                    FUNC_NAME);
    }
  }

  /* Ditch the old errhandler, and decrement its refcount */

  OBJ_RELEASE(comm->error_handler);

  /* We have a valid comm and errhandler, so increment its refcount */

  comm->error_handler = errhandler;
  OBJ_RETAIN(comm->error_handler);

  /* All done */
  
  return MPI_SUCCESS;
}
