/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"
#include "file/file.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_File_create_errhandler = PMPI_File_create_errhandler
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_create_errhandler";


OMPI_EXPORT
int MPI_File_create_errhandler(MPI_File_errhandler_fn *function,
		                        MPI_Errhandler *errhandler) {
  int err = MPI_SUCCESS;

  /* Error checking */

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    if (NULL == function || 
        NULL == errhandler) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_File_create_errhandler");
    }
  }

  /* Create and cache the errhandler.  Sets a refcount of 1. */

  *errhandler = 
    ompi_errhandler_create(OMPI_ERRHANDLER_TYPE_FILE,
                          (ompi_errhandler_generic_handler_fn_t*) function);
  if (NULL == *errhandler) {
    err = MPI_ERR_INTERN;
  }

  OMPI_ERRHANDLER_RETURN(err, MPI_COMM_WORLD, MPI_ERR_INTERN,
                        "MPI_File_create_errhandler");
}
