/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"
#include "file/file.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_get_errhandler = PMPI_File_get_errhandler
#endif

int MPI_File_get_errhandler( MPI_File file, MPI_Errhandler *errhandler) {
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (NULL == file || 
        MPI_FILE_NULL == file) {
      return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_File_get_errhandler");
    } else if (NULL == errhandler) {
      return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_File_get_errhandler");
    }
  }

  /* Return the errhandler.  Do not increase the refcount here; we
     only refcount on communicators */

  *errhandler = file->error_handler;

  /* All done */
  return MPI_SUCCESS;
}
