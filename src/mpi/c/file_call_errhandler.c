/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"
#include "file/file.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_call_errhandler = PMPI_File_call_errhandler
#endif

int MPI_File_call_errhandler(MPI_File fh, int errorcode) {
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (NULL == fh ||
        MPI_FILE_NULL == fh) {
      return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_File_call_errhandler");
    }
  }

  /* Invoke the errhandler */

  return LAM_ERRHANDLER_INVOKE(fh, errorcode,
                               "MPI_File_call_errhandler");
}
