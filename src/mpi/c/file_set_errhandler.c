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
#pragma weak MPI_File_set_errhandler = PMPI_File_set_errhandler
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_File_set_errhandler( MPI_File file, MPI_Errhandler errhandler) {
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (NULL == file || 
        MPI_FILE_NULL == file) {
      return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_File_set_errhandler");
    } else if (NULL == errhandler ||
               MPI_ERRHANDLER_NULL == errhandler ||
               LAM_ERRHANDLER_TYPE_FILE != errhandler->eh_mpi_object_type) {
      return LAM_ERRHANDLER_INVOKE(file, MPI_ERR_ARG,
                               "MPI_File_set_errhandler");
    }
  }

  /* Ditch the old errhandler, and decrement its refcount */

  OBJ_RELEASE(file->error_handler);

  /* We have a valid comm and errhandler, so increment its refcount */

  file->error_handler = errhandler;
  OBJ_RETAIN(file->error_handler);

  /* All done */
  return MPI_SUCCESS;
}
