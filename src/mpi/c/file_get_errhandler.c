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
#pragma weak MPI_File_get_errhandler = PMPI_File_get_errhandler
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_File_get_errhandler";


int MPI_File_get_errhandler( MPI_File file, MPI_Errhandler *errhandler) 
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    if (NULL == file || 
        MPI_FILE_NULL == file) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_FILE,
                                   "MPI_File_get_errhandler");
    } else if (NULL == errhandler) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_File_get_errhandler");
    }
  }

  /* Return the errhandler.  Do not increase the refcount here; we
     only refcount on communicators */

  *errhandler = file->error_handler;

  /* All done */

  return MPI_SUCCESS;
}
