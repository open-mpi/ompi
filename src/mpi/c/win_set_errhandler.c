/*
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"
#include "win/win.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Win_set_errhandler = PMPI_Win_set_errhandler
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Win_set_errhandler";


int MPI_Win_set_errhandler(MPI_Win win, MPI_Errhandler errhandler) 
{
  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
  }

  /* This function is not yet implemented */

  return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, FUNC_NAME);

  if (MPI_PARAM_CHECK) {
    if (NULL == win || 
        MPI_WIN_NULL == win) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_Win_set_errhandler");
    } else if (NULL == errhandler ||
               MPI_ERRHANDLER_NULL == errhandler ||
               (OMPI_ERRHANDLER_TYPE_WIN != errhandler->eh_mpi_object_type && 
		OMPI_ERRHANDLER_TYPE_PREDEFINED != errhandler->eh_mpi_object_type) ) {
      return OMPI_ERRHANDLER_INVOKE(win, MPI_ERR_ARG,
                               "MPI_Win_set_errhandler");
    }
  }

  /* Ditch the old errhandler, and decrement its refcount */

  OBJ_RELEASE(win->error_handler);

  /* We have a valid comm and errhandler, so increment its refcount */

  win->error_handler = errhandler;
  OBJ_RETAIN(win->error_handler);

  /* All done */
  
  return MPI_SUCCESS;
}
