/*
 * $HEADERS$
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
#pragma weak MPI_Win_get_errhandler = PMPI_Win_get_errhandler
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Win_get_errhandler";


OMPI_EXPORT
int MPI_Win_get_errhandler(MPI_Win win, MPI_Errhandler *errhandler) 
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    if (NULL == win || 
        MPI_WIN_NULL == win) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                    FUNC_NAME);
    } else if (NULL == errhandler) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                    FUNC_NAME);
    }
  }

  /* Return the errhandler.  Do not increase the refcount here; we
     only refcount on communicators */

  *errhandler = win->error_handler;

  /* All done */

  return MPI_SUCCESS;
}
