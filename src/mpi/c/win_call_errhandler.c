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

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Win_call_errhandler = PMPI_Win_call_errhandler
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Win_call_errhandler(MPI_Win win, int errorcode) {
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (NULL == win ||
        MPI_WIN_NULL == win) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                   "MPI_Win_call_errhandler");
    }
  }

  /* Invoke the errhandler */

  return OMPI_ERRHANDLER_INVOKE(win, errorcode,
                               "MPI_Win_call_errhandler");
}
