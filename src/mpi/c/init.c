/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>

#include "mpi.h"
#include "include/constants.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Init = PMPI_Init
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Init";


int MPI_Init(int *argc, char ***argv)
{
  int err;
  int provided;
  char *env;
  int required = MPI_THREAD_SINGLE;

  /* Ensure that we were not already initialized or finalized */

  if (ompi_mpi_finalized) {
      /* JMS show_help */
      return ompi_errhandler_invoke(NULL, NULL, OMPI_ERRHANDLER_TYPE_COMM, 
                                    MPI_ERR_OTHER, FUNC_NAME);
  } else if (ompi_mpi_initialized) {
    /* JMS show_help */
    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, FUNC_NAME);
  }

  /* check for environment overrides for required thread level.  If
     there is, check to see that it is a valid/supported thread level.
     If not, default to MPI_THREAD_MULTIPLE. */

  if (NULL != (env = getenv("OMPI_MPI_THREAD_LEVEL"))) {
    required = atoi(env);
    if (required < MPI_THREAD_SINGLE || required > MPI_THREAD_MULTIPLE) {
      required = MPI_THREAD_MULTIPLE;
    }
  } 

  /* Call the back-end initialization function (we need to put as
     little in this function as possible so that if it's profiled, we
     don't lose anything) */

  if (NULL != argc && NULL != argv) {
      err = ompi_mpi_init(*argc, *argv, required, &provided);
  } else {
      err = ompi_mpi_init(0, NULL, required, &provided);
  }

  /* Since we don't have a communicator to invoke an errorhandler on
     here, don't use the fancy-schmancy ERRHANDLER macros; they're
     really designed for real communicator objects.  Just use the
     back-end function directly. */

  if (MPI_SUCCESS != err) {
      return ompi_errhandler_invoke(NULL, NULL, OMPI_ERRHANDLER_TYPE_COMM, 
                                    err < 0 ? ompi_errcode_get_mpi_code(err) : 
                                    err, FUNC_NAME);
  }
  return MPI_SUCCESS;
}
