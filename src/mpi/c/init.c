/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "util/sys_info.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Init = PMPI_Init
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


int MPI_Init(int *argc, char ***argv)
{
  int provided;
  char *env;
  int requested = MPI_THREAD_SINGLE;

  /* check for environment overrides for requested thread level.  If
     there is, check to see that it is a valid/supported thread level.
     If not, default to MPI_THREAD_SINGLE. */

  if (NULL != (env = getenv("OMPI_MPI_THREAD_LEVEL"))) {
    requested = atoi(env);
    if (requested < MPI_THREAD_SINGLE || requested > MPI_THREAD_MULTIPLE) {
      /* JMS call the show_help() interface */
      exit(1);
    }
  } 

  /* Get the local system information and populate the ompi_system_info structure */
  ompi_sys_info();
    
  /* Call the back-end initialization function (we need to put as
     little in this function as possible so that if it's profiled, we
     don't lose anything) */

  return ompi_mpi_init(*argc, *argv, requested, &provided);
}
