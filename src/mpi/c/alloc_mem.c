/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Alloc_mem = PMPI_Alloc_mem
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Alloc_mem";


int MPI_Alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr)
{
  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    if (size < 0 || NULL == baseptr) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                    FUNC_NAME);
    }
  }

  /* Check the MPI_Info and see if we requested a specific MCA
     type/module.  If so, invoke that module's alloc_mem function
     (query the MCA base ?or the type base? to find the module and/or
     invoke the alloc_mem on it).  Otherwise, call malloc().

     If either fails, return an error on MPI_COMM_WORLD. */

  /* This function is not yet implemented */

  return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, FUNC_NAME);
}
