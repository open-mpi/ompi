/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Free_mem = PMPI_Free_mem
#endif


int MPI_Free_mem(void *baseptr)
{
  if (NULL == baseptr) {
    /* Return error on MPI_COMM_WORLD */
  }

  /* Look and see if this pointer was allocated with a specific
     MPI_Info that got the memory from a module's alloc_mem function.
     If so, call the module's corresponding free_mem function.
     Otherwise, call free().

     If either fails, return an error on MPI_COMM_WORLD. */

  return MPI_SUCCESS;
}
