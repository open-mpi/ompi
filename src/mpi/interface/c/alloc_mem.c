/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Alloc_mem = MPI_Alloc_mem
#endif


int MPI_Alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr)
{
  if (size < 0) {
    /* Return error on MPI_COMM_WORLD */
  }
  if (NULL == baseptr) {
    /* Return error on MPI_COMM_WORLD */
  }

  /* Check the MPI_Info and see if we requested a specific MCA
     type/module.  If so, invoke that module's alloc_mem function
     (query the MCA base ?or the type base? to find the module and/or
     invoke the alloc_mem on it).  Otherwise, call malloc().

     If either fails, return an error on MPI_COMM_WORLD. */

  return MPI_SUCCESS;
}
