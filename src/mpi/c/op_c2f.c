/*
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "op/op.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Op_c2f = PMPI_Op_c2f
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Op_c2f";


MPI_Fint MPI_Op_c2f(MPI_Op op) 
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

    /* mapping an invalid handle to a null handle */
    /* not invoking an error handler */
    if (NULL == op) {
		op = MPI_OP_NULL;
    }
  }

  /* All done */

  return (MPI_Fint) op->o_f_to_c_index;
}
