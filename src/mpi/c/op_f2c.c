/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "op/op.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Op_f2c = PMPI_Op_f2c
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

MPI_Op MPI_Op_f2c(MPI_Fint op_f)
{
  size_t o_index = (size_t) op_f;

  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (0 > o_index || 
        o_index >= ompi_pointer_array_get_size(ompi_op_f_to_c_table)) {
      return MPI_OP_NULL;
    }
  }

  /* All done */

  return ompi_op_f_to_c_table->addr[o_index];
}
