/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Errhandler_f2c = PMPI_Errhandler_f2c
#endif


MPI_Errhandler MPI_Errhandler_f2c(MPI_Fint errhandler_f)
{
  size_t eh_index = (size_t) errhandler_f;

  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (0 > eh_index || 
        eh_index >= lam_pointer_array_get_size(lam_errhandler_f_to_c_table)) {
      return MPI_ERRHANDLER_NULL;
    }
  }

  /* All done */

  return lam_errhandler_f_to_c_table->addr[eh_index];
}
