/*
 * $HEADER$
 */

#include "lam_config.h"

#include <string.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/util/strncpy.h"
#include "lam/communicator.h"
#include "lam/totalview.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Comm_get_name = MPI_Comm_get_name
#endif

int
MPI_Comm_get_name(MPI_Comm a, char *b, int *c)
{
  return MPI_ERR_UNKNOWN;
}
