/*
 * $HEADER$
 */

#include "lam_config.h"

#include <string.h>

#include "mpi.h"
#if 0
/* JMS Need to continue here */
#include "mpi/f77/profiling.h"
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#include "mpi/f77/weak_prototypes.h"
#pragma weak mpi_comm_set_name_ = mpi_comm_set_name_f
#pragma weak mpi_comm_set_name__ = mpi_comm_set_name_f
#pragma weak mpi_comm_set_name = mpi_comm_set_name_f
#pragma weak MPI_COMM_SET_NAME = mpi_comm_set_name_f

#if LAM_WANT_MPI_PROFILING
#pragma weak pmpi_comm_set_name_ = mpi_comm_set_name_f
#pragma weak pmpi_comm_set_name__ = mpi_comm_set_name_f
#pragma weak pmpi_comm_set_name = mpi_comm_set_name_f
#pragma weak PMPI_COMM_SET_NAME = mpi_comm_set_name_f

#endif
#else
#if 0
/* JMS Need to continue here */
//#include "mpi-fortran-"
#endif
#endif

int
mpi_comm_set_name_f(int *comm, char *name)
{
  /* JMS: Translate comm from int to MPI_Comm */
     
  MPI_Comm c_comm = MPI_COMM_WORLD;

  return MPI_Comm_set_name(c_comm, name);
}
