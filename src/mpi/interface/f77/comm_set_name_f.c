/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SET_NAME = mpi_comm_set_name_f
#pragma weak mpi_comm_set_name = mpi_comm_set_name_f
#pragma weak mpi_comm_set_name_ = mpi_comm_set_name_f
#pragma weak mpi_comm_set_name__ = mpi_comm_set_name_f
#if LAM_WANT_MPI_PROFILING
#pragma weak PMPI_COMM_SET_NAME = mpi_comm_set_name_f
#pragma weak pmpi_comm_set_name = mpi_comm_set_name_f
#pragma weak pmpi_comm_set_name_ = mpi_comm_set_name_f
#pragma weak pmpi_comm_set_name__ = mpi_comm_set_name_f
#endif
#endif


void
mpi_comm_set_name_f(int *comm, char *name, int *ierror, int charlen)
{
  /* JMS: Translate comm from int to MPI_Comm */
     
  MPI_Comm c_comm = MPI_COMM_WORLD;

  *ierror = MPI_Comm_set_name(c_comm, name);
}
