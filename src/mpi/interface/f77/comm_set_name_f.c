/*
 * $HEADER$
 */

#include <string.h>

#include "mpi.h"


int
mpi_comm_set_name_f(int *comm, char *name)
{
  /* JMS: Translate comm from int to MPI_Comm */
     
  MPI_Comm c_comm = MPI_COMM_WORLD;

  return MPI_Comm_set_name(c_comm, name);
}
