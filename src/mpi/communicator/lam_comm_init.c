/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/communicator/communicator.h"


/*
 * Global variables
 */

lam_communicator_t *lam_mpi_comm_array;
size_t lam_mpi_comm_array_size;

lam_communicator_t lam_mpi_comm_world;
lam_communicator_t lam_mpi_comm_self;


/*
 * This is a shell function that needs to be filled in.  It is here so
 * that this file will get linked into MPI executables (POSIX says
 * that linkers do not have to pull .o files from libraries without
 * function symbols -- global variable symbols alone are not
 * sufficient to pull in .o files).
 */
int lam_comm_init(lam_communicator_t *comm)
{
  return LAM_SUCCESS;
}
