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
