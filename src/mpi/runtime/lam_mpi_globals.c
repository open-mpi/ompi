/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"


/*
 * Global variables and symbols for the MPI layer
 */

bool lam_mpi_initialized = false;
bool lam_mpi_finalized = false;

bool lam_mpi_thread_multiple = false;
int lam_mpi_thread_requested = MPI_THREAD_SINGLE;
int lam_mpi_thread_provided = MPI_THREAD_SINGLE;

