/*
 * $HEADER$
 */

#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Address = PMPI_Address
#endif

int MPI_Address(void *location, MPI_Aint *address){
	return MPI_SUCCESS;
}
