/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_c2f = PMPI_Comm_c2f
#endif

MPI_Fint MPI_Comm_c2f(MPI_Comm comm) {
	/*
	 * Anju:
	 * Dont know what it is supposed to return
	 */
    return MPI_SUCCESS;
}
