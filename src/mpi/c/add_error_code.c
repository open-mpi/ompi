/*
 *  * $HEADER$
 *   */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Add_error_code = PMPI_Add_error_code
#endif

int MPI_Add_error_code(int errorclass, int *errorcode){
	return MPI_SUCCESS;
}
