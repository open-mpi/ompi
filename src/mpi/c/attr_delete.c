/*
 * $HEADER$
 */

#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Attr_delete = PMPI_Attr_delete
#endif

int MPI_Attr_delete(MPI_Comm comm, int keyval){
	return MPI_SUCCESS;
}

