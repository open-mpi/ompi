/*
 * $HEADER$
 */

#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Attr_get = PMPI_Attr_get
#endif

int MPI_Attr_get(MPI_Comm comm, int keyval, void *attribute_val, int *flag) {
	return MPI_SUCCESS;
}

