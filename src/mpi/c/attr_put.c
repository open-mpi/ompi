/*
 * $HEADER$
 */

#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Attr_put = PMPI_Attr_put
#endif

int MPI_Attr_put(MPI_Comm comm, int keyval, void *attribute_val) {
		return MPI_SUCCESS;
}

