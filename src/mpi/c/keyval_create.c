/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Keyval_create = PMPI_Keyval_create
#endif

int MPI_Keyval_create(MPI_Copy_function *copy_fn,
                      MPI_Delete_function *delete_fn,
                      int *keyval, void *extra_state) {
    return MPI_SUCCESS;
}
