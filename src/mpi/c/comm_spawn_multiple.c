/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_spawn_multiple = PMPI_Comm_spawn_multiple
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_spawn_multiple(int count, char **array_of_commands, char ***array_of_argv,
                            int *array_of_maxprocs, MPI_Info *array_of_info,
                            int root, MPI_Comm comm, MPI_Comm *intercomm,
                            int *array_of_errcodes) {
    return MPI_SUCCESS;
}
