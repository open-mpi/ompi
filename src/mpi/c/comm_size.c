/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "runtime/runtime.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_size = PMPI_Comm_size
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_size(MPI_Comm comm, int *size) {

    if ( MPI_PARAM_CHECK ) {
        if ( lam_mpi_finalized )
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, 
                                         MPI_ERR_INTERN, "MPI_Comm_size");

        if ( MPI_COMM_NULL == comm || lam_comm_invalid (comm))
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, 
                                         MPI_ERR_COMM, "MPI_Comm_size");

        if ( NULL == size )
            return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, "MPI_Comm_size");
    }

    *size = lam_comm_size((lam_communicator_t*)comm);
    return MPI_SUCCESS;
}
