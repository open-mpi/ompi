/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "runtime/runtime.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_rank = PMPI_Comm_rank
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


int MPI_Comm_rank(MPI_Comm comm, int *rank) 
{
    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Comm_rank");

        if ( MPI_COMM_NULL == comm || ompi_comm_invalid (comm))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_rank");

        if ( NULL == rank )
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                         "MPI_Comm_rank");
    }

    *rank = ompi_comm_rank((ompi_communicator_t*)comm);
    return MPI_SUCCESS;
}

