/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Topo_test = PMPI_Topo_test
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Topo_test(MPI_Comm comm, int *status) 
{

    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Topo_test");

        if ( MPI_COMM_NULL == comm || ompi_comm_invalid (comm))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Topo_test");

        if ( NULL == status )
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                         "MPI_Topo_test");
    }

    if ( comm->c_flags & OMPI_COMM_CART ) 
        *status = MPI_CART;
    else if ( comm->c_flags & OMPI_COMM_GRAPH ) 
        *status = MPI_GRAPH;
    else
        *status = MPI_UNDEFINED;

    return MPI_SUCCESS;
}
