/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Topo_test = PMPI_Topo_test
#endif

int MPI_Topo_test(MPI_Comm comm, int *status) 
{

    if ( MPI_PARAM_CHECK ) {
        if ( lam_mpi_finalized )
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Topo_test");

        if ( MPI_COMM_NULL == comm || lam_comm_invalid (comm))
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Topo_test");

        if ( NULL == status )
            return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                         "MPI_Topo_test");
    }

    if ( comm->c_flags & LAM_COMM_CART ) 
        *status = MPI_CART;
    else if ( comm->c_flags & LAM_COMM_GRAPH ) 
        *status = MPI_GRAPH;
    else
        *status = MPI_UNDEFINED;

    return MPI_SUCCESS;
}
