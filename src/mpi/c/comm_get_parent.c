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
#pragma weak MPI_Comm_get_parent = PMPI_Comm_get_parent
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_get_parent(MPI_Comm *parent) 
{

    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                          "MPI_Comm_get_parent");
        if ( NULL == parent )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                         "MPI_Comm_get_parent");
    }
    /*
     * ompi_mpi_comm_parent is MPI_COMM_NULL, in case this 
     * world has not been spawned. This is also the return
     * value required by MPI-2.

     *parent = &ompi_mpi_comm_parent;
     */

    return MPI_SUCCESS;
}
