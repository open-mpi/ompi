/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>
#include <string.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"


#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_remote_group = PMPI_Comm_remote_group
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


int MPI_Comm_remote_group(MPI_Comm comm, MPI_Group *group) 
{

    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Comm_remote_group");

        if (MPI_COMM_NULL == comm || ompi_comm_invalid (comm))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_remote_group");

        if ( NULL == group )
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                         "MPI_Comm_remote_group");
    }

    if ( OMPI_COMM_IS_INTER(comm) ) {        
        OBJ_RETAIN(comm->c_remote_group);
    }
    else
        return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_COMM, 
                                      "MPI_Comm_remote_group");

    *group = (MPI_Group) comm->c_remote_group;
    return MPI_SUCCESS;
}
