/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>
#include <string.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"


#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_remote_group = PMPI_Comm_remote_group
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


int MPI_Comm_remote_group(MPI_Comm comm, MPI_Group *group) 
{

    if ( MPI_PARAM_CHECK ) {
        if ( lam_mpi_finalized )
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Comm_remote_group");

        if (MPI_COMM_NULL == comm || lam_comm_invalid (comm))
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_remote_group");

        if ( NULL == group )
            return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                         "MPI_Comm_remote_group");
    }

    if ( LAM_COMM_IS_INTER(comm) ) {        
        OBJ_RETAIN(comm->c_remote_group);
    }
    else
        return LAM_ERRHANDLER_INVOKE (comm, MPI_ERR_COMM, 
                                      "MPI_Comm_remote_group");

    *group = (MPI_Group) comm->c_remote_group;
    return MPI_SUCCESS;
}
