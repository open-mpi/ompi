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
#pragma weak MPI_Comm_create = PMPI_Comm_create
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *newcomm) {
    
    int rc;

    if ( MPI_PARAM_CHECK ) {
        if ( lam_mpi_finalized )
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Comm_create");
        
        if ( MPI_COMM_NULL == comm  || lam_comm_invalid (comm))
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_create");
        
        if ( MPI_GROUP_NULL == group )
            return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_GROUP, 
                                         "MPI_Comm_create");
        
        if ( NULL == newcomm )
            return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                         "MPI_Comm_create");
    }

    rc = lam_comm_create ( (lam_communicator_t*)comm, (lam_group_t*)group, 
                           (lam_communicator_t**)newcomm );
    LAM_ERRHANDLER_RETURN ( rc, comm, rc, "MPI_Comm_create");
}
