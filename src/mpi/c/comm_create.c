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
#pragma weak MPI_Comm_create = PMPI_Comm_create
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *newcomm) {
    
    int rc;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE; 
        
        if ( MPI_COMM_NULL == comm  || ompi_comm_invalid (comm))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_create");
        
        if ( MPI_GROUP_NULL == group )
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_GROUP, 
                                         "MPI_Comm_create");
        
        if ( NULL == newcomm )
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                         "MPI_Comm_create");
    }

    rc = ompi_comm_create ( (ompi_communicator_t*)comm, (ompi_group_t*)group, 
                           (ompi_communicator_t**)newcomm );
    OMPI_ERRHANDLER_RETURN ( rc, comm, rc, "MPI_Comm_create");
}
