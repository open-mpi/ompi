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
#pragma weak MPI_Comm_group = PMPI_Comm_group
#endif

int MPI_Comm_group(MPI_Comm comm, MPI_Group *group) {

    int rc;

    /* argument checking */
    if ( MPI_PARAM_CHECK ) {
        if ( lam_mpi_finalized ) 
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Comm_group");

        if ( MPI_COMM_NULL == comm || lam_comm_invalid (comm) )
           return  LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_group");

        if ( NULL == group ) 
            return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                         "MPI_Comm_group");
    } /* end if ( MPI_PARAM_CHECK) */

    
   rc = lam_comm_group ( (lam_communicator_t*)comm, (lam_group_t**)group );
   LAM_ERRHANDLER_RETURN ( rc, comm, rc, "MPI_Comm_group");
}
