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
#pragma weak MPI_Comm_free = PMPI_Comm_free
#endif

int MPI_Comm_free(MPI_Comm *comm) {
    
    if ( MPI_PARAM_CHECK ) {
        if (lam_mpi_finalized )
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Comm_free");
        
        if ( NULL == *comm  || MPI_COMM_WORLD == *comm ||
             MPI_COMM_SELF == *comm  || lam_comm_invalid (*comm))
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_free");
    }
    
    /* Call attribute delete functions */
    
    /* free the object */

    lam_comm_free ( comm ); 
    
    *comm = MPI_COMM_NULL;
    return MPI_SUCCESS;
}
