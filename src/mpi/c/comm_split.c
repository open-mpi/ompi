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
#pragma weak MPI_Comm_split = PMPI_Comm_split
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm) {

    int rc;

    if ( MPI_PARAM_CHECK ) {
        if ( lam_mpi_finalized ) 
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                      "MPI_Comm_split");

        if ( comm == MPI_COMM_NULL || lam_comm_invalid ( comm ))
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                      "MPI_Comm_split");

        if ( color < 0 &&  MPI_UNDEFINED != color ) 
            return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                      "MPI_Comm_split");
        
        if ( NULL == newcomm )
            return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                      "MPI_Comm_split");
    }
    
    rc = lam_comm_split ( (lam_communicator_t*)comm, color, key, 
                          (lam_communicator_t**)newcomm );
    LAM_ERRHANDLER_RETURN ( rc, comm, rc, "MPI_Comm_split");
}
