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
#pragma weak MPI_Comm_test_inter = PMPI_Comm_test_inter
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_test_inter(MPI_Comm comm, int *flag) {

    if ( MPI_PARAM_CHECK ) {
        if (lam_mpi_finalized )
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Comm_test_inter");
        
        if ( MPI_COMM_NULL == comm || lam_comm_invalid ( comm ) )
             return LAM_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_COMM,
                                            "MPI_Comm_test_inter");
        
        if ( NULL == flag ) 
             return LAM_ERRHANDLER_INVOKE ( comm, MPI_ERR_ARG, 
                                            "MPI_Comm_test_inter");
    }

    *flag = (comm->c_flags & LAM_COMM_INTER);
    return MPI_SUCCESS;
}
