/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Intercomm_merge = PMPI_Intercomm_merge
#endif

int MPI_Intercomm_merge(MPI_Comm intercomm, int high,
                        MPI_Comm *newcomm) {


    if ( MPI_PARAM_CHECK ) {
        if ( lam_mpi_finalized ) 
            return LAM_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_INTERN,
                                           "MPI_Intercomm_merge");

        if ( MPI_COMM_NULL == intercomm || lam_comm_invalid ( intercomm ) ||
             !( intercomm->c_flags & LAM_COMM_INTER ) ) 
            return LAM_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_COMM,
                                           "MPI_Intercomm_merge");

        if ( NULL == newcomm )
            return LAM_ERRHANDLER_INVOKE ( intercomm, MPI_ERR_ARG, 
                                           "MPI_Intercomm_merge");
    }

    return MPI_SUCCESS;
}
