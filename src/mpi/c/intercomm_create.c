/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"


#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Intercomm_create = PMPI_Intercomm_create
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Intercomm_create(MPI_Comm local_comm, int local_leader,
                         MPI_Comm bridge_comm, int remote_leader,
                         int tag, MPI_Comm *newintercomm) {

    int local_size, local_rank;

    if ( MPI_PARAM_CHECK ) {
        if ( lam_mpi_finalized ) 
            return LAM_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_INTERN,
                                           "MPI_Intercomm_create");

        if ( MPI_COMM_NULL == local_comm || lam_comm_invalid ( local_comm ) ||
             ( local_comm->c_flags & LAM_COMM_INTER ) ) 
            return LAM_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_COMM,
                                           "MPI_Intercomm_create");

        if ( NULL == newintercomm )
            return LAM_ERRHANDLER_INVOKE ( local_comm, MPI_ERR_ARG, 
                                           "MPI_Intercomm_create");
        
        if ( tag < 0 || tag > MPI_TAG_UB )
            return LAM_ERRHANDLER_INVOKE ( local_comm, MPI_ERR_ARG, 
                                           "MPI_Intercomm_create");
    }

    local_size = lam_comm_size ( local_comm );
    local_rank = lam_comm_size ( local_comm );

    if ( MPI_PARAM_CHECK ) {
        if ( local_leader < 0 || local_leader > local_size ) 
            return LAM_ERRHANDLER_INVOKE ( local_comm, MPI_ERR_ARG, 
                                           "MPI_Intercomm_create");

        /* remember that the remote_leader and bridge_comm arguments
           just have to be valid at the local_leader */
        if ( local_rank == local_leader ) {
            if ( MPI_COMM_NULL == bridge_comm || lam_comm_invalid ( bridge_comm) ||
                 bridge_comm->c_flags & LAM_COMM_INTER ) 
                return LAM_ERRHANDLER_INVOKE ( local_comm, MPI_ERR_COMM, 
                                               "MPI_Intercomm_create");
            
            if ( remote_leader < 0 || remote_leader > lam_comm_size(bridge_comm))
                return LAM_ERRHANDLER_INVOKE ( local_comm, MPI_ERR_ARG,
                                               "MPI_Intercomm_create");
        } /* if ( local_rank == local_leader ) */
    }

    return MPI_SUCCESS;
}
