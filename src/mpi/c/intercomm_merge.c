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

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

#define INTERCOMM_MERGE_TAG 1010

int MPI_Intercomm_merge(MPI_Comm intercomm, int high,
                        MPI_Comm *newcomm) 
{
    lam_communicator_t *newcomp;
    lam_proc_t **procs=NULL;
    int local_size, remote_size;
    int local_rank;
    int first;
    int total_size;

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

    local_size  = lam_comm_size ( intercomm );
    local_rank  = lam_comm_rank ( intercomm );
    remote_size = lam_comm_remote_size ( intercomm );
    total_size  = local_size + remote_size;
    procs = (lam_proc_t **) malloc ( total_size * sizeof(lam_proc_t *));
    if ( NULL == procs ) {
        return LAM_ERRHANDLER_INVOKE(intercomm,MPI_ERR_INTERN, "MPI_Intercomm_merge");
    }
    
    first = lam_comm_determine_first ( intercomm, high );
    if ( first ) {
        memcpy ( procs, intercomm->c_local_group->grp_proc_pointers, 
                 local_size * sizeof(lam_proc_t *));
        memcpy ( &procs[local_size], intercomm->c_remote_group->grp_proc_pointers, 
                 remote_size * sizeof(lam_proc_t *));
    }
    else {
        memcpy ( procs, intercomm->c_remote_group->grp_proc_pointers, 
                 remote_size * sizeof(lam_proc_t *));
        memcpy ( &procs[remote_size], intercomm->c_local_group->grp_proc_pointers, 
                 local_size * sizeof(lam_proc_t *));
    }

    newcomp = lam_comm_set ( LAM_COMM_INTER_INTRA,     /* mode */
                             intercomm,                /* old comm */
                             NULL,                     /* bridge comm */
                             total_size,               /* local_size */
                             procs,                    /* local_procs*/
                             0,                        /* remote_size */
                             NULL,                     /* remote_procs */
                             NULL,                     /* attrs */
                             intercomm->error_handler, /* error handler*/
                             NULL,                     /* coll module */
                             NULL,                     /* topo mpodule */
                             MPI_UNDEFINED,            /* local leader */
                             MPI_UNDEFINED             /* remote leader */
                             );

    if ( newcomp == MPI_COMM_NULL ) {
        return LAM_ERRHANDLER_INVOKE (intercomm, MPI_ERR_INTERN, "MPI_Intercomm_merge");
    }

    if ( NULL != procs ) {
        free ( procs );
    }

    *newcomm = newcomp;
    return MPI_SUCCESS;
}
