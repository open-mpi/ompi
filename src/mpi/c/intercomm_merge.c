/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Intercomm_merge = PMPI_Intercomm_merge
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

#define INTERCOMM_MERGE_TAG 1010

int MPI_Intercomm_merge(MPI_Comm intercomm, int high,
                        MPI_Comm *newcomm) 
{
    ompi_communicator_t *newcomp;
    ompi_proc_t **procs=NULL;
    int local_size, remote_size;
    int local_rank;
    int first;
    int total_size;
    int rc=MPI_SUCCESS;

    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized ) 
            return OMPI_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_INTERN,
                                           "MPI_Intercomm_merge");

        if ( MPI_COMM_NULL == intercomm || ompi_comm_invalid ( intercomm ) ||
             !( intercomm->c_flags & OMPI_COMM_INTER ) ) 
            return OMPI_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_COMM,
                                           "MPI_Intercomm_merge");

        if ( NULL == newcomm )
            return OMPI_ERRHANDLER_INVOKE ( intercomm, MPI_ERR_ARG, 
                                           "MPI_Intercomm_merge");
    }

    local_size  = ompi_comm_size ( intercomm );
    local_rank  = ompi_comm_rank ( intercomm );
    remote_size = ompi_comm_remote_size ( intercomm );
    total_size  = local_size + remote_size;
    procs = (ompi_proc_t **) malloc ( total_size * sizeof(ompi_proc_t *));
    if ( NULL == procs ) {
        return OMPI_ERRHANDLER_INVOKE(intercomm,MPI_ERR_INTERN, "MPI_Intercomm_merge");
    }
    
    first = ompi_comm_determine_first ( intercomm, high );
    if ( first ) {
        memcpy ( procs, intercomm->c_local_group->grp_proc_pointers, 
                 local_size * sizeof(ompi_proc_t *));
        memcpy ( &procs[local_size], intercomm->c_remote_group->grp_proc_pointers, 
                 remote_size * sizeof(ompi_proc_t *));
    }
    else {
        memcpy ( procs, intercomm->c_remote_group->grp_proc_pointers, 
                 remote_size * sizeof(ompi_proc_t *));
        memcpy ( &procs[remote_size], intercomm->c_local_group->grp_proc_pointers, 
                 local_size * sizeof(ompi_proc_t *));
    }

    newcomp = ompi_comm_set ( intercomm,                /* old comm */
                              total_size,               /* local_size */
                              procs,                    /* local_procs*/
                              0,                        /* remote_size */
                              NULL,                     /* remote_procs */
                              NULL,                     /* attrs */
                              intercomm->error_handler, /* error handler*/
                              NULL,                     /* coll module */
                              NULL                      /* topo mpodule */
                              );

    if ( newcomp == MPI_COMM_NULL ) {
        return OMPI_ERRHANDLER_INVOKE (intercomm, MPI_ERR_INTERN, 
                                       "MPI_Intercomm_merge");
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid ( newcomp,              /* new comm */ 
                             intercomm,            /* old comm */
                             NULL,                 /* bridge comm */
                             NULL,                 /* local leader */
                             NULL,                 /* remote_leader */
                             OMPI_COMM_CID_INTER); /* mode */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

 exit:
    if ( NULL != procs ) {
        free ( procs );
    }
    if ( OMPI_SUCCESS != rc ) {
        *newcomm = MPI_COMM_NULL;
        return OMPI_ERRHANDLER_INVOKE(intercomm, MPI_ERR_INTERN,
                                      "MPI_Intercom_merge");
    }

    *newcomm = newcomp;
    return MPI_SUCCESS;
}
