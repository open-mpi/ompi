/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mca/pml/pml.h"
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
                         int tag, MPI_Comm *newintercomm) 
{
    int local_size, local_rank;
    lam_communicator_t *newcomp;
    lam_proc_t **rprocs=NULL;
    int rc, rsize;

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
    local_rank = lam_comm_rank ( local_comm );

    if ( MPI_PARAM_CHECK ) {
        if ( 0 < local_leader || local_leader > local_size ) 
            return LAM_ERRHANDLER_INVOKE ( local_comm, MPI_ERR_ARG, 
                                           "MPI_Intercomm_create");

        /* remember that the remote_leader and bridge_comm arguments
           just have to be valid at the local_leader */
        if ( local_rank == local_leader ) {
            if ( MPI_COMM_NULL == bridge_comm || lam_comm_invalid ( bridge_comm) ||
                 bridge_comm->c_flags & LAM_COMM_INTER ) {
                return LAM_ERRHANDLER_INVOKE ( local_comm, MPI_ERR_COMM, 
                                               "MPI_Intercomm_create");
            }            

            if ( remote_leader < 0 || remote_leader > lam_comm_size(bridge_comm)) {
                return LAM_ERRHANDLER_INVOKE ( local_comm, MPI_ERR_ARG,
                                               "MPI_Intercomm_create");
            }
        } /* if ( local_rank == local_leader ) */
    }

    if ( local_rank == local_leader ) {
        MPI_Request req;
        MPI_Status status;
        
        /* local leader exchange group sizes and vpid lists */
        rc =mca_pml.pml_irecv (&rsize, 1, MPI_INT, remote_leader, tag, bridge_comm,
                               &req );
        if ( rc != MPI_SUCCESS ) {
            goto err_exit;
        }
        rc = mca_pml.pml_send ( &local_size, 1, MPI_INT, remote_leader, tag,
                                MCA_PML_BASE_SEND_STANDARD, bridge_comm );
        if ( rc != MPI_SUCCESS ) {
            goto err_exit;
        }
        rc = mca_pml.pml_wait ( 1, &req, NULL, &status);
        if ( rc != MPI_SUCCESS ) {
            goto err_exit;
        }

    }

    /* bcast size and vpid lists to all processes in local_comm */
    rc = local_comm->c_coll.coll_bcast_intra ( &rsize, 1, MPI_INT, local_leader, 
                                               local_comm );
    if ( rc != MPI_SUCCESS ) {
        goto err_exit;
    }

    rprocs = lam_comm_get_rprocs ( local_comm, bridge_comm, local_leader,
                                   remote_leader, tag, rsize );
    newcomp = lam_comm_set ( LAM_COMM_INTRA_INTER,                         /* mode */
                             local_comm,                                   /* old comm */
                             bridge_comm,                                  /* bridge comm */
                             local_comm->c_local_group->grp_proc_count,    /* local_size */
                             local_comm->c_local_group->grp_proc_pointers, /* local_procs*/
                             rsize,                                        /* remote_size */
                             rprocs,                                       /* remote_procs */
                             NULL,                                         /* attrs */
                             local_comm->error_handler,                    /* error handler*/
                             NULL,                                         /* coll module */
                             NULL,                                         /* topo mpodule */
                             local_leader,                                 /* local leader */
                             remote_leader                                 /* remote leader */
                             );

    if ( newcomp == MPI_COMM_NULL ) {
        return LAM_ERRHANDLER_INVOKE (local_comm, MPI_ERR_INTERN, "MPI_Intercomm_create");
    }

 err_exit:
    if ( NULL == rprocs ) {
        free ( rprocs );
    }

    *newintercomm = newcomp;
    return MPI_SUCCESS;
}
