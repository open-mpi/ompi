/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>
#include <string.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_dup = PMPI_Comm_dup
#endif

int MPI_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm) {
    
    /* local variables */
    lam_communicator_t *comp, *newcomp;
    int rc;
    
    /* argument checking */
    if ( MPI_PARAM_CHECK ) {
        if (lam_mpi_finalized)  
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Comm_dup");
        
        if (MPI_COMM_NULL == comm || lam_comm_invalid (comm))
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_dup");
        
        if ( NULL == newcomm )
            return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                         "MPI_Comm_dup");
    }

    comp = (lam_communicator_t *) comm;
    /* This routine allocates an element, allocates the according groups,
       sets the f2c handle and increases the reference counters of
       comm, group and remote_group */
    newcomp = lam_comm_allocate (comp->c_local_group->grp_proc_count, 
                                 comp->c_remote_group->grp_proc_count );

    /* copy local group */
    newcomp->c_local_group->grp_my_rank = comp->c_local_group->grp_my_rank;
    memcpy (newcomp->c_local_group->grp_proc_pointers, 
            comp->c_local_group->grp_proc_pointers, 
            comp->c_local_group->grp_proc_count * sizeof(lam_proc_t *));
    lam_group_increment_proc_count(newcomp->c_local_group);

    if ( comp->c_flags & LAM_COMM_INTER ) {
        /* copy remote group */
        memcpy (newcomp->c_remote_group->grp_proc_pointers, 
                comp->c_remote_group->grp_proc_pointers, 
                comp->c_remote_group->grp_proc_count * sizeof(lam_proc_t *));
        lam_group_increment_proc_count(newcomp->c_remote_group);

        /* Get new context id */
        newcomp->c_contextid = lam_comm_nextcid (comm, LAM_COMM_INTER_INTER);
    }
    else {
        /* Get new context id */
        newcomp->c_contextid = lam_comm_nextcid (comm, LAM_COMM_INTRA_INTRA);
    }

    /* other fields */
    newcomp->c_my_rank = comp->c_my_rank;
    newcomp->c_flags   = comp->c_flags;
    

    /* Copy topology information */


    /* Copy error handler */
    newcomp->error_handler = comp->error_handler;
    OBJ_RETAIN ( comp->error_handler );

    /* Copy attributes */
    rc = lam_attr_copy_all ( COMM_ATTR, comp, newcomp );
    if ( rc != LAM_SUCCESS ) {
        lam_comm_free ( (MPI_Comm *)newcomp );
        return LAM_ERRHANDLER_INVOKE ( comm, rc, "MPI_Comm_dup");
    }

    return MPI_SUCCESS;
}
