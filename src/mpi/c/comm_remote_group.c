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
#pragma weak MPI_Comm_remote_group = PMPI_Comm_remote_group
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


int MPI_Comm_remote_group(MPI_Comm comm, MPI_Group *group) {
    
    lam_communicator_t *comp;
    lam_group_t *group_p;

    if ( MPI_PARAM_CHECK ) {
        if ( lam_mpi_finalized )
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Comm_remote_group");

        if (MPI_COMM_NULL == comm || lam_comm_invalid (comm))
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_remote_group");

        if ( NULL == group )
            return LAM_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                         "MPI_Comm_remote_group");
    }

    comp = (lam_communicator_t *) comm;
    if ( comp->c_flags & LAM_COMM_INTER ) {        
        /* get new group struct */
        group_p=lam_group_allocate(comp->c_remote_group->grp_proc_count);
        if( NULL == group_p ) {
            return LAM_ERRHANDLER_INVOKE (comm, MPI_ERR_INTERN, 
                                          "MPI_Comm_remote_group");
        }

        group_p->grp_my_rank = MPI_UNDEFINED;
        memcpy ( group_p->grp_proc_pointers, 
                 comp->c_remote_group->grp_proc_pointers,
                 group_p->grp_proc_count * sizeof ( lam_proc_t *));
        /* increment proc reference counters */
        lam_group_increment_proc_count(group_p);
    }
    else
        return LAM_ERRHANDLER_INVOKE (comm, MPI_ERR_COMM, 
                                      "MPI_Comm_remote_group");

    *group = (MPI_Group) group_p;
    return MPI_SUCCESS;
}
