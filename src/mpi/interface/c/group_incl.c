/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "mpi/group/group.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_incl = PMPI_Group_incl
#endif

int MPI_Group_incl(MPI_Group group, int n, int *ranks, MPI_Group *new_group) 
{
    /* local variables */
    int return_value,proc;
    lam_group_t *group_pointer, *new_group_pointer;

    return_value = MPI_SUCCESS;
    group_pointer = (lam_group_t *)group;

    if( MPI_PARAM_CHECK ) {
        /* including anything of the empty group is still the empty group */
        if ((group == MPI_GROUP_EMPTY) || (n == 0)) {
            *new_group = MPI_GROUP_EMPTY;
            return return_value;
        }

        /* verify that group is valid group */
        if ( NULL == group || NULL == ranks ) {
            return MPI_ERR_GROUP;
        }

        /* check that new group is no larger than old group */
        if ( n > group_pointer->grp_proc_count) {
            return MPI_ERR_RANK;
        }

    }  /* end if( MPI_CHECK_ARGS) */


    /* get new group struct */
    new_group_pointer=group_allocate(n);
    if( NULL == new_group_pointer ) {
        return MPI_ERR_GROUP;
    }

    /* put group elements in the list */
    for (proc = 0; proc < n; proc++) {
        if ((ranks[proc] < 0) ||
                (ranks[proc] >= group_pointer->grp_proc_count)){
            return MPI_ERR_RANK;
        }

        new_group_pointer->grp_proc_pointers[proc] =
            group_pointer->grp_proc_pointers[ranks[proc]];

    }                           /* end proc loop */

    /* find my rank */
    new_group_pointer->grp_my_rank = MPI_PROC_NULL;
    if( MPI_PROC_NULL != group_pointer->grp_my_rank) {
        for ( proc=0 ; proc < n ; proc++ ){
            if( new_group_pointer->grp_proc_pointers[proc] == 
                    group_pointer->grp_proc_pointers
                    [group_pointer->grp_my_rank]){
                new_group_pointer->grp_my_rank = proc;
            }
        }
    }

    *new_group = (MPI_Group)new_group_pointer;

    return MPI_SUCCESS;
}
