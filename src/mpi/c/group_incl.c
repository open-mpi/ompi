/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "group/group.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_incl = PMPI_Group_incl
#endif

int MPI_Group_incl(MPI_Group group, int n, int *ranks, MPI_Group *new_group) 
{
    /* local variables */
    int return_value,proc,my_group_rank;
    lam_group_t *group_pointer, *new_group_pointer;
    lam_proc_t *my_proc_pointer;

    return_value = MPI_SUCCESS;
    group_pointer = (lam_group_t *)group;

    if( MPI_PARAM_CHECK ) {

        /* verify that group is valid group */
        if ( MPI_GROUP_NULL == group || NULL == ranks ) {
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

    /* increment proc reference counters */
    lam_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank=group_pointer->grp_my_rank;
    my_proc_pointer=group_pointer->grp_proc_pointers[my_group_rank];
    lam_set_group_rank(new_group_pointer,my_proc_pointer);

    *new_group = (MPI_Group)new_group_pointer;

    return MPI_SUCCESS;
}
