/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "group/group.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_excl = PMPI_Group_excl
#endif

int MPI_Group_excl(MPI_Group group, int n, int *ranks,
                   MPI_Group *new_group) {

    /* local variables */
    int return_value,proc,i_excl,found,excl_proc,cnt,my_group_rank;
    lam_group_t *group_pointer, *new_group_pointer;
    lam_proc_t *my_proc_pointer;

    return_value = MPI_SUCCESS;
    group_pointer = (lam_group_t *)group;

    if( MPI_PARAM_CHECK ) {
        /* verify that group is valid group */
        if ( MPI_GROUP_NULL == group  || NULL == ranks ) {
            return MPI_ERR_GROUP;
        }

        /* check that new group is no larger than old group */
        if ( n > group_pointer->grp_proc_count) {
            return MPI_ERR_RANK;
        }

    }  /* end if( MPI_CHECK_ARGS) */

    /*
     * pull out elements
     */

    /* get new group struct */
    new_group_pointer=group_allocate(group_pointer->grp_proc_count-n);
    if( NULL == new_group_pointer ) {
        return MPI_ERR_GROUP;
    }

    /* put group elements in the list */
    cnt=0;
    for (proc = 0; proc < n; proc++) {
        found=0;
        /* check to see if this proc is in the exclusion list */
        for( i_excl=0 ; i_excl < n ; ++i_excl ) {
            excl_proc=ranks[i_excl];
            /* check to see if this proc is within range */
            if( ( 0 > excl_proc ) ||
                (excl_proc >= group_pointer->grp_proc_count)){
                    return MPI_ERR_RANK;
                }
            if(excl_proc == proc ){
                found=1;
                break;
            }

        } /* end i_excl loop */
        if( !found ) {
            new_group_pointer->grp_proc_pointers[cnt] =
                group_pointer->grp_proc_pointers[proc];
            cnt++;
        }

    }   /* end proc loop */

    /* increment proc reference counters */
    lam_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank=group_pointer->grp_my_rank;
    my_proc_pointer=group_pointer->grp_proc_pointers[my_group_rank];
    lam_set_group_rank(new_group_pointer,my_proc_pointer);

    *new_group = (MPI_Group)new_group_pointer;

    return MPI_SUCCESS;
}
