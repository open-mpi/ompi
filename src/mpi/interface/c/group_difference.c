/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "mpi/group/group.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_difference = PMPI_Group_difference
#endif

int MPI_Group_difference(MPI_Group group1, MPI_Group group2,
                         MPI_Group *new_group) {

    /* local varibles */
    int return_value, new_group_size, proc1, proc2, found_in_group2, cnt;
    lam_group_t *group1_pointer, *group2_pointer, *new_group_pointer;
    lam_proc_t *proc1_pointer, *proc2_pointer;

    return_value=MPI_SUCCESS;
    group1_pointer=(lam_group_t *)group1;
    group2_pointer=(lam_group_t *)group2;

    /* the difference of an empty group and anything is an empty group */
    if (MPI_GROUP_EMPTY == group1 ) {
        *new_group = MPI_GROUP_EMPTY;
        return MPI_SUCCESS;
    }

    /* the difference of a group with the empty group is the group itself */
    if (MPI_GROUP_EMPTY == group2 ) {
        *new_group=group1;
        OBJ_RETAIN(group1_pointer);
        return MPI_SUCCESS;
    }

    /*
     * form union
     */

    /* get new group size */
    new_group_size=0;

    /* loop over group1 members */
    for( proc1=0; proc1 < group1_pointer->grp_proc_count; proc1++ ) {
        proc1_pointer=group1_pointer->grp_proc_pointers[proc1];
        /* check to see if this proc is in group2 */
        found_in_group2=0;
        for( proc2=0 ; proc2 < group2_pointer->grp_proc_count ; proc2++ ) {
            proc2_pointer=group2_pointer->grp_proc_pointers[proc2];
            if( proc1_pointer == proc2_pointer ) {
                found_in_group2=true;
                break;
            }
        }  /* end proc1 loop */
        if(found_in_group2)
            continue;
        new_group_size++;
    }  /* end proc loop */

    if (new_group_size == 0) {
        *new_group = MPI_GROUP_EMPTY;
        return MPI_SUCCESS;
    }

    /* allocate a new lam_group_t structure */
    new_group_pointer=group_allocate(new_group_size);
    if( NULL == new_group_pointer ) {
        return MPI_ERR_GROUP;
    }

    /* fill in group list */
    cnt=0;
    /* loop over group1 members */
    for( proc1=0; proc1 < group1_pointer->grp_proc_count; proc1++ ) {
        proc1_pointer=group1_pointer->grp_proc_pointers[proc1];
        /* check to see if this proc is in group2 */
        found_in_group2=0;
        for( proc2=0 ; proc2 < group2_pointer->grp_proc_count ; proc2++ ) {
            proc2_pointer=group2_pointer->grp_proc_pointers[proc2];
            if( proc1_pointer == proc2_pointer ) {
                found_in_group2=true;
                break;
            }
        }  /* end proc1 loop */
        if(found_in_group2)
            continue;

        new_group_pointer->grp_proc_pointers[cnt] =
            group1_pointer->grp_proc_pointers[proc1];

        cnt++;
    }  /* end proc loop */

    return return_value;
}
