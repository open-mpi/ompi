/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "mpi/group/group.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_intersection = PMPI_Group_intersection
#endif

int MPI_Group_intersection(MPI_Group group1, MPI_Group group2,
        MPI_Group *new_group) {

    /* local variables */
    int return_value;
    int group_size,proc,proc1,proc2,cnt;
    lam_group_t *group1_pointer, *group2_pointer, *new_group_pointer;
    lam_proc_t *proc1_pointer, *proc2_pointer;

    /* initialize data */
    return_value = MPI_SUCCESS;

    /* verify that groups are valid */
    if ( (NULL == group1) || (NULL == group2) ) {
        return MPI_ERR_GROUP;
    }

    group1_pointer=(lam_group_t *)group1;
    group2_pointer=(lam_group_t *)group2;

    /*
     * Check for empty group
     */
    if ((group1_pointer->grp_proc_count == 0) || 
            (group2_pointer->grp_proc_count == 0)) {
        *new_group = MPI_GROUP_EMPTY;
    }

    /*
     * form intersection
     */

    /* figure out how large the intersection is */
    group_size = 0;
    /* loop over group1 members */
    for (proc1 = 0; proc1 < group1_pointer->grp_proc_count; proc1++) {
        proc1_pointer=group1_pointer->grp_proc_pointers[proc1];
        /* check to see if this proc is in group2 */
    
        for (proc2 = 0; proc2 < group2_pointer->grp_proc_count; proc2++) {
            proc2_pointer=group2_pointer->grp_proc_pointers[proc2];
            if( proc1_pointer == proc2_pointer ) {
                group_size++;
                break;
            }
        }  /* end proc2 loop */
    }  /* end proc1 loop */

    if (group_size == 0) {
        /* empty group */
        *new_group = MPI_GROUP_EMPTY;
    } else {
        /* fill in new group */
        new_group_pointer=group_allocate(group_size);
        if( NULL == new_group_pointer ) {
            return MPI_ERR_GROUP;
        }

        cnt = 0;

        /* loop over group1 members */

        for (proc1 = 0; proc1 < group1_pointer->grp_proc_count; proc1++) {
            proc1_pointer=group1_pointer->grp_proc_pointers[proc1];
            /* check to see if this proc is in group2 */
            for (proc2 = 0; proc2 < group2_pointer->grp_proc_count; proc2++) {
                proc2_pointer=group2_pointer->grp_proc_pointers[proc2];
                if( proc1_pointer == proc2_pointer ) {
                    new_group_pointer->grp_proc_pointers[cnt]=proc1_pointer;
                    cnt++;
                    break;
                }
            }  /* end proc2 loop */
        }  /* end proc1 loop */
    }  /* end else */

    /* find my rank */
    new_group_pointer->grp_my_rank = MPI_PROC_NULL;
    if( MPI_PROC_NULL != group1_pointer->grp_my_rank) {
        for ( proc=0 ; proc < group_size ; proc++ ){
            if( new_group_pointer->grp_proc_pointers[proc] == 
                    group1_pointer->grp_proc_pointers
                    [group1_pointer->grp_my_rank]){
                new_group_pointer->grp_my_rank = proc;
            }
        }
    }

    *new_group = (MPI_Group)new_group_pointer;

    return return_value;
}
