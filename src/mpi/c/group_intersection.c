/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "group/group.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_intersection = PMPI_Group_intersection
#endif

int MPI_Group_intersection(MPI_Group group1, MPI_Group group2,
        MPI_Group *new_group) {

    /* local variables */
    int my_group_rank;
    int group_size,proc1,proc2,cnt;
    lam_group_t *group1_pointer, *group2_pointer, *new_group_pointer;
    lam_proc_t *proc1_pointer, *proc2_pointer, *my_proc_pointer;

    /* verify that groups are valid */
    if ( (MPI_GROUP_NULL == group1) || (MPI_GROUP_NULL == group2) ||
            ( NULL == group1) || (NULL == group2) ) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                        "MPI_Group_intersection");
    }

    group1_pointer=(lam_group_t *)group1;
    group2_pointer=(lam_group_t *)group2;

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

    /* fill in new group */
    new_group_pointer=lam_group_allocate(group_size);
    if( NULL == new_group_pointer ) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                        "MPI_Group_intersection - II");
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

    /* increment proc reference counters */
    lam_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank=group1_pointer->grp_my_rank;
    my_proc_pointer=group1_pointer->grp_proc_pointers[my_group_rank];
    lam_set_group_rank(new_group_pointer,my_proc_pointer);

    *new_group = (MPI_Group)new_group_pointer;

    return MPI_SUCCESS;
}
