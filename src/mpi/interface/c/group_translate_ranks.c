/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "mpi/group/group.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_translate_ranks = PMPI_Group_translate_ranks
#endif

int MPI_Group_translate_ranks(MPI_Group group1, int n_ranks, int *ranks1,
                              MPI_Group group2, int *ranks2) {

    int i, rank, proc, proc2;
    lam_proc_t *proc1_pointer, *proc2_pointer;
    lam_group_t *group1_pointer, *group2_pointer;

    group1_pointer=(lam_group_t *)group1;
    group2_pointer=(lam_group_t *)group2;

    /* return an error if the source group is the empty group... */
    if (MPI_GROUP_EMPTY == group1) {
        return MPI_ERR_GROUP;
    }

    /* if group 2 is empty, fill the rank list with MPI_UNDEFINED */
    if ( MPI_GROUP_EMPTY == group2) {
        for (i = 0; i < n_ranks; i++) {
            ranks2[i] = MPI_UNDEFINED;
        }
        return MPI_SUCCESS;
    }

    /* loop over all ranks */
    for (proc = 0; proc < n_ranks; proc++) {
        rank=ranks1[proc];
        proc1_pointer=group1_pointer->grp_proc_pointers[rank];
        /* initialize to no "match" */
        ranks2[proc] = MPI_UNDEFINED;
        for (proc2 = 0; proc2 < group2_pointer->grp_proc_count; proc2++) 
        {
            proc2_pointer=group2_pointer->grp_proc_pointers[proc2];
            if ( proc1_pointer == proc2_pointer) {
                ranks2[proc] = proc2;
                break;
            }
        }  /* end proc2 loop */
    } /* end proc loop */

    return MPI_SUCCESS;
}
