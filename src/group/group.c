/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "group/group.h"
#include "include/constants.h"
#include "mpi.h"

int ompi_group_translate_ranks ( ompi_group_t *group1, 
                                 int n_ranks, int *ranks1,
                                 ompi_group_t *group2, 
                                 int *ranks2) 
{
    int rank, proc, proc2;
    struct ompi_proc_t *proc1_pointer, *proc2_pointer;
    
    /* loop over all ranks */
    for (proc = 0; proc < n_ranks; proc++) {
        rank=ranks1[proc];
        proc1_pointer=group1->grp_proc_pointers[rank];
        /* initialize to no "match" */
        ranks2[proc] = MPI_UNDEFINED;
        for (proc2 = 0; proc2 < group2->grp_proc_count; proc2++) 
        {
            proc2_pointer=group2->grp_proc_pointers[proc2];
            if ( proc1_pointer == proc2_pointer) {
                ranks2[proc] = proc2;
                break;
            }
        }  /* end proc2 loop */
    } /* end proc loop */

    return MPI_SUCCESS;
}
