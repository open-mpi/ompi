/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "group/group.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Group_translate_ranks = PMPI_Group_translate_ranks
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Group_translate_ranks";


int MPI_Group_translate_ranks(MPI_Group group1, int n_ranks, int *ranks1,
                              MPI_Group group2, int *ranks2) {

    int rank, proc, proc2;
    ompi_proc_t *proc1_pointer, *proc2_pointer;
    ompi_group_t *group1_pointer, *group2_pointer;

    group1_pointer=(ompi_group_t *)group1;
    group2_pointer=(ompi_group_t *)group2;

    /* check for errors */
    if( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ((MPI_GROUP_NULL == group1) || (MPI_GROUP_NULL == group2) ||
                (NULL == group1) || (NULL == group2) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP, 
                                          FUNC_NAME);
        }
        if( (n_ranks > group1_pointer->grp_proc_count) || (0 >= n_ranks) ){
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP, 
                                          FUNC_NAME);
        }
        if( (NULL == ranks1) || (NULL == ranks2 ) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP, 
                                          FUNC_NAME);
        }
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
