/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "mpi/group/group.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_rank = PMPI_Group_rank
#endif

int MPI_Group_rank(MPI_Group group, int *rank) {

    /* error checking */
    if( MPI_PARAM_CHECK ) {
        if( MPI_GROUP_NULL == group ){
            return MPI_ERR_GROUP;
        }
    }

    *rank=lam_group_rank((lam_group_t *)group);

    return MPI_SUCCESS;
}
