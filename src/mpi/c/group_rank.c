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
#pragma weak MPI_Group_rank = PMPI_Group_rank
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Group_rank(MPI_Group group, int *rank) {

    /* error checking */
    if( MPI_PARAM_CHECK ) {
        if( (MPI_GROUP_NULL == group) || ( NULL == group) ){
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                        "MPI_Group_rank");
        }
    }

    *rank=lam_group_rank((lam_group_t *)group);

    return MPI_SUCCESS;
}
