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
#pragma weak MPI_Group_size = PMPI_Group_size
#endif

int MPI_Group_size(MPI_Group group, int *size) {

    /* error checking */
    if( MPI_PARAM_CHECK ) {
        if( (MPI_GROUP_NULL == group) || (NULL == group) ) {
            return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                        "MPI_Group_size");
        }
    }

    *size=lam_group_size((lam_group_t *)group);

    return MPI_SUCCESS;
}
