/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "group/group.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_size = PMPI_Group_size
#endif

int MPI_Group_size(MPI_Group group, int *size) {

    /* error checking */
    if( MPI_PARAM_CHECK ) {
        if( MPI_GROUP_NULL == group ) {
            return MPI_ERR_GROUP;
        }
    }

    *size=lam_group_size((lam_group_t *)group);
    return MPI_SUCCESS;
}
