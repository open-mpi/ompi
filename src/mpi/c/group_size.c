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
#pragma weak MPI_Group_size = PMPI_Group_size
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Group_size(MPI_Group group, int *size) {

    /* error checking */
    if( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE; 

        if( (MPI_GROUP_NULL == group) || (NULL == group) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                        "MPI_Group_size");
        }
    }

    *size=ompi_group_size((ompi_group_t *)group);

    return MPI_SUCCESS;
}
