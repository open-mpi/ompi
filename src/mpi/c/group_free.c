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
#pragma weak MPI_Group_free = PMPI_Group_free
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Group_free(MPI_Group *group)
{
    ompi_group_t *l_group;

    /* check to make sure we don't free GROUP_EMPTY or GROUP_NULL */
    if (MPI_PARAM_CHECK) {
        if ((MPI_GROUP_NULL == *group) || (MPI_GROUP_EMPTY == *group) ||
                (NULL == *group) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                        "MPI_Group_free");
        }
    }

    l_group = (ompi_group_t *) *group;

    /* check to see if group may be freed */
    if( false == l_group->grp_ok_to_free ) {
        return MPI_ERR_GROUP;
    }

    OBJ_RELEASE(l_group);

    *group = MPI_GROUP_NULL;

    return MPI_SUCCESS;
}
