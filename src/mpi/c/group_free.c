/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "group/group.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_free = PMPI_Group_free
#endif

int MPI_Group_free(MPI_Group *group)
{
    int proc;
    lam_group_t *l_group;

    /* check to make sure we don't free GROUP_EMPTY or GROUP_NULL */
    if (MPI_PARAM_CHECK) {
        if ((MPI_GROUP_NULL == *group) || (MPI_GROUP_EMPTY == *group)) {
            return MPI_ERR_GROUP;
        }
    }

    l_group = (lam_group_t *) *group;

    /* decrement proc reference count */
    for (proc = 0; proc < l_group->grp_proc_count; proc++) {
        OBJ_RELEASE(l_group->grp_proc_pointers[proc]);
    }

    OBJ_RELEASE(l_group);

    *group = MPI_GROUP_NULL;

    return MPI_SUCCESS;
}
