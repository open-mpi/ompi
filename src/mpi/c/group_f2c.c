/*
 * $HEADERS$
 */

#include "ompi_config.h"
#include "mpi.h"
#include "mpi/c/bindings.h"
#include "group/group.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Group_f2c = PMPI_Group_f2c
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Group_f2c";


MPI_Group MPI_Group_f2c(MPI_Fint group_f)
{
    /* local variables */
    ompi_group_t *group_c;
    size_t group_index;

    group_index = (size_t) group_f;

    /* error checks */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (0 > group_index) {
            (void) OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                                          FUNC_NAME);
            return MPI_GROUP_NULL;
        }
        if (group_index >= ompi_group_f_to_c_table->size) {
            (void) OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                                          FUNC_NAME);
            return MPI_GROUP_NULL;
        }
    }

    group_c = ompi_group_f_to_c_table->addr[group_index];
    return (MPI_Group) group_c;
}
