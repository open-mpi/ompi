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
#pragma weak MPI_Group_c2f = PMPI_Group_c2f
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

MPI_Fint MPI_Group_c2f(MPI_Group group) {

    /* local variables */
    ompi_group_t *group_c;

    /* error checking */
    if( MPI_PARAM_CHECK ) {
        /* check for MPI_GROUP_NULL */
        if( (NULL == group) ) {
            return (MPI_Fint) 
                OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                        "MPI_Group_c2f");
        }
    }

    group_c=(ompi_group_t *)group;

    return (MPI_Fint) (group_c->grp_f_to_c_index) ;
}
