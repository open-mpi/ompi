/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "mpi/group/group.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_c2f = PMPI_Group_c2f
#endif

MPI_Fint MPI_Group_c2f(MPI_Group group) {

    /* local variables */
    lam_group_t *group_c;

    /* error checking */
    if( MPI_PARAM_CHECK ) {
        /* check for MPI_GROUP_NULL */
        if( NULL == group ) {
            return (MPI_Fint) MPI_ERR_GROUP;
        }
    }

    group_c=(lam_group_t *)group;

    return (MPI_Fint) (group_c->grp_f_to_c_index) ;
}
