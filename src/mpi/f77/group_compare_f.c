/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/f77/bindings.h"
#include "mpi/group/group.h"


#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GROUP_COMPARE = mpi_group_compare_f
#pragma weak pmpi_group_compare = mpi_group_compare_f
#pragma weak pmpi_group_compare_ = mpi_group_compare_f
#pragma weak pmpi_group_compare__ = mpi_group_compare_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GROUP_COMPARE,
        pmpi_group_compare,
        pmpi_group_compare_,
        pmpi_group_compare__,
        pmpi_group_compare_f,
        (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *result, MPI_Fint *ierror),
        (group1,group2,result,ierror))
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INIT = mpi_group_compare_f
#pragma weak mpi_init = mpi_group_compare_f
#pragma weak mpi_init_ = mpi_group_compare_f
#pragma weak mpi_init__ = mpi_group_compare_f
#endif

#if ! LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
LAM_GENERATE_F77_BINDINGS (PMPI_GROUP_COMPARE,
        pmpi_group_compare,
        pmpi_group_compare_,
        pmpi_group_compare__,
        pmpi_group_compare_f,
        (MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *result, MPI_Fint *ierror),
        (group1,group2,result,ierror))
#endif


void
mpi_group_compare_f(MPI_Fint *group1, MPI_Fint *group2, MPI_Fint *result,
        MPI_Fint *ierror)
{
    /* local variables */
    int c_result, c_error;
    lam_group_t *c_group1, *c_group2;

    /* make the fortran to c representation conversion */
    c_group1=group_f2c(*group1);
    c_group2=group_f2c(*group2);

    c_error = MPI_Group_compare(c_group1, c_group2, &c_result);

    /* translate the results from c to fortran */
    *ierror=(MPI_Fint)c_error;
    *result=(MPI_Fint)c_result;
}
