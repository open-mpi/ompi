/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/f77/bindings.h"
#include "group/group.h"


#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GROUP_COMPARE = mpi_group_compare_f
#pragma weak pmpi_group_compare = mpi_group_compare_f
#pragma weak pmpi_group_compare_ = mpi_group_compare_f
#pragma weak pmpi_group_compare__ = mpi_group_compare_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_COMPARE,
                           pmpi_group_compare,
                           pmpi_group_compare_,
                           pmpi_group_compare__,
                           pmpi_group_compare_f,
                           (MPI_Fint *group1, MPI_Fint *group2,
                            MPI_Fint *result, MPI_Fint *ierror),
                           (group1,group2,result,ierror))
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_COMPARE = mpi_group_compare_f
#pragma weak mpi_group_compare = mpi_group_compare_f
#pragma weak mpi_group_compare_ = mpi_group_compare_f
#pragma weak mpi_group_compare__ = mpi_group_compare_f
#endif

#if ! OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_COMPARE,
                           mpi_group_compare,
                           mpi_group_compare_,
                           mpi_group_compare__,
                           mpi_group_compare_f,
                           (MPI_Fint *group1, MPI_Fint *group2,
                            MPI_Fint *result, MPI_Fint *ierror),
                           (group1,group2,result,ierror))
#endif



#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_group_compare_f(MPI_Fint *group1, MPI_Fint *group2,
                         MPI_Fint *result, MPI_Fint *ierr)
{
    ompi_group_t *c_group1, *c_group2;
    OMPI_SINGLE_NAME_DECL(result);

    /* make the fortran to c representation conversion */
    c_group1 = MPI_Group_f2c(*group1);
    c_group2 = MPI_Group_f2c(*group2);

    *ierr = OMPI_INT_2_FINT(MPI_Group_compare(c_group1, c_group2, 
					      OMPI_SINGLE_NAME_CONVERT(result)
					      ));
    OMPI_SINGLE_INT_2_FINT(result);
}
