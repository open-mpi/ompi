/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_UB = mpi_type_ub_f
#pragma weak pmpi_type_ub = mpi_type_ub_f
#pragma weak pmpi_type_ub_ = mpi_type_ub_f
#pragma weak pmpi_type_ub__ = mpi_type_ub_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_UB,
                           pmpi_type_ub,
                           pmpi_type_ub_,
                           pmpi_type_ub__,
                           pmpi_type_ub_f,
                           (MPI_Fint *mtype, MPI_Fint *ub, MPI_Fint *ierr),
                           (mtype, ub, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_UB = mpi_type_ub_f
#pragma weak mpi_type_ub = mpi_type_ub_f
#pragma weak mpi_type_ub_ = mpi_type_ub_f
#pragma weak mpi_type_ub__ = mpi_type_ub_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_UB,
                           mpi_type_ub,
                           mpi_type_ub_,
                           mpi_type_ub__,
                           mpi_type_ub_f,
                           (MPI_Fint *mtype, MPI_Fint *ub, MPI_Fint *ierr),
                           (mtype, ub, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_type_ub_f(MPI_Fint *mtype, MPI_Fint *ub, MPI_Fint *ierr)
{
    MPI_Datatype c_mtype = MPI_Type_f2c(*mtype);
    MPI_Aint c_ub;

    *ierr = MPI_Type_ub(c_mtype, &c_ub);

    if (*ierr == MPI_SUCCESS)
      *ub = (MPI_Fint)c_ub;
}
