/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TYPE_UB = mpi_type_ub_f
#pragma weak pmpi_type_ub = mpi_type_ub_f
#pragma weak pmpi_type_ub_ = mpi_type_ub_f
#pragma weak pmpi_type_ub__ = mpi_type_ub_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TYPE_UB,
                           pmpi_type_ub,
                           pmpi_type_ub_,
                           pmpi_type_ub__,
                           pmpi_type_ub_f,
                           (MPI_Fint *mtype, MPI_Fint *ub, MPI_Fint *ierr),
                           (mtype, ub, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_UB = mpi_type_ub_f
#pragma weak mpi_type_ub = mpi_type_ub_f
#pragma weak mpi_type_ub_ = mpi_type_ub_f
#pragma weak mpi_type_ub__ = mpi_type_ub_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TYPE_UB,
                           mpi_type_ub,
                           mpi_type_ub_,
                           mpi_type_ub__,
                           mpi_type_ub_f,
                           (MPI_Fint *mtype, MPI_Fint *ub, MPI_Fint *ierr),
                           (mtype, ub, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_ub_f(MPI_Fint *mtype, MPI_Fint *ub, MPI_Fint *ierr)
{

}
