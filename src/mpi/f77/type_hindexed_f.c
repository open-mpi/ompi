/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TYPE_HINDEXED = mpi_type_hindexed_f
#pragma weak pmpi_type_hindexed = mpi_type_hindexed_f
#pragma weak pmpi_type_hindexed_ = mpi_type_hindexed_f
#pragma weak pmpi_type_hindexed__ = mpi_type_hindexed_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TYPE_HINDEXED,
                           pmpi_type_hindexed,
                           pmpi_type_hindexed_,
                           pmpi_type_hindexed__,
                           pmpi_type_hindexed_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_HINDEXED = mpi_type_hindexed_f
#pragma weak mpi_type_hindexed = mpi_type_hindexed_f
#pragma weak mpi_type_hindexed_ = mpi_type_hindexed_f
#pragma weak mpi_type_hindexed__ = mpi_type_hindexed_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TYPE_HINDEXED,
                           mpi_type_hindexed,
                           mpi_type_hindexed_,
                           mpi_type_hindexed__,
                           mpi_type_hindexed_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) )
#endif

void mpi_type_hindexed_f(MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr)
{

}
