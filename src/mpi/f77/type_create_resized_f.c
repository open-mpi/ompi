/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TYPE_CREATE_RESIZED = mpi_type_create_resized_f
#pragma weak pmpi_type_create_resized = mpi_type_create_resized_f
#pragma weak pmpi_type_create_resized_ = mpi_type_create_resized_f
#pragma weak pmpi_type_create_resized__ = mpi_type_create_resized_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_RESIZED,
                           pmpi_type_create_resized,
                           pmpi_type_create_resized_,
                           pmpi_type_create_resized__,
                           pmpi_type_create_resized_f,
                           (MPI_Fint *oldtype, MPI_Fint *lb, MPI_Fint *extent, MPI_Fint *newtype, MPI_Fint *ierr),
                           (oldtype, lb, extent, newtype, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_RESIZED = mpi_type_create_resized_f
#pragma weak mpi_type_create_resized = mpi_type_create_resized_f
#pragma weak mpi_type_create_resized_ = mpi_type_create_resized_f
#pragma weak mpi_type_create_resized__ = mpi_type_create_resized_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_RESIZED,
                           mpi_type_create_resized,
                           mpi_type_create_resized_,
                           mpi_type_create_resized__,
                           mpi_type_create_resized_f,
                           (MPI_Fint *oldtype, MPI_Fint *lb, MPI_Fint *extent, MPI_Fint *newtype, MPI_Fint *ierr),
                           (oldtype, lb, extent, newtype, ierr) )
#endif

void mpi_type_create_resized_f(MPI_Fint *oldtype, MPI_Fint *lb, MPI_Fint *extent, MPI_Fint *newtype, MPI_Fint *ierr)
{

}
