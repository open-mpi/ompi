/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TYPE_CREATE_F90_REAL = mpi_type_create_f90_real_f
#pragma weak pmpi_type_create_f90_real = mpi_type_create_f90_real_f
#pragma weak pmpi_type_create_f90_real_ = mpi_type_create_f90_real_f
#pragma weak pmpi_type_create_f90_real__ = mpi_type_create_f90_real_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_F90_REAL,
                           pmpi_type_create_f90_real,
                           pmpi_type_create_f90_real_,
                           pmpi_type_create_f90_real__,
                           pmpi_type_create_f90_real_f,
                           (MPI_Fint *p, MPI_Fint *r, MPI_Fint *newtype, MPI_Fint *ierr),
                           (p, r, newtype, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_F90_REAL = mpi_type_create_f90_real_f
#pragma weak mpi_type_create_f90_real = mpi_type_create_f90_real_f
#pragma weak mpi_type_create_f90_real_ = mpi_type_create_f90_real_f
#pragma weak mpi_type_create_f90_real__ = mpi_type_create_f90_real_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_F90_REAL,
                           mpi_type_create_f90_real,
                           mpi_type_create_f90_real_,
                           mpi_type_create_f90_real__,
                           mpi_type_create_f90_real_f,
                           (MPI_Fint *p, MPI_Fint *r, MPI_Fint *newtype, MPI_Fint *ierr),
                           (p, r, newtype, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_create_f90_real_f(MPI_Fint *p, MPI_Fint *r, MPI_Fint *newtype, MPI_Fint *ierr)
{

}
