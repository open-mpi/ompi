/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TYPE_STRUCT = mpi_type_struct_f
#pragma weak pmpi_type_struct = mpi_type_struct_f
#pragma weak pmpi_type_struct_ = mpi_type_struct_f
#pragma weak pmpi_type_struct__ = mpi_type_struct_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TYPE_STRUCT,
                           pmpi_type_struct,
                           pmpi_type_struct_,
                           pmpi_type_struct__,
                           pmpi_type_struct_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *array_of_types, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, array_of_types, newtype, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_STRUCT = mpi_type_struct_f
#pragma weak mpi_type_struct = mpi_type_struct_f
#pragma weak mpi_type_struct_ = mpi_type_struct_f
#pragma weak mpi_type_struct__ = mpi_type_struct_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TYPE_STRUCT,
                           mpi_type_struct,
                           mpi_type_struct_,
                           mpi_type_struct__,
                           mpi_type_struct_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *array_of_types, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, array_of_types, newtype, ierr) )
#endif

void mpi_type_struct_f(MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *array_of_types, MPI_Fint *newtype, MPI_Fint *ierr)
{

}
