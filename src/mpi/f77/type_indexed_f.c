/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_INDEXED = mpi_type_indexed_f
#pragma weak pmpi_type_indexed = mpi_type_indexed_f
#pragma weak pmpi_type_indexed_ = mpi_type_indexed_f
#pragma weak pmpi_type_indexed__ = mpi_type_indexed_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_INDEXED,
                           pmpi_type_indexed,
                           pmpi_type_indexed_,
                           pmpi_type_indexed__,
                           pmpi_type_indexed_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_INDEXED = mpi_type_indexed_f
#pragma weak mpi_type_indexed = mpi_type_indexed_f
#pragma weak mpi_type_indexed_ = mpi_type_indexed_f
#pragma weak mpi_type_indexed__ = mpi_type_indexed_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_INDEXED,
                           mpi_type_indexed,
                           mpi_type_indexed_,
                           mpi_type_indexed__,
                           mpi_type_indexed_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_indexed_f(MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr)
{
    MPI_Datatype c_old = MPI_Type_f2c(*oldtype);
    MPI_Datatype c_new;

    *ierr = MPI_Type_indexed(*count, array_of_blocklengths, 
                             array_of_displacements, c_old, &c_new);

    if (*ierr == MPI_SUCCESS)
      *newtype = MPI_Type_c2f(c_new);
}
