/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_MATCH_SIZE = mpi_type_match_size_f
#pragma weak pmpi_type_match_size = mpi_type_match_size_f
#pragma weak pmpi_type_match_size_ = mpi_type_match_size_f
#pragma weak pmpi_type_match_size__ = mpi_type_match_size_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_MATCH_SIZE,
                           pmpi_type_match_size,
                           pmpi_type_match_size_,
                           pmpi_type_match_size__,
                           pmpi_type_match_size_f,
                           (MPI_Fint *typeclass, MPI_Fint *size, MPI_Fint *type, MPI_Fint *ierr),
                           (typeclass, size, type, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_MATCH_SIZE = mpi_type_match_size_f
#pragma weak mpi_type_match_size = mpi_type_match_size_f
#pragma weak mpi_type_match_size_ = mpi_type_match_size_f
#pragma weak mpi_type_match_size__ = mpi_type_match_size_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_MATCH_SIZE,
                           mpi_type_match_size,
                           mpi_type_match_size_,
                           mpi_type_match_size__,
                           mpi_type_match_size_f,
                           (MPI_Fint *typeclass, MPI_Fint *size, MPI_Fint *type, MPI_Fint *ierr),
                           (typeclass, size, type, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_match_size_f(MPI_Fint *typeclass, MPI_Fint *size, MPI_Fint *type, MPI_Fint *ierr)
{

}
