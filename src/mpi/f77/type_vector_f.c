/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_VECTOR = mpi_type_vector_f
#pragma weak pmpi_type_vector = mpi_type_vector_f
#pragma weak pmpi_type_vector_ = mpi_type_vector_f
#pragma weak pmpi_type_vector__ = mpi_type_vector_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_VECTOR,
                           pmpi_type_vector,
                           pmpi_type_vector_,
                           pmpi_type_vector__,
                           pmpi_type_vector_f,
                           (MPI_Fint *count, MPI_Fint *blocklength, MPI_Fint *stride, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, blocklength, stride, oldtype, newtype, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_VECTOR = mpi_type_vector_f
#pragma weak mpi_type_vector = mpi_type_vector_f
#pragma weak mpi_type_vector_ = mpi_type_vector_f
#pragma weak mpi_type_vector__ = mpi_type_vector_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_VECTOR,
                           mpi_type_vector,
                           mpi_type_vector_,
                           mpi_type_vector__,
                           mpi_type_vector_f,
                           (MPI_Fint *count, MPI_Fint *blocklength, MPI_Fint *stride, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, blocklength, stride, oldtype, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_vector_f(MPI_Fint *count, MPI_Fint *blocklength, MPI_Fint *stride, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr)
{
    MPI_Datatype c_old;
    MPI_Datatype c_new;

    c_old = MPI_Type_f2c(*oldtype);

    *ierr = MPI_Type_vector(*count, *blocklength, *stride,
                          c_old, &c_new);

    if (*ierr == MPI_SUCCESS) {
        *newtype = MPI_Type_c2f(c_new);
    }
}
