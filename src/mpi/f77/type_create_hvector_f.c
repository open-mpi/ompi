/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_CREATE_HVECTOR = mpi_type_create_hvector_f
#pragma weak pmpi_type_create_hvector = mpi_type_create_hvector_f
#pragma weak pmpi_type_create_hvector_ = mpi_type_create_hvector_f
#pragma weak pmpi_type_create_hvector__ = mpi_type_create_hvector_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_HVECTOR,
                           pmpi_type_create_hvector,
                           pmpi_type_create_hvector_,
                           pmpi_type_create_hvector__,
                           pmpi_type_create_hvector_f,
                           (MPI_Fint *count, MPI_Fint *blocklength, MPI_Fint *stride, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, blocklength, stride, oldtype, newtype, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_HVECTOR = mpi_type_create_hvector_f
#pragma weak mpi_type_create_hvector = mpi_type_create_hvector_f
#pragma weak mpi_type_create_hvector_ = mpi_type_create_hvector_f
#pragma weak mpi_type_create_hvector__ = mpi_type_create_hvector_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_HVECTOR,
                           mpi_type_create_hvector,
                           mpi_type_create_hvector_,
                           mpi_type_create_hvector__,
                           mpi_type_create_hvector_f,
                           (MPI_Fint *count, MPI_Fint *blocklength, MPI_Fint *stride, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, blocklength, stride, oldtype, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_create_hvector_f(MPI_Fint *count, MPI_Fint *blocklength, MPI_Fint *stride, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr)
{
    MPI_Datatype c_old = MPI_Type_f2c(*oldtype);
    MPI_Datatype c_new;
    MPI_Aint c_stride = (MPI_Aint)*stride;

    *ierr = MPI_Type_hvector(*count, *blocklength, c_stride,
                             c_old, &c_new);

    if (*ierr == MPI_SUCCESS){
        *newtype = MPI_Type_c2f(c_new);
    }
}
