/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_DUP = mpi_type_dup_f
#pragma weak pmpi_type_dup = mpi_type_dup_f
#pragma weak pmpi_type_dup_ = mpi_type_dup_f
#pragma weak pmpi_type_dup__ = mpi_type_dup_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_DUP,
                           pmpi_type_dup,
                           pmpi_type_dup_,
                           pmpi_type_dup__,
                           pmpi_type_dup_f,
                           (MPI_Fint *type, MPI_Fint *newtype, MPI_Fint *ierr),
                           (type, newtype, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_DUP = mpi_type_dup_f
#pragma weak mpi_type_dup = mpi_type_dup_f
#pragma weak mpi_type_dup_ = mpi_type_dup_f
#pragma weak mpi_type_dup__ = mpi_type_dup_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_DUP,
                           mpi_type_dup,
                           mpi_type_dup_,
                           mpi_type_dup__,
                           mpi_type_dup_f,
                           (MPI_Fint *type, MPI_Fint *newtype, MPI_Fint *ierr),
                           (type, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_dup_f(MPI_Fint *type, MPI_Fint *newtype, MPI_Fint *ierr)
{
    MPI_Datatype c_type = MPI_Type_f2c(*type);
    MPI_Datatype c_new;

    *ierr = MPI_Type_dup(c_type, &c_new);

    if (*ierr == MPI_SUCCESS)
      *newtype = MPI_Type_c2f(c_new);
}
