/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_COMMIT = mpi_type_commit_f
#pragma weak pmpi_type_commit = mpi_type_commit_f
#pragma weak pmpi_type_commit_ = mpi_type_commit_f
#pragma weak pmpi_type_commit__ = mpi_type_commit_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_COMMIT,
                           pmpi_type_commit,
                           pmpi_type_commit_,
                           pmpi_type_commit__,
                           pmpi_type_commit_f,
                           (MPI_Fint *type, MPI_Fint *ierr),
                           (type, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_COMMIT = mpi_type_commit_f
#pragma weak mpi_type_commit = mpi_type_commit_f
#pragma weak mpi_type_commit_ = mpi_type_commit_f
#pragma weak mpi_type_commit__ = mpi_type_commit_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_COMMIT,
                           mpi_type_commit,
                           mpi_type_commit_,
                           mpi_type_commit__,
                           mpi_type_commit_f,
                           (MPI_Fint *type, MPI_Fint *ierr),
                           (type, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_type_commit_f(MPI_Fint *type, MPI_Fint *ierr)
{
    MPI_Datatype c_type = MPI_Type_f2c(*type);

    *ierr = MPI_Type_commit(&c_type); 

    if (MPI_SUCCESS == *ierr) {
      *type = MPI_Type_c2f(c_type);
    }
}
