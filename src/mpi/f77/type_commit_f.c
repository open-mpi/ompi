/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TYPE_COMMIT = mpi_type_commit_f
#pragma weak pmpi_type_commit = mpi_type_commit_f
#pragma weak pmpi_type_commit_ = mpi_type_commit_f
#pragma weak pmpi_type_commit__ = mpi_type_commit_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TYPE_COMMIT,
                           pmpi_type_commit,
                           pmpi_type_commit_,
                           pmpi_type_commit__,
                           pmpi_type_commit_f,
                           (MPI_Fint *type, MPI_Fint *ierr),
                           (type, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_COMMIT = mpi_type_commit_f
#pragma weak mpi_type_commit = mpi_type_commit_f
#pragma weak mpi_type_commit_ = mpi_type_commit_f
#pragma weak mpi_type_commit__ = mpi_type_commit_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TYPE_COMMIT,
                           mpi_type_commit,
                           mpi_type_commit_,
                           mpi_type_commit__,
                           mpi_type_commit_f,
                           (MPI_Fint *type, MPI_Fint *ierr),
                           (type, ierr) )
#endif

void mpi_type_commit_f(MPI_Fint *type, MPI_Fint *ierr)
{

}
