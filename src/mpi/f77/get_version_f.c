/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GET_VERSION = mpi_get_version_f
#pragma weak pmpi_get_version = mpi_get_version_f
#pragma weak pmpi_get_version_ = mpi_get_version_f
#pragma weak pmpi_get_version__ = mpi_get_version_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GET_VERSION,
                           pmpi_get_version,
                           pmpi_get_version_,
                           pmpi_get_version__,
                           pmpi_get_version_f,
                           (MPI_Fint *version, MPI_Fint *subversion, MPI_Fint *ierr),
                           (version, subversion, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_VERSION = mpi_get_version_f
#pragma weak mpi_get_version = mpi_get_version_f
#pragma weak mpi_get_version_ = mpi_get_version_f
#pragma weak mpi_get_version__ = mpi_get_version_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GET_VERSION,
                           mpi_get_version,
                           mpi_get_version_,
                           mpi_get_version__,
                           mpi_get_version_f,
                           (MPI_Fint *version, MPI_Fint *subversion, MPI_Fint *ierr),
                           (version, subversion, ierr) )
#endif

void mpi_get_version_f(MPI_Fint *version, MPI_Fint *subversion, MPI_Fint *ierr)
{

}
