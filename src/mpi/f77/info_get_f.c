/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_INFO_GET = mpi_info_get_f
#pragma weak pmpi_info_get = mpi_info_get_f
#pragma weak pmpi_info_get_ = mpi_info_get_f
#pragma weak pmpi_info_get__ = mpi_info_get_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_INFO_GET,
                           pmpi_info_get,
                           pmpi_info_get_,
                           pmpi_info_get__,
                           pmpi_info_get_f,
                           (MPI_Fint *info, char *key, MPI_Fint *valuelen, char *value, MPI_Fint *flag, MPI_Fint *ierr),
                           (info, key, valuelen, value, flag, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_GET = mpi_info_get_f
#pragma weak mpi_info_get = mpi_info_get_f
#pragma weak mpi_info_get_ = mpi_info_get_f
#pragma weak mpi_info_get__ = mpi_info_get_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_INFO_GET,
                           mpi_info_get,
                           mpi_info_get_,
                           mpi_info_get__,
                           mpi_info_get_f,
                           (MPI_Fint *info, char *key, MPI_Fint *valuelen, char *value, MPI_Fint *flag, MPI_Fint *ierr),
                           (info, key, valuelen, value, flag, ierr) )
#endif

void mpi_info_get_f(MPI_Fint *info, char *key, MPI_Fint *valuelen, char *value, MPI_Fint *flag, MPI_Fint *ierr)
{

}
