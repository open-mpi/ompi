/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_INFO_GET_NTHKEY = mpi_info_get_nthkey_f
#pragma weak pmpi_info_get_nthkey = mpi_info_get_nthkey_f
#pragma weak pmpi_info_get_nthkey_ = mpi_info_get_nthkey_f
#pragma weak pmpi_info_get_nthkey__ = mpi_info_get_nthkey_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_INFO_GET_NTHKEY,
                           pmpi_info_get_nthkey,
                           pmpi_info_get_nthkey_,
                           pmpi_info_get_nthkey__,
                           pmpi_info_get_nthkey_f,
                           (MPI_Fint *info, MPI_Fint *n, char *key, MPI_Fint *ierr),
                           (info, n, key, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_GET_NTHKEY = mpi_info_get_nthkey_f
#pragma weak mpi_info_get_nthkey = mpi_info_get_nthkey_f
#pragma weak mpi_info_get_nthkey_ = mpi_info_get_nthkey_f
#pragma weak mpi_info_get_nthkey__ = mpi_info_get_nthkey_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_INFO_GET_NTHKEY,
                           mpi_info_get_nthkey,
                           mpi_info_get_nthkey_,
                           mpi_info_get_nthkey__,
                           mpi_info_get_nthkey_f,
                           (MPI_Fint *info, MPI_Fint *n, char *key, MPI_Fint *ierr),
                           (info, n, key, ierr) )
#endif

void mpi_info_get_nthkey_f(MPI_Fint *info, MPI_Fint *n, char *key, MPI_Fint *ierr)
{

}
