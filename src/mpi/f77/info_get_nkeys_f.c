/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_INFO_GET_NKEYS = mpi_info_get_nkeys_f
#pragma weak pmpi_info_get_nkeys = mpi_info_get_nkeys_f
#pragma weak pmpi_info_get_nkeys_ = mpi_info_get_nkeys_f
#pragma weak pmpi_info_get_nkeys__ = mpi_info_get_nkeys_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_INFO_GET_NKEYS,
                           pmpi_info_get_nkeys,
                           pmpi_info_get_nkeys_,
                           pmpi_info_get_nkeys__,
                           pmpi_info_get_nkeys_f,
                           (MPI_Fint *info, MPI_Fint *nkeys, MPI_Fint *ierr),
                           (info, nkeys, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_GET_NKEYS = mpi_info_get_nkeys_f
#pragma weak mpi_info_get_nkeys = mpi_info_get_nkeys_f
#pragma weak mpi_info_get_nkeys_ = mpi_info_get_nkeys_f
#pragma weak mpi_info_get_nkeys__ = mpi_info_get_nkeys_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_INFO_GET_NKEYS,
                           mpi_info_get_nkeys,
                           mpi_info_get_nkeys_,
                           mpi_info_get_nkeys__,
                           mpi_info_get_nkeys_f,
                           (MPI_Fint *info, MPI_Fint *nkeys, MPI_Fint *ierr),
                           (info, nkeys, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_info_get_nkeys_f(MPI_Fint *info, MPI_Fint *nkeys, MPI_Fint *ierr)
{

}
