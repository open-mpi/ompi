/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_INFO_GET_VALUELEN = mpi_info_get_valuelen_f
#pragma weak pmpi_info_get_valuelen = mpi_info_get_valuelen_f
#pragma weak pmpi_info_get_valuelen_ = mpi_info_get_valuelen_f
#pragma weak pmpi_info_get_valuelen__ = mpi_info_get_valuelen_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_INFO_GET_VALUELEN,
                           pmpi_info_get_valuelen,
                           pmpi_info_get_valuelen_,
                           pmpi_info_get_valuelen__,
                           pmpi_info_get_valuelen_f,
                           (MPI_Fint *info, char *key, MPI_Fint *valuelen, MPI_Fint *flag, MPI_Fint *ierr),
                           (info, key, valuelen, flag, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_GET_VALUELEN = mpi_info_get_valuelen_f
#pragma weak mpi_info_get_valuelen = mpi_info_get_valuelen_f
#pragma weak mpi_info_get_valuelen_ = mpi_info_get_valuelen_f
#pragma weak mpi_info_get_valuelen__ = mpi_info_get_valuelen_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_INFO_GET_VALUELEN,
                           mpi_info_get_valuelen,
                           mpi_info_get_valuelen_,
                           mpi_info_get_valuelen__,
                           mpi_info_get_valuelen_f,
                           (MPI_Fint *info, char *key, MPI_Fint *valuelen, MPI_Fint *flag, MPI_Fint *ierr),
                           (info, key, valuelen, flag, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_info_get_valuelen_f(MPI_Fint *info, char *key, MPI_Fint *valuelen, MPI_Fint *flag, MPI_Fint *ierr)
{

}
