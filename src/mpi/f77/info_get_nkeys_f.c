/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_INFO_GET_NKEYS = mpi_info_get_nkeys_f
#pragma weak pmpi_info_get_nkeys = mpi_info_get_nkeys_f
#pragma weak pmpi_info_get_nkeys_ = mpi_info_get_nkeys_f
#pragma weak pmpi_info_get_nkeys__ = mpi_info_get_nkeys_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INFO_GET_NKEYS,
                           pmpi_info_get_nkeys,
                           pmpi_info_get_nkeys_,
                           pmpi_info_get_nkeys__,
                           pmpi_info_get_nkeys_f,
                           (MPI_Fint *info, MPI_Fint *nkeys, MPI_Fint *ierr),
                           (info, nkeys, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_GET_NKEYS = mpi_info_get_nkeys_f
#pragma weak mpi_info_get_nkeys = mpi_info_get_nkeys_f
#pragma weak mpi_info_get_nkeys_ = mpi_info_get_nkeys_f
#pragma weak mpi_info_get_nkeys__ = mpi_info_get_nkeys_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INFO_GET_NKEYS,
                           mpi_info_get_nkeys,
                           mpi_info_get_nkeys_,
                           mpi_info_get_nkeys__,
                           mpi_info_get_nkeys_f,
                           (MPI_Fint *info, MPI_Fint *nkeys, MPI_Fint *ierr),
                           (info, nkeys, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

OMPI_EXPORT
void mpi_info_get_nkeys_f(MPI_Fint *info, MPI_Fint *nkeys, MPI_Fint *ierr)
{
    MPI_Info c_info;
    OMPI_SINGLE_NAME_DECL(nkeys);

    c_info = MPI_Info_f2c(*info);
    
    *ierr = OMPI_INT_2_FINT(MPI_Info_get_nkeys(c_info, 
					       OMPI_SINGLE_NAME_CONVERT(nkeys)
					       ));
    
    OMPI_SINGLE_INT_2_FINT(nkeys);
}
