/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_LOOKUP_NAME = mpi_lookup_name_f
#pragma weak pmpi_lookup_name = mpi_lookup_name_f
#pragma weak pmpi_lookup_name_ = mpi_lookup_name_f
#pragma weak pmpi_lookup_name__ = mpi_lookup_name_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_LOOKUP_NAME,
                           pmpi_lookup_name,
                           pmpi_lookup_name_,
                           pmpi_lookup_name__,
                           pmpi_lookup_name_f,
                           (char *service_name, MPI_Fint *info, char *port_name, MPI_Fint *ierr),
                           (service_name, info, port_name, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_LOOKUP_NAME = mpi_lookup_name_f
#pragma weak mpi_lookup_name = mpi_lookup_name_f
#pragma weak mpi_lookup_name_ = mpi_lookup_name_f
#pragma weak mpi_lookup_name__ = mpi_lookup_name_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_LOOKUP_NAME,
                           mpi_lookup_name,
                           mpi_lookup_name_,
                           mpi_lookup_name__,
                           mpi_lookup_name_f,
                           (char *service_name, MPI_Fint *info, char *port_name, MPI_Fint *ierr),
                           (service_name, info, port_name, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_lookup_name_f(char *service_name, MPI_Fint *info,
		       char *port_name, MPI_Fint *ierr)
{
    MPI_Info c_info;

    c_info = MPI_Info_f2c(*info);
    
    *ierr = OMPI_INT_2_FINT(MPI_Lookup_name(service_name, c_info,
					    port_name));
}
