/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_ATTR_GET = mpi_attr_get_f
#pragma weak pmpi_attr_get = mpi_attr_get_f
#pragma weak pmpi_attr_get_ = mpi_attr_get_f
#pragma weak pmpi_attr_get__ = mpi_attr_get_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_ATTR_GET,
                           pmpi_attr_get,
                           pmpi_attr_get_,
                           pmpi_attr_get__,
                           pmpi_attr_get_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, flag, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ATTR_GET = mpi_attr_get_f
#pragma weak mpi_attr_get = mpi_attr_get_f
#pragma weak mpi_attr_get_ = mpi_attr_get_f
#pragma weak mpi_attr_get__ = mpi_attr_get_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_ATTR_GET,
                           mpi_attr_get,
                           mpi_attr_get_,
                           mpi_attr_get__,
                           mpi_attr_get_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, flag, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_attr_get_f(MPI_Fint *comm, MPI_Fint *keyval, char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr)
{

}
