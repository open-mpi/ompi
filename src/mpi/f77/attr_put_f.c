/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ATTR_PUT = mpi_attr_put_f
#pragma weak pmpi_attr_put = mpi_attr_put_f
#pragma weak pmpi_attr_put_ = mpi_attr_put_f
#pragma weak pmpi_attr_put__ = mpi_attr_put_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ATTR_PUT,
                           pmpi_attr_put,
                           pmpi_attr_put_,
                           pmpi_attr_put__,
                           pmpi_attr_put_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, char *attribute_val, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ATTR_PUT = mpi_attr_put_f
#pragma weak mpi_attr_put = mpi_attr_put_f
#pragma weak mpi_attr_put_ = mpi_attr_put_f
#pragma weak mpi_attr_put__ = mpi_attr_put_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ATTR_PUT,
                           mpi_attr_put,
                           mpi_attr_put_,
                           mpi_attr_put__,
                           mpi_attr_put_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, char *attribute_val, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_attr_put_f(MPI_Fint *comm, MPI_Fint *keyval, char *attribute_val, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}
