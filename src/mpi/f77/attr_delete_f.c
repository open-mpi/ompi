/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ATTR_DELETE = mpi_attr_delete_f
#pragma weak pmpi_attr_delete = mpi_attr_delete_f
#pragma weak pmpi_attr_delete_ = mpi_attr_delete_f
#pragma weak pmpi_attr_delete__ = mpi_attr_delete_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ATTR_DELETE,
                           pmpi_attr_delete,
                           pmpi_attr_delete_,
                           pmpi_attr_delete__,
                           pmpi_attr_delete_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *ierr),
                           (comm, keyval, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ATTR_DELETE = mpi_attr_delete_f
#pragma weak mpi_attr_delete = mpi_attr_delete_f
#pragma weak mpi_attr_delete_ = mpi_attr_delete_f
#pragma weak mpi_attr_delete__ = mpi_attr_delete_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ATTR_DELETE,
                           mpi_attr_delete,
                           mpi_attr_delete_,
                           mpi_attr_delete__,
                           mpi_attr_delete_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *ierr),
                           (comm, keyval, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_attr_delete_f(MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *ierr)
{

}
