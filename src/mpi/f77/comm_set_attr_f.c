/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_SET_ATTR = mpi_comm_set_attr_f
#pragma weak pmpi_comm_set_attr = mpi_comm_set_attr_f
#pragma weak pmpi_comm_set_attr_ = mpi_comm_set_attr_f
#pragma weak pmpi_comm_set_attr__ = mpi_comm_set_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_SET_ATTR,
                           pmpi_comm_set_attr,
                           pmpi_comm_set_attr_,
                           pmpi_comm_set_attr__,
                           pmpi_comm_set_attr_f,
                           (MPI_Fint *comm, MPI_Fint *comm_keyval, char *attribute_val, MPI_Fint *ierr),
                           (comm, comm_keyval, attribute_val, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SET_ATTR = mpi_comm_set_attr_f
#pragma weak mpi_comm_set_attr = mpi_comm_set_attr_f
#pragma weak mpi_comm_set_attr_ = mpi_comm_set_attr_f
#pragma weak mpi_comm_set_attr__ = mpi_comm_set_attr_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_SET_ATTR,
                           mpi_comm_set_attr,
                           mpi_comm_set_attr_,
                           mpi_comm_set_attr__,
                           mpi_comm_set_attr_f,
                           (MPI_Fint *comm, MPI_Fint *comm_keyval, char *attribute_val, MPI_Fint *ierr),
                           (comm, comm_keyval, attribute_val, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_set_attr_f(MPI_Fint *comm, MPI_Fint *comm_keyval, char *attribute_val, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}
