/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_COMM_GET_PARENT = mpi_comm_get_parent_f
#pragma weak pmpi_comm_get_parent = mpi_comm_get_parent_f
#pragma weak pmpi_comm_get_parent_ = mpi_comm_get_parent_f
#pragma weak pmpi_comm_get_parent__ = mpi_comm_get_parent_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_COMM_GET_PARENT,
                           pmpi_comm_get_parent,
                           pmpi_comm_get_parent_,
                           pmpi_comm_get_parent__,
                           pmpi_comm_get_parent_f,
                           (MPI_Fint *parent, MPI_Fint *ierr),
                           (parent, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_GET_PARENT = mpi_comm_get_parent_f
#pragma weak mpi_comm_get_parent = mpi_comm_get_parent_f
#pragma weak mpi_comm_get_parent_ = mpi_comm_get_parent_f
#pragma weak mpi_comm_get_parent__ = mpi_comm_get_parent_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_COMM_GET_PARENT,
                           mpi_comm_get_parent,
                           mpi_comm_get_parent_,
                           mpi_comm_get_parent__,
                           mpi_comm_get_parent_f,
                           (MPI_Fint *parent, MPI_Fint *ierr),
                           (parent, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_comm_get_parent_f(MPI_Fint *parent, MPI_Fint *ierr)
{

}
