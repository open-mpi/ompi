/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_COMM_GET_NAME = mpi_comm_get_name_f
#pragma weak pmpi_comm_get_name = mpi_comm_get_name_f
#pragma weak pmpi_comm_get_name_ = mpi_comm_get_name_f
#pragma weak pmpi_comm_get_name__ = mpi_comm_get_name_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_COMM_GET_NAME,
                           pmpi_comm_get_name,
                           pmpi_comm_get_name_,
                           pmpi_comm_get_name__,
                           pmpi_comm_get_name_f,
                           (MPI_Fint *comm, char *comm_name, MPI_Fint *resultlen, MPI_Fint *ierr),
                           (comm, comm_name, resultlen, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_GET_NAME = mpi_comm_get_name_f
#pragma weak mpi_comm_get_name = mpi_comm_get_name_f
#pragma weak mpi_comm_get_name_ = mpi_comm_get_name_f
#pragma weak mpi_comm_get_name__ = mpi_comm_get_name_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_COMM_GET_NAME,
                           mpi_comm_get_name,
                           mpi_comm_get_name_,
                           mpi_comm_get_name__,
                           mpi_comm_get_name_f,
                           (MPI_Fint *comm, char *comm_name, MPI_Fint *resultlen, MPI_Fint *ierr),
                           (comm, comm_name, resultlen, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_comm_get_name_f(MPI_Fint *comm, char *comm_name, MPI_Fint *resultlen, MPI_Fint *ierr)
{

}
