/*
 * $HEADER$
 */

#include "lam_config.h"

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
	        (MPI_Fint *comm, char *name, MPI_Fint *l, MPI_Fint *ierror, MPI_Fint charlen),
	        (comm, name, l, ierror, charlen))
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_GET_NAME = mpi_comm_get_name_f
#pragma weak mpi_comm_get_name = mpi_comm_get_name_f
#pragma weak mpi_comm_get_name_ = mpi_comm_get_name_f
#pragma weak mpi_comm_get_name__ = mpi_comm_get_name_f
#endif

#if ! LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
LAM_GENERATE_F77_BINDINGS (MPI_COMM_GET_NAME,
	        mpi_comm_get_name,
	        mpi_comm_get_name_,
	        mpi_comm_get_name__,
	        mpi_comm_get_name_f,
	        (MPI_Fint *comm, char *name, MPI_Fint *l, MPI_Fint *ierror, MPI_Fint charlen),
	        (comm, name, l, ierror, charlen))
#endif
	

void
mpi_comm_get_name_f(MPI_Fint *comm, char *name, MPI_Fint *l, MPI_Fint *ierror, MPI_Fint charlen)
{
  MPI_Comm c_comm = MPI_COMM_WORLD;

  *ierror = MPI_Comm_get_name(c_comm, name, l);
}
