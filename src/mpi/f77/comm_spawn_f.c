/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_SPAWN = mpi_comm_spawn_f
#pragma weak pmpi_comm_spawn = mpi_comm_spawn_f
#pragma weak pmpi_comm_spawn_ = mpi_comm_spawn_f
#pragma weak pmpi_comm_spawn__ = mpi_comm_spawn_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_SPAWN,
                           pmpi_comm_spawn,
                           pmpi_comm_spawn_,
                           pmpi_comm_spawn__,
                           pmpi_comm_spawn_f,
                           (char *command, char *argv, MPI_Fint *maxprocs, MPI_Fint *info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr),
                           (command, argv, maxprocs, info, root, comm, intercomm, array_of_errcodes, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SPAWN = mpi_comm_spawn_f
#pragma weak mpi_comm_spawn = mpi_comm_spawn_f
#pragma weak mpi_comm_spawn_ = mpi_comm_spawn_f
#pragma weak mpi_comm_spawn__ = mpi_comm_spawn_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_SPAWN,
                           mpi_comm_spawn,
                           mpi_comm_spawn_,
                           mpi_comm_spawn__,
                           mpi_comm_spawn_f,
                           (char *command, char *argv, MPI_Fint *maxprocs, MPI_Fint *info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr),
                           (command, argv, maxprocs, info, root, comm, intercomm, array_of_errcodes, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_spawn_f(char *command, char *argv, MPI_Fint *maxprocs, MPI_Fint *info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr)
{

}
