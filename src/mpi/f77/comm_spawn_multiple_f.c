/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_COMM_SPAWN_MULTIPLE = mpi_comm_spawn_multiple_f
#pragma weak pmpi_comm_spawn_multiple = mpi_comm_spawn_multiple_f
#pragma weak pmpi_comm_spawn_multiple_ = mpi_comm_spawn_multiple_f
#pragma weak pmpi_comm_spawn_multiple__ = mpi_comm_spawn_multiple_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_COMM_SPAWN_MULTIPLE,
                           pmpi_comm_spawn_multiple,
                           pmpi_comm_spawn_multiple_,
                           pmpi_comm_spawn_multiple__,
                           pmpi_comm_spawn_multiple_f,
                           (MPI_Fint *count, char *array_of_commands, char *array_of_argv, MPI_Fint *array_of_maxprocs, MPI_Fint *array_of_info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr),
                           (count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, root, comm, intercomm, array_of_errcodes, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SPAWN_MULTIPLE = mpi_comm_spawn_multiple_f
#pragma weak mpi_comm_spawn_multiple = mpi_comm_spawn_multiple_f
#pragma weak mpi_comm_spawn_multiple_ = mpi_comm_spawn_multiple_f
#pragma weak mpi_comm_spawn_multiple__ = mpi_comm_spawn_multiple_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_COMM_SPAWN_MULTIPLE,
                           mpi_comm_spawn_multiple,
                           mpi_comm_spawn_multiple_,
                           mpi_comm_spawn_multiple__,
                           mpi_comm_spawn_multiple_f,
                           (MPI_Fint *count, char *array_of_commands, char *array_of_argv, MPI_Fint *array_of_maxprocs, MPI_Fint *array_of_info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr),
                           (count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, root, comm, intercomm, array_of_errcodes, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_comm_spawn_multiple_f(MPI_Fint *count, char *array_of_commands, char *array_of_argv, MPI_Fint *array_of_maxprocs, MPI_Fint *array_of_info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr)
{

}
