/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_SPAWN_MULTIPLE = mpi_comm_spawn_multiple_f
#pragma weak pmpi_comm_spawn_multiple = mpi_comm_spawn_multiple_f
#pragma weak pmpi_comm_spawn_multiple_ = mpi_comm_spawn_multiple_f
#pragma weak pmpi_comm_spawn_multiple__ = mpi_comm_spawn_multiple_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_SPAWN_MULTIPLE,
                           pmpi_comm_spawn_multiple,
                           pmpi_comm_spawn_multiple_,
                           pmpi_comm_spawn_multiple__,
                           pmpi_comm_spawn_multiple_f,
                           (MPI_Fint *count, char *array_of_commands, char *array_of_argv, MPI_Fint *array_of_maxprocs, MPI_Fint *array_of_info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr),
                           (count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, root, comm, intercomm, array_of_errcodes, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SPAWN_MULTIPLE = mpi_comm_spawn_multiple_f
#pragma weak mpi_comm_spawn_multiple = mpi_comm_spawn_multiple_f
#pragma weak mpi_comm_spawn_multiple_ = mpi_comm_spawn_multiple_f
#pragma weak mpi_comm_spawn_multiple__ = mpi_comm_spawn_multiple_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_SPAWN_MULTIPLE,
                           mpi_comm_spawn_multiple,
                           mpi_comm_spawn_multiple_,
                           mpi_comm_spawn_multiple__,
                           mpi_comm_spawn_multiple_f,
                           (MPI_Fint *count, char *array_of_commands, char *array_of_argv, MPI_Fint *array_of_maxprocs, MPI_Fint *array_of_info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr),
                           (count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, root, comm, intercomm, array_of_errcodes, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_spawn_multiple_f(MPI_Fint *count, char *array_commands,
			       char *array_argv,
			       MPI_Fint *array_maxprocs,
			       MPI_Fint *array_info, MPI_Fint *root,
			       MPI_Fint *comm, MPI_Fint *intercomm,
			       MPI_Fint *array_errcds, MPI_Fint *ierr)
{
#if 0
    MPI_Comm c_comm, c_new_comm;
    MPI_Info *c_info;
    int size, array_size, i;
    OMPI_ARRAY_NAME_DECL(array_maxprocs);
    OMPI_ARRAY_NAME_DECL(array_errcds);
    
    c_comm = MPI_Comm_f2c(*comm);
    
    MPI_Comm_size(c_comm, &size);

    array_size = OMPI_FINT_2_INT(*count);
    OMPI_ARRAY_FINT_2_INT(array_maxprocs, array_size);
    OMPI_ARRAY_FINT_2_INT_ALLOC(array_errcds, size);

    c_info = malloc (array_size * sizeof(MPI_Info));
    for (i = 0; i < array_size; ++i) {
	c_info[i] = MPI_Info_f2c(array_info[i]);
    }

    *ierr = 
	OMPI_INT_2_FINT(MPI_Comm_spawn_multiple(OMPI_FINT_2_INT(*count),
				       array_commands,
				       array_argv, 
				       OMPI_ARRAY_NAME_CONVERT(array_maxprocs),
				       c_info,
				       OMPI_FINT_2_INT(*root),
				       c_comm, &c_new_comm,
				       OMPI_ARRAY_NAME_CONVERT(array_errcds)));
    
    *intercomm = MPI_Comm_c2f(c_new_comm);
    OMPI_ARRAY_INT_2_FINT(array_errcds, size);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_maxprocs);

#endif
}
