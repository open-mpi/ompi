/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_SPLIT = mpi_comm_split_f
#pragma weak pmpi_comm_split = mpi_comm_split_f
#pragma weak pmpi_comm_split_ = mpi_comm_split_f
#pragma weak pmpi_comm_split__ = mpi_comm_split_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_SPLIT,
                           pmpi_comm_split,
                           pmpi_comm_split_,
                           pmpi_comm_split__,
                           pmpi_comm_split_f,
                           (MPI_Fint *comm, MPI_Fint *color, MPI_Fint *key, MPI_Fint *newcomm, MPI_Fint *ierr),
                           (comm, color, key, newcomm, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SPLIT = mpi_comm_split_f
#pragma weak mpi_comm_split = mpi_comm_split_f
#pragma weak mpi_comm_split_ = mpi_comm_split_f
#pragma weak mpi_comm_split__ = mpi_comm_split_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_SPLIT,
                           mpi_comm_split,
                           mpi_comm_split_,
                           mpi_comm_split__,
                           mpi_comm_split_f,
                           (MPI_Fint *comm, MPI_Fint *color, MPI_Fint *key, MPI_Fint *newcomm, MPI_Fint *ierr),
                           (comm, color, key, newcomm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_split_f(MPI_Fint *comm, MPI_Fint *color, MPI_Fint *key, MPI_Fint *newcomm, 
                      MPI_Fint *ierr)
{
    MPI_Comm c_newcomm;
    MPI_Comm c_comm = MPI_Comm_f2c ( *comm );

    *ierr = MPI_Comm_split ( c_comm, (int) color, (int) key, &c_newcomm );
    *newcomm = MPI_Comm_c2f (c_newcomm);
}
