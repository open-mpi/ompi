/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_COMPARE = mpi_comm_compare_f
#pragma weak pmpi_comm_compare = mpi_comm_compare_f
#pragma weak pmpi_comm_compare_ = mpi_comm_compare_f
#pragma weak pmpi_comm_compare__ = mpi_comm_compare_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_COMPARE,
                           pmpi_comm_compare,
                           pmpi_comm_compare_,
                           pmpi_comm_compare__,
                           pmpi_comm_compare_f,
                           (MPI_Fint *comm1, MPI_Fint *comm2, MPI_Fint *result, MPI_Fint *ierr),
                           (comm1, comm2, result, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_COMPARE = mpi_comm_compare_f
#pragma weak mpi_comm_compare = mpi_comm_compare_f
#pragma weak mpi_comm_compare_ = mpi_comm_compare_f
#pragma weak mpi_comm_compare__ = mpi_comm_compare_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_COMPARE,
                           mpi_comm_compare,
                           mpi_comm_compare_,
                           mpi_comm_compare__,
                           mpi_comm_compare_f,
                           (MPI_Fint *comm1, MPI_Fint *comm2, MPI_Fint *result, MPI_Fint *ierr),
                           (comm1, comm2, result, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_compare_f(MPI_Fint *comm1, MPI_Fint *comm2, MPI_Fint *result, MPI_Fint *ierr)
{
    MPI_Comm c_comm1 = MPI_Comm_f2c(*comm1);
    MPI_Comm c_comm2 = MPI_Comm_f2c(*comm2);
    OMPI_SINGLE_NAME_DECL(result);

    *ierr = OMPI_INT_2_FINT(MPI_Comm_compare(c_comm1, c_comm2, 
				     OMPI_SINGLE_NAME_CONVERT(result)));

    OMPI_SINGLE_INT_2_FINT(result);
}
