/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_COMM_CREATE_ERRHANDLER = mpi_comm_create_errhandler_f
#pragma weak pmpi_comm_create_errhandler = mpi_comm_create_errhandler_f
#pragma weak pmpi_comm_create_errhandler_ = mpi_comm_create_errhandler_f
#pragma weak pmpi_comm_create_errhandler__ = mpi_comm_create_errhandler_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_COMM_CREATE_ERRHANDLER,
                           pmpi_comm_create_errhandler,
                           pmpi_comm_create_errhandler_,
                           pmpi_comm_create_errhandler__,
                           pmpi_comm_create_errhandler_f,
                           (MPI_Fint *function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_CREATE_ERRHANDLER = mpi_comm_create_errhandler_f
#pragma weak mpi_comm_create_errhandler = mpi_comm_create_errhandler_f
#pragma weak mpi_comm_create_errhandler_ = mpi_comm_create_errhandler_f
#pragma weak mpi_comm_create_errhandler__ = mpi_comm_create_errhandler_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_COMM_CREATE_ERRHANDLER,
                           mpi_comm_create_errhandler,
                           mpi_comm_create_errhandler_,
                           mpi_comm_create_errhandler__,
                           mpi_comm_create_errhandler_f,
                           (MPI_Fint *function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_comm_create_errhandler_f(MPI_Fint *function, MPI_Fint *errhandler, MPI_Fint *ierr)
{

}
