/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_SEND_INIT = mpi_send_init_f
#pragma weak pmpi_send_init = mpi_send_init_f
#pragma weak pmpi_send_init_ = mpi_send_init_f
#pragma weak pmpi_send_init__ = mpi_send_init_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_SEND_INIT,
                           pmpi_send_init,
                           pmpi_send_init_,
                           pmpi_send_init__,
                           pmpi_send_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SEND_INIT = mpi_send_init_f
#pragma weak mpi_send_init = mpi_send_init_f
#pragma weak mpi_send_init_ = mpi_send_init_f
#pragma weak mpi_send_init__ = mpi_send_init_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_SEND_INIT,
                           mpi_send_init,
                           mpi_send_init_,
                           mpi_send_init__,
                           mpi_send_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_send_init_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{

}
