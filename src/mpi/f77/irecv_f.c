/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_IRECV = mpi_irecv_f
#pragma weak pmpi_irecv = mpi_irecv_f
#pragma weak pmpi_irecv_ = mpi_irecv_f
#pragma weak pmpi_irecv__ = mpi_irecv_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_IRECV,
                           pmpi_irecv,
                           pmpi_irecv_,
                           pmpi_irecv__,
                           pmpi_irecv_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, source, tag, comm, request, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IRECV = mpi_irecv_f
#pragma weak mpi_irecv = mpi_irecv_f
#pragma weak mpi_irecv_ = mpi_irecv_f
#pragma weak mpi_irecv__ = mpi_irecv_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_IRECV,
                           mpi_irecv,
                           mpi_irecv_,
                           mpi_irecv__,
                           mpi_irecv_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, source, tag, comm, request, ierr) )
#endif

void mpi_irecv_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{

}
