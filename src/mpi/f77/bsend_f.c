/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_BSEND = mpi_bsend_f
#pragma weak pmpi_bsend = mpi_bsend_f
#pragma weak pmpi_bsend_ = mpi_bsend_f
#pragma weak pmpi_bsend__ = mpi_bsend_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_BSEND,
                           pmpi_bsend,
                           pmpi_bsend_,
                           pmpi_bsend__,
                           pmpi_bsend_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BSEND = mpi_bsend_f
#pragma weak mpi_bsend = mpi_bsend_f
#pragma weak mpi_bsend_ = mpi_bsend_f
#pragma weak mpi_bsend__ = mpi_bsend_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_BSEND,
                           mpi_bsend,
                           mpi_bsend_,
                           mpi_bsend__,
                           mpi_bsend_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#endif

void mpi_bsend_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr)
{

}
