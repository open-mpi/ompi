/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_SSEND = mpi_ssend_f
#pragma weak pmpi_ssend = mpi_ssend_f
#pragma weak pmpi_ssend_ = mpi_ssend_f
#pragma weak pmpi_ssend__ = mpi_ssend_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_SSEND,
                           pmpi_ssend,
                           pmpi_ssend_,
                           pmpi_ssend__,
                           pmpi_ssend_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SSEND = mpi_ssend_f
#pragma weak mpi_ssend = mpi_ssend_f
#pragma weak mpi_ssend_ = mpi_ssend_f
#pragma weak mpi_ssend__ = mpi_ssend_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_SSEND,
                           mpi_ssend,
                           mpi_ssend_,
                           mpi_ssend__,
                           mpi_ssend_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#endif

void mpi_ssend_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr)
{

}
