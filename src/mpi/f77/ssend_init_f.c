/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_SSEND_INIT = mpi_ssend_init_f
#pragma weak pmpi_ssend_init = mpi_ssend_init_f
#pragma weak pmpi_ssend_init_ = mpi_ssend_init_f
#pragma weak pmpi_ssend_init__ = mpi_ssend_init_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_SSEND_INIT,
                           pmpi_ssend_init,
                           pmpi_ssend_init_,
                           pmpi_ssend_init__,
                           pmpi_ssend_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SSEND_INIT = mpi_ssend_init_f
#pragma weak mpi_ssend_init = mpi_ssend_init_f
#pragma weak mpi_ssend_init_ = mpi_ssend_init_f
#pragma weak mpi_ssend_init__ = mpi_ssend_init_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_SSEND_INIT,
                           mpi_ssend_init,
                           mpi_ssend_init_,
                           mpi_ssend_init__,
                           mpi_ssend_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_ssend_init_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{

}
