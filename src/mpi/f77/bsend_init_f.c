/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_BSEND_INIT = mpi_bsend_init_f
#pragma weak pmpi_bsend_init = mpi_bsend_init_f
#pragma weak pmpi_bsend_init_ = mpi_bsend_init_f
#pragma weak pmpi_bsend_init__ = mpi_bsend_init_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_BSEND_INIT,
                           pmpi_bsend_init,
                           pmpi_bsend_init_,
                           pmpi_bsend_init__,
                           pmpi_bsend_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BSEND_INIT = mpi_bsend_init_f
#pragma weak mpi_bsend_init = mpi_bsend_init_f
#pragma weak mpi_bsend_init_ = mpi_bsend_init_f
#pragma weak mpi_bsend_init__ = mpi_bsend_init_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_BSEND_INIT,
                           mpi_bsend_init,
                           mpi_bsend_init_,
                           mpi_bsend_init__,
                           mpi_bsend_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_bsend_init_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{

}
