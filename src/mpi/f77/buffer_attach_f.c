/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_BUFFER_ATTACH = mpi_buffer_attach_f
#pragma weak pmpi_buffer_attach = mpi_buffer_attach_f
#pragma weak pmpi_buffer_attach_ = mpi_buffer_attach_f
#pragma weak pmpi_buffer_attach__ = mpi_buffer_attach_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_BUFFER_ATTACH,
                           pmpi_buffer_attach,
                           pmpi_buffer_attach_,
                           pmpi_buffer_attach__,
                           pmpi_buffer_attach_f,
                           (char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (buffer, size, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BUFFER_ATTACH = mpi_buffer_attach_f
#pragma weak mpi_buffer_attach = mpi_buffer_attach_f
#pragma weak mpi_buffer_attach_ = mpi_buffer_attach_f
#pragma weak mpi_buffer_attach__ = mpi_buffer_attach_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_BUFFER_ATTACH,
                           mpi_buffer_attach,
                           mpi_buffer_attach_,
                           mpi_buffer_attach__,
                           mpi_buffer_attach_f,
                           (char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (buffer, size, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_buffer_attach_f(char *buffer, MPI_Fint *size, MPI_Fint *ierr)
{

}
