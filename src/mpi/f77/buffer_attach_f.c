/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_BUFFER_ATTACH = mpi_buffer_attach_f
#pragma weak pmpi_buffer_attach = mpi_buffer_attach_f
#pragma weak pmpi_buffer_attach_ = mpi_buffer_attach_f
#pragma weak pmpi_buffer_attach__ = mpi_buffer_attach_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_BUFFER_ATTACH,
                           pmpi_buffer_attach,
                           pmpi_buffer_attach_,
                           pmpi_buffer_attach__,
                           pmpi_buffer_attach_f,
                           (char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (buffer, size, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BUFFER_ATTACH = mpi_buffer_attach_f
#pragma weak mpi_buffer_attach = mpi_buffer_attach_f
#pragma weak mpi_buffer_attach_ = mpi_buffer_attach_f
#pragma weak mpi_buffer_attach__ = mpi_buffer_attach_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_BUFFER_ATTACH,
                           mpi_buffer_attach,
                           mpi_buffer_attach_,
                           mpi_buffer_attach__,
                           mpi_buffer_attach_f,
                           (char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (buffer, size, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_buffer_attach_f(char *buffer, MPI_Fint *size, MPI_Fint *ierr)
{
  *ierr = MPI_Buffer_attach(buffer, *size);
}
