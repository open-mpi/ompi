/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_WRITE_ORDERED_END = mpi_file_write_ordered_end_f
#pragma weak pmpi_file_write_ordered_end = mpi_file_write_ordered_end_f
#pragma weak pmpi_file_write_ordered_end_ = mpi_file_write_ordered_end_f
#pragma weak pmpi_file_write_ordered_end__ = mpi_file_write_ordered_end_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_WRITE_ORDERED_END,
                           pmpi_file_write_ordered_end,
                           pmpi_file_write_ordered_end_,
                           pmpi_file_write_ordered_end__,
                           pmpi_file_write_ordered_end_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *status, MPI_Fint *ierr),
                           (fh, buf, status, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_WRITE_ORDERED_END = mpi_file_write_ordered_end_f
#pragma weak mpi_file_write_ordered_end = mpi_file_write_ordered_end_f
#pragma weak mpi_file_write_ordered_end_ = mpi_file_write_ordered_end_f
#pragma weak mpi_file_write_ordered_end__ = mpi_file_write_ordered_end_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_WRITE_ORDERED_END,
                           mpi_file_write_ordered_end,
                           mpi_file_write_ordered_end_,
                           mpi_file_write_ordered_end__,
                           mpi_file_write_ordered_end_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *status, MPI_Fint *ierr),
                           (fh, buf, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_file_write_ordered_end_f(MPI_Fint *fh, char *buf, MPI_Fint *status, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}
