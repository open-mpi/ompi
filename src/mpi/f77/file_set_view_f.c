/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_SET_VIEW = mpi_file_set_view_f
#pragma weak pmpi_file_set_view = mpi_file_set_view_f
#pragma weak pmpi_file_set_view_ = mpi_file_set_view_f
#pragma weak pmpi_file_set_view__ = mpi_file_set_view_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_SET_VIEW,
                           pmpi_file_set_view,
                           pmpi_file_set_view_,
                           pmpi_file_set_view__,
                           pmpi_file_set_view_f,
                           (MPI_Fint *fh, MPI_Fint *disp, MPI_Fint *etype, MPI_Fint *filetype, char *datarep, MPI_Fint *info, MPI_Fint *ierr),
                           (fh, disp, etype, filetype, datarep, info, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_SET_VIEW = mpi_file_set_view_f
#pragma weak mpi_file_set_view = mpi_file_set_view_f
#pragma weak mpi_file_set_view_ = mpi_file_set_view_f
#pragma weak mpi_file_set_view__ = mpi_file_set_view_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_SET_VIEW,
                           mpi_file_set_view,
                           mpi_file_set_view_,
                           mpi_file_set_view__,
                           mpi_file_set_view_f,
                           (MPI_Fint *fh, MPI_Fint *disp, MPI_Fint *etype, MPI_Fint *filetype, char *datarep, MPI_Fint *info, MPI_Fint *ierr),
                           (fh, disp, etype, filetype, datarep, info, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_file_set_view_f(MPI_Fint *fh, MPI_Fint *disp, MPI_Fint *etype, MPI_Fint *filetype, char *datarep, MPI_Fint *info, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}
