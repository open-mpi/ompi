/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_CREATE = mpi_win_create_f
#pragma weak pmpi_win_create = mpi_win_create_f
#pragma weak pmpi_win_create_ = mpi_win_create_f
#pragma weak pmpi_win_create__ = mpi_win_create_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_CREATE,
                           pmpi_win_create,
                           pmpi_win_create_,
                           pmpi_win_create__,
                           pmpi_win_create_f,
                           (char *base, MPI_Fint *size, MPI_Fint *disp_unit, MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win, MPI_Fint *ierr),
                           (base, size, disp_unit, info, comm, win, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_CREATE = mpi_win_create_f
#pragma weak mpi_win_create = mpi_win_create_f
#pragma weak mpi_win_create_ = mpi_win_create_f
#pragma weak mpi_win_create__ = mpi_win_create_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_CREATE,
                           mpi_win_create,
                           mpi_win_create_,
                           mpi_win_create__,
                           mpi_win_create_f,
                           (char *base, MPI_Fint *size, MPI_Fint *disp_unit, MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win, MPI_Fint *ierr),
                           (base, size, disp_unit, info, comm, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_win_create_f(char *base, MPI_Fint *size, MPI_Fint *disp_unit, MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}
