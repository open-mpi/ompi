/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_PUT = mpi_put_f
#pragma weak pmpi_put = mpi_put_f
#pragma weak pmpi_put_ = mpi_put_f
#pragma weak pmpi_put__ = mpi_put_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_PUT,
                           pmpi_put,
                           pmpi_put_,
                           pmpi_put__,
                           pmpi_put_f,
                           (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Fint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *ierr),
                           (origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PUT = mpi_put_f
#pragma weak mpi_put = mpi_put_f
#pragma weak mpi_put_ = mpi_put_f
#pragma weak mpi_put__ = mpi_put_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_PUT,
                           mpi_put,
                           mpi_put_,
                           mpi_put__,
                           mpi_put_f,
                           (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Fint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *ierr),
                           (origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_put_f(char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Fint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *ierr)
{

}
