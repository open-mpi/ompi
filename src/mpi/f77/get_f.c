/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GET = mpi_get_f
#pragma weak pmpi_get = mpi_get_f
#pragma weak pmpi_get_ = mpi_get_f
#pragma weak pmpi_get__ = mpi_get_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GET,
                           pmpi_get,
                           pmpi_get_,
                           pmpi_get__,
                           pmpi_get_f,
                           (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Fint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *ierr),
                           (origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET = mpi_get_f
#pragma weak mpi_get = mpi_get_f
#pragma weak mpi_get_ = mpi_get_f
#pragma weak mpi_get__ = mpi_get_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GET,
                           mpi_get,
                           mpi_get_,
                           mpi_get__,
                           mpi_get_f,
                           (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Fint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *ierr),
                           (origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_get_f(char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Fint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *ierr)
{

}
