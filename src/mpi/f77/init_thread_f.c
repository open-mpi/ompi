/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_INIT_THREAD = mpi_init_thread_f
#pragma weak pmpi_init_thread = mpi_init_thread_f
#pragma weak pmpi_init_thread_ = mpi_init_thread_f
#pragma weak pmpi_init_thread__ = mpi_init_thread_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INIT_THREAD,
                           pmpi_init_thread,
                           pmpi_init_thread_,
                           pmpi_init_thread__,
                           pmpi_init_thread_f,
                           (MPI_Fint *required, MPI_Fint *provided, MPI_Fint *ierr),
                           (required, provided, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INIT_THREAD = mpi_init_thread_f
#pragma weak mpi_init_thread = mpi_init_thread_f
#pragma weak mpi_init_thread_ = mpi_init_thread_f
#pragma weak mpi_init_thread__ = mpi_init_thread_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INIT_THREAD,
                           mpi_init_thread,
                           mpi_init_thread_,
                           mpi_init_thread__,
                           mpi_init_thread_f,
                           (MPI_Fint *required, MPI_Fint *provided, MPI_Fint *ierr),
                           (required, provided, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_init_thread_f( MPI_Fint *required, MPI_Fint *provided, MPI_Fint *ierr )
{
    int argc = 0;
    char** argv = NULL;

    *ierr = OMPI_INT_2_FINT(MPI_Init_thread( &argc, &argv, 
                                             *required, provided ));
}
