/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_IS_THREAD_MAIN = mpi_is_thread_main_f
#pragma weak pmpi_is_thread_main = mpi_is_thread_main_f
#pragma weak pmpi_is_thread_main_ = mpi_is_thread_main_f
#pragma weak pmpi_is_thread_main__ = mpi_is_thread_main_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_IS_THREAD_MAIN,
                           pmpi_is_thread_main,
                           pmpi_is_thread_main_,
                           pmpi_is_thread_main__,
                           pmpi_is_thread_main_f,
                           (MPI_Fint *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IS_THREAD_MAIN = mpi_is_thread_main_f
#pragma weak mpi_is_thread_main = mpi_is_thread_main_f
#pragma weak mpi_is_thread_main_ = mpi_is_thread_main_f
#pragma weak mpi_is_thread_main__ = mpi_is_thread_main_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_IS_THREAD_MAIN,
                           mpi_is_thread_main,
                           mpi_is_thread_main_,
                           mpi_is_thread_main__,
                           mpi_is_thread_main_f,
                           (MPI_Fint *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_is_thread_main_f(MPI_Fint *flag, MPI_Fint *ierr)
{
    *ierr = MPI_Is_thread_main( (int)flag );
}
