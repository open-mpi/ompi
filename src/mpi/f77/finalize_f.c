/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FINALIZE = mpi_finalize_f
#pragma weak pmpi_finalize = mpi_finalize_f
#pragma weak pmpi_finalize_ = mpi_finalize_f
#pragma weak pmpi_finalize__ = mpi_finalize_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FINALIZE,
                           pmpi_finalize,
                           pmpi_finalize_,
                           pmpi_finalize__,
                           pmpi_finalize_f,
                           (MPI_Fint *ierr),
                           (ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FINALIZE = mpi_finalize_f
#pragma weak mpi_finalize = mpi_finalize_f
#pragma weak mpi_finalize_ = mpi_finalize_f
#pragma weak mpi_finalize__ = mpi_finalize_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FINALIZE,
                           mpi_finalize,
                           mpi_finalize_,
                           mpi_finalize__,
                           mpi_finalize_f,
                           (MPI_Fint *ierr),
                           (ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_finalize_f(MPI_Fint *ierr)
{
    *ierr = MPI_Finalize();
}
