/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"


#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FINALIZE = mpi_finalize_f
#pragma weak pmpi_finalize = mpi_finalize_f
#pragma weak pmpi_finalize_ = mpi_finalize_f
#pragma weak pmpi_finalize__ = mpi_finalize_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FINALIZE,
            pmpi_finalize,
            pmpi_finalize_,
            pmpi_finalize__,
            pmpi_finalize_f,
            (MPI_Fint *ierror),
            (ierror))
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FINALIZE = mpi_finalize_f
#pragma weak mpi_finalize = mpi_finalize_f
#pragma weak mpi_finalize_ = mpi_finalize_f
#pragma weak mpi_finalize__ = mpi_finalize_f
#endif

#if ! LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
LAM_GENERATE_F77_BINDINGS (MPI_FINALIZE,
            mpi_finalize,
            mpi_finalize_,
            mpi_finalize__,
            mpi_finalize_f,
            (MPI_Fint *ierror),
            (ierror))
#endif
    

void
mpi_finalize_f(MPI_Fint *ierror)
{
  *ierror = MPI_Finalize();
}
