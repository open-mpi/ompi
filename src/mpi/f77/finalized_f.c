/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FINALIZED = mpi_finalized_f
#pragma weak pmpi_finalized = mpi_finalized_f
#pragma weak pmpi_finalized_ = mpi_finalized_f
#pragma weak pmpi_finalized__ = mpi_finalized_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FINALIZED,
                           pmpi_finalized,
                           pmpi_finalized_,
                           pmpi_finalized__,
                           pmpi_finalized_f,
                           (MPI_Fint *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FINALIZED = mpi_finalized_f
#pragma weak mpi_finalized = mpi_finalized_f
#pragma weak mpi_finalized_ = mpi_finalized_f
#pragma weak mpi_finalized__ = mpi_finalized_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FINALIZED,
                           mpi_finalized,
                           mpi_finalized_,
                           mpi_finalized__,
                           mpi_finalized_f,
                           (MPI_Fint *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_finalized_f(MPI_Fint *flag, MPI_Fint *ierr)
{
    OMPI_SINGLE_NAME_DECL(flag);
    *ierr = OMPI_INT_2_FINT(MPI_Finalized(OMPI_SINGLE_NAME_CONVERT(flag)));
    OMPI_SINGLE_INT_2_FINT(flag);
}
