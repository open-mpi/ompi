/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_INTERCOMM_MERGE = mpi_intercomm_merge_f
#pragma weak pmpi_intercomm_merge = mpi_intercomm_merge_f
#pragma weak pmpi_intercomm_merge_ = mpi_intercomm_merge_f
#pragma weak pmpi_intercomm_merge__ = mpi_intercomm_merge_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INTERCOMM_MERGE,
                           pmpi_intercomm_merge,
                           pmpi_intercomm_merge_,
                           pmpi_intercomm_merge__,
                           pmpi_intercomm_merge_f,
                           (MPI_Fint *intercomm, MPI_Fint *high, MPI_Fint *newintercomm, MPI_Fint *ierr),
                           (intercomm, high, newintercomm, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INTERCOMM_MERGE = mpi_intercomm_merge_f
#pragma weak mpi_intercomm_merge = mpi_intercomm_merge_f
#pragma weak mpi_intercomm_merge_ = mpi_intercomm_merge_f
#pragma weak mpi_intercomm_merge__ = mpi_intercomm_merge_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INTERCOMM_MERGE,
                           mpi_intercomm_merge,
                           mpi_intercomm_merge_,
                           mpi_intercomm_merge__,
                           mpi_intercomm_merge_f,
                           (MPI_Fint *intercomm, MPI_Fint *high, MPI_Fint *newintercomm, MPI_Fint *ierr),
                           (intercomm, high, newintercomm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_intercomm_merge_f(MPI_Fint *intercomm, MPI_Fint *high, MPI_Fint *newintercomm, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}
