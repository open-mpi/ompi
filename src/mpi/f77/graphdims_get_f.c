/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GRAPHDIMS_GET = mpi_graphdims_get_f
#pragma weak pmpi_graphdims_get = mpi_graphdims_get_f
#pragma weak pmpi_graphdims_get_ = mpi_graphdims_get_f
#pragma weak pmpi_graphdims_get__ = mpi_graphdims_get_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GRAPHDIMS_GET,
                           pmpi_graphdims_get,
                           pmpi_graphdims_get_,
                           pmpi_graphdims_get__,
                           pmpi_graphdims_get_f,
                           (MPI_Fint *comm, MPI_Fint *nnodes, MPI_Fint *nedges, MPI_Fint *ierr),
                           (comm, nnodes, nedges, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GRAPHDIMS_GET = mpi_graphdims_get_f
#pragma weak mpi_graphdims_get = mpi_graphdims_get_f
#pragma weak mpi_graphdims_get_ = mpi_graphdims_get_f
#pragma weak mpi_graphdims_get__ = mpi_graphdims_get_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GRAPHDIMS_GET,
                           mpi_graphdims_get,
                           mpi_graphdims_get_,
                           mpi_graphdims_get__,
                           mpi_graphdims_get_f,
                           (MPI_Fint *comm, MPI_Fint *nnodes, MPI_Fint *nedges, MPI_Fint *ierr),
                           (comm, nnodes, nedges, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_graphdims_get_f(MPI_Fint *comm, MPI_Fint *nnodes, MPI_Fint *nedges, MPI_Fint *ierr)
{

}
