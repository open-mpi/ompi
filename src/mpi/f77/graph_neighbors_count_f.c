/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GRAPH_NEIGHBORS_COUNT = mpi_graph_neighbors_count_f
#pragma weak pmpi_graph_neighbors_count = mpi_graph_neighbors_count_f
#pragma weak pmpi_graph_neighbors_count_ = mpi_graph_neighbors_count_f
#pragma weak pmpi_graph_neighbors_count__ = mpi_graph_neighbors_count_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GRAPH_NEIGHBORS_COUNT,
                           pmpi_graph_neighbors_count,
                           pmpi_graph_neighbors_count_,
                           pmpi_graph_neighbors_count__,
                           pmpi_graph_neighbors_count_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *nneighbors, MPI_Fint *ierr),
                           (comm, rank, nneighbors, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GRAPH_NEIGHBORS_COUNT = mpi_graph_neighbors_count_f
#pragma weak mpi_graph_neighbors_count = mpi_graph_neighbors_count_f
#pragma weak mpi_graph_neighbors_count_ = mpi_graph_neighbors_count_f
#pragma weak mpi_graph_neighbors_count__ = mpi_graph_neighbors_count_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GRAPH_NEIGHBORS_COUNT,
                           mpi_graph_neighbors_count,
                           mpi_graph_neighbors_count_,
                           mpi_graph_neighbors_count__,
                           mpi_graph_neighbors_count_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *nneighbors, MPI_Fint *ierr),
                           (comm, rank, nneighbors, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_graph_neighbors_count_f(MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *nneighbors, MPI_Fint *ierr)
{

}
