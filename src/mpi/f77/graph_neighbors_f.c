/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GRAPH_NEIGHBORS = mpi_graph_neighbors_f
#pragma weak pmpi_graph_neighbors = mpi_graph_neighbors_f
#pragma weak pmpi_graph_neighbors_ = mpi_graph_neighbors_f
#pragma weak pmpi_graph_neighbors__ = mpi_graph_neighbors_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GRAPH_NEIGHBORS,
                           pmpi_graph_neighbors,
                           pmpi_graph_neighbors_,
                           pmpi_graph_neighbors__,
                           pmpi_graph_neighbors_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxneighbors, MPI_Fint *neighbors, MPI_Fint *ierr),
                           (comm, rank, maxneighbors, neighbors, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GRAPH_NEIGHBORS = mpi_graph_neighbors_f
#pragma weak mpi_graph_neighbors = mpi_graph_neighbors_f
#pragma weak mpi_graph_neighbors_ = mpi_graph_neighbors_f
#pragma weak mpi_graph_neighbors__ = mpi_graph_neighbors_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GRAPH_NEIGHBORS,
                           mpi_graph_neighbors,
                           mpi_graph_neighbors_,
                           mpi_graph_neighbors__,
                           mpi_graph_neighbors_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxneighbors, MPI_Fint *neighbors, MPI_Fint *ierr),
                           (comm, rank, maxneighbors, neighbors, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_graph_neighbors_f(MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxneighbors, MPI_Fint *neighbors, MPI_Fint *ierr)
{

}
