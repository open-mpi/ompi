/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GRAPH_MAP = mpi_graph_map_f
#pragma weak pmpi_graph_map = mpi_graph_map_f
#pragma weak pmpi_graph_map_ = mpi_graph_map_f
#pragma weak pmpi_graph_map__ = mpi_graph_map_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GRAPH_MAP,
                           pmpi_graph_map,
                           pmpi_graph_map_,
                           pmpi_graph_map__,
                           pmpi_graph_map_f,
                           (MPI_Fint *comm, MPI_Fint *nnodes, MPI_Fint *index, MPI_Fint *edges, MPI_Fint *newrank, MPI_Fint *ierr),
                           (comm, nnodes, index, edges, newrank, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GRAPH_MAP = mpi_graph_map_f
#pragma weak mpi_graph_map = mpi_graph_map_f
#pragma weak mpi_graph_map_ = mpi_graph_map_f
#pragma weak mpi_graph_map__ = mpi_graph_map_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GRAPH_MAP,
                           mpi_graph_map,
                           mpi_graph_map_,
                           mpi_graph_map__,
                           mpi_graph_map_f,
                           (MPI_Fint *comm, MPI_Fint *nnodes, MPI_Fint *index, MPI_Fint *edges, MPI_Fint *newrank, MPI_Fint *ierr),
                           (comm, nnodes, index, edges, newrank, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_graph_map_f(MPI_Fint *comm, MPI_Fint *nnodes, MPI_Fint *index, MPI_Fint *edges, MPI_Fint *newrank, MPI_Fint *ierr)
{

}
