/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GRAPH_CREATE = mpi_graph_create_f
#pragma weak pmpi_graph_create = mpi_graph_create_f
#pragma weak pmpi_graph_create_ = mpi_graph_create_f
#pragma weak pmpi_graph_create__ = mpi_graph_create_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GRAPH_CREATE,
                           pmpi_graph_create,
                           pmpi_graph_create_,
                           pmpi_graph_create__,
                           pmpi_graph_create_f,
                           (MPI_Fint *comm_old, MPI_Fint *nnodes, MPI_Fint *index, MPI_Fint *edges, MPI_Fint *reorder, MPI_Fint *comm_graph, MPI_Fint *ierr),
                           (comm_old, nnodes, index, edges, reorder, comm_graph, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GRAPH_CREATE = mpi_graph_create_f
#pragma weak mpi_graph_create = mpi_graph_create_f
#pragma weak mpi_graph_create_ = mpi_graph_create_f
#pragma weak mpi_graph_create__ = mpi_graph_create_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GRAPH_CREATE,
                           mpi_graph_create,
                           mpi_graph_create_,
                           mpi_graph_create__,
                           mpi_graph_create_f,
                           (MPI_Fint *comm_old, MPI_Fint *nnodes, MPI_Fint *index, MPI_Fint *edges, MPI_Fint *reorder, MPI_Fint *comm_graph, MPI_Fint *ierr),
                           (comm_old, nnodes, index, edges, reorder, comm_graph, ierr) )
#endif

void mpi_graph_create_f(MPI_Fint *comm_old, MPI_Fint *nnodes, MPI_Fint *index, MPI_Fint *edges, MPI_Fint *reorder, MPI_Fint *comm_graph, MPI_Fint *ierr)
{

}
