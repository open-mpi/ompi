/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GRAPH_GET = mpi_graph_get_f
#pragma weak pmpi_graph_get = mpi_graph_get_f
#pragma weak pmpi_graph_get_ = mpi_graph_get_f
#pragma weak pmpi_graph_get__ = mpi_graph_get_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GRAPH_GET,
                           pmpi_graph_get,
                           pmpi_graph_get_,
                           pmpi_graph_get__,
                           pmpi_graph_get_f,
                           (MPI_Fint *comm, MPI_Fint *maxindex, MPI_Fint *maxedges, MPI_Fint *index, MPI_Fint *edges, MPI_Fint *ierr),
                           (comm, maxindex, maxedges, index, edges, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GRAPH_GET = mpi_graph_get_f
#pragma weak mpi_graph_get = mpi_graph_get_f
#pragma weak mpi_graph_get_ = mpi_graph_get_f
#pragma weak mpi_graph_get__ = mpi_graph_get_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GRAPH_GET,
                           mpi_graph_get,
                           mpi_graph_get_,
                           mpi_graph_get__,
                           mpi_graph_get_f,
                           (MPI_Fint *comm, MPI_Fint *maxindex, MPI_Fint *maxedges, MPI_Fint *index, MPI_Fint *edges, MPI_Fint *ierr),
                           (comm, maxindex, maxedges, index, edges, ierr) )
#endif

void mpi_graph_get_f(MPI_Fint *comm, MPI_Fint *maxindex, MPI_Fint *maxedges, MPI_Fint *index, MPI_Fint *edges, MPI_Fint *ierr)
{

}
