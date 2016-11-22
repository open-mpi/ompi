/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_GRAPH_NEIGHBORS_COUNT = ompi_graph_neighbors_count_f
#pragma weak pmpi_graph_neighbors_count = ompi_graph_neighbors_count_f
#pragma weak pmpi_graph_neighbors_count_ = ompi_graph_neighbors_count_f
#pragma weak pmpi_graph_neighbors_count__ = ompi_graph_neighbors_count_f

#pragma weak PMPI_Graph_neighbors_count_f = ompi_graph_neighbors_count_f
#pragma weak PMPI_Graph_neighbors_count_f08 = ompi_graph_neighbors_count_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_GRAPH_NEIGHBORS_COUNT,
                           pmpi_graph_neighbors_count,
                           pmpi_graph_neighbors_count_,
                           pmpi_graph_neighbors_count__,
                           pompi_graph_neighbors_count_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *nneighbors, MPI_Fint *ierr),
                           (comm, rank, nneighbors, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GRAPH_NEIGHBORS_COUNT = ompi_graph_neighbors_count_f
#pragma weak mpi_graph_neighbors_count = ompi_graph_neighbors_count_f
#pragma weak mpi_graph_neighbors_count_ = ompi_graph_neighbors_count_f
#pragma weak mpi_graph_neighbors_count__ = ompi_graph_neighbors_count_f

#pragma weak MPI_Graph_neighbors_count_f = ompi_graph_neighbors_count_f
#pragma weak MPI_Graph_neighbors_count_f08 = ompi_graph_neighbors_count_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_GRAPH_NEIGHBORS_COUNT,
                           mpi_graph_neighbors_count,
                           mpi_graph_neighbors_count_,
                           mpi_graph_neighbors_count__,
                           ompi_graph_neighbors_count_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *nneighbors, MPI_Fint *ierr),
                           (comm, rank, nneighbors, ierr) )
#else
#define ompi_graph_neighbors_count_f pompi_graph_neighbors_count_f
#endif
#endif


void ompi_graph_neighbors_count_f(MPI_Fint *comm, MPI_Fint *rank,
				 MPI_Fint *nneighbors, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    OMPI_SINGLE_NAME_DECL(nneighbors);

    c_comm = PMPI_Comm_f2c(*comm);

    c_ierr = PMPI_Graph_neighbors_count(c_comm,
                                       OMPI_FINT_2_INT(*rank),
                                       OMPI_SINGLE_NAME_CONVERT(nneighbors));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(nneighbors);
    }
}
