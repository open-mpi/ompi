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
#pragma weak PMPI_GRAPH_CREATE = ompi_graph_create_f
#pragma weak pmpi_graph_create = ompi_graph_create_f
#pragma weak pmpi_graph_create_ = ompi_graph_create_f
#pragma weak pmpi_graph_create__ = ompi_graph_create_f

#pragma weak PMPI_Graph_create_f = ompi_graph_create_f
#pragma weak PMPI_Graph_create_f08 = ompi_graph_create_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_GRAPH_CREATE,
                           pmpi_graph_create,
                           pmpi_graph_create_,
                           pmpi_graph_create__,
                           pompi_graph_create_f,
                           (MPI_Fint *comm_old, MPI_Fint *nnodes, MPI_Fint *indx, MPI_Fint *edges, ompi_fortran_logical_t *reorder, MPI_Fint *comm_graph, MPI_Fint *ierr),
                           (comm_old, nnodes, indx, edges, reorder, comm_graph, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GRAPH_CREATE = ompi_graph_create_f
#pragma weak mpi_graph_create = ompi_graph_create_f
#pragma weak mpi_graph_create_ = ompi_graph_create_f
#pragma weak mpi_graph_create__ = ompi_graph_create_f

#pragma weak MPI_Graph_create_f = ompi_graph_create_f
#pragma weak MPI_Graph_create_f08 = ompi_graph_create_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_GRAPH_CREATE,
                           mpi_graph_create,
                           mpi_graph_create_,
                           mpi_graph_create__,
                           ompi_graph_create_f,
                           (MPI_Fint *comm_old, MPI_Fint *nnodes, MPI_Fint *indx, MPI_Fint *edges, ompi_fortran_logical_t *reorder, MPI_Fint *comm_graph, MPI_Fint *ierr),
                           (comm_old, nnodes, indx, edges, reorder, comm_graph, ierr) )
#else
#define ompi_graph_create_f pompi_graph_create_f
#endif
#endif


void ompi_graph_create_f(MPI_Fint *comm_old, MPI_Fint *nnodes,
                        MPI_Fint *indx, MPI_Fint *edges,
                        ompi_fortran_logical_t *reorder, MPI_Fint *comm_graph,
                        MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm_old, c_comm_graph;
    OMPI_ARRAY_NAME_DECL(indx);
    OMPI_ARRAY_NAME_DECL(edges);

    c_comm_old = PMPI_Comm_f2c(*comm_old);

    OMPI_ARRAY_FINT_2_INT(indx, *nnodes);

    /* Number of edges is equal to the last entry in the index array */
    OMPI_ARRAY_FINT_2_INT(edges, indx[*nnodes - 1]);

    c_ierr = PMPI_Graph_create(c_comm_old,
                              OMPI_FINT_2_INT(*nnodes),
                              OMPI_ARRAY_NAME_CONVERT(indx),
                              OMPI_ARRAY_NAME_CONVERT(edges),
                              OMPI_LOGICAL_2_INT(*reorder),
                              &c_comm_graph);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (OMPI_SUCCESS == c_ierr) {
        *comm_graph = PMPI_Comm_c2f(c_comm_graph);
    }

    OMPI_ARRAY_FINT_2_INT_CLEANUP(indx);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(edges);
}
