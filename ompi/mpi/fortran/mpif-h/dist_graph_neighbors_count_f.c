/*
 * Copyright (c) 2011-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2013 Inria.  All rights reserved.
 * Copyright (c) 2011-2013 Université Bordeaux 1
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_DIST_GRAPH_NEIGHBORS_COUNT = ompi_dist_graph_neighbors_count_f
#pragma weak pmpi_dist_graph_neighbors_count = ompi_dist_graph_neighbors_count_f
#pragma weak pmpi_dist_graph_neighbors_count_ = ompi_dist_graph_neighbors_count_f
#pragma weak pmpi_dist_graph_neighbors_count__ = ompi_dist_graph_neighbors_count_f

#pragma weak PMPI_Dist_graph_neighbors_count_f = ompi_dist_graph_neighbors_count_f
#pragma weak PMPI_Dist_graph_neighbors_count_f08 = ompi_dist_graph_neighbors_count_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_DIST_GRAPH_NEIGHBORS_COUNT,
                            pmpi_dist_graph_neighbors_count,
                            pmpi_dist_graph_neighbors_count_,
                            pmpi_dist_graph_neighbors_count__,
                            pompi_dist_graph_neighbors_count_f,
                            (MPI_Fint *comm, MPI_Fint *inneighbors, MPI_Fint *outneighbors, ompi_fortran_logical_t *weighted, MPI_Fint *ierr),
                            (comm, inneighbors, outneighbors, weighted, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_DIST_GRAPH_NEIGHBORS_COUNT = ompi_dist_graph_neighbors_count_f
#pragma weak mpi_dist_graph_neighbors_count = ompi_dist_graph_neighbors_count_f
#pragma weak mpi_dist_graph_neighbors_count_ = ompi_dist_graph_neighbors_count_f
#pragma weak mpi_dist_graph_neighbors_count__ = ompi_dist_graph_neighbors_count_f

#pragma weak MPI_Dist_graph_neighbors_count_f = ompi_dist_graph_neighbors_count_f
#pragma weak MPI_Dist_graph_neighbors_count_f08 = ompi_dist_graph_neighbors_count_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_DIST_GRAPH_NEIGHBORS_COUNT,
                            mpi_dist_graph_neighbors_count,
                            mpi_dist_graph_neighbors_count_,
                            mpi_dist_graph_neighbors_count__,
                            ompi_dist_graph_neighbors_count_f,
                            (MPI_Fint *comm, MPI_Fint *inneighbors, MPI_Fint *outneighbors, ompi_fortran_logical_t *weighted, MPI_Fint *ierr),
                            (comm, inneighbors, outneighbors, weighted, ierr) )
#else
#define ompi_dist_graph_neighbors_count_f pompi_dist_graph_neighbors_count_f
#endif
#endif


void ompi_dist_graph_neighbors_count_f(MPI_Fint *comm, MPI_Fint *inneighbors,
                                       MPI_Fint *outneighbors, ompi_fortran_logical_t *weighted,
                                       MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    OMPI_SINGLE_NAME_DECL(inneighbors);
    OMPI_SINGLE_NAME_DECL(outneighbors);
    OMPI_LOGICAL_NAME_DECL(weighted);
    int c_ierr;

    c_comm = PMPI_Comm_f2c(*comm);

    c_ierr = PMPI_Dist_graph_neighbors_count(c_comm,
                                             OMPI_SINGLE_NAME_CONVERT(inneighbors),
                                             OMPI_SINGLE_NAME_CONVERT(outneighbors),
                                             OMPI_LOGICAL_SINGLE_NAME_CONVERT(weighted));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    OMPI_SINGLE_INT_2_LOGICAL(weighted);
    if (OMPI_SUCCESS == c_ierr) {
          OMPI_SINGLE_INT_2_FINT(inneighbors);
          OMPI_SINGLE_INT_2_FINT(outneighbors);
    }
}

