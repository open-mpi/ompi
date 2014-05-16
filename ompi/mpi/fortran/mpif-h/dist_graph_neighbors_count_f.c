/*
 * Copyright (c) 2011-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2013 Inria.  All rights reserved.
 * Copyright (c) 2011-2013 Université Bordeaux 1
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_DIST_GRAPH_NEIGHBORS_COUNT = ompi_dist_graph_neighbors_count_f
#pragma weak pmpi_dist_graph_neighbors_count = ompi_dist_graph_neighbors_count_f
#pragma weak pmpi_dist_graph_neighbors_count_ = ompi_dist_graph_neighbors_count_f
#pragma weak pmpi_dist_graph_neighbors_count__ = ompi_dist_graph_neighbors_count_f

#pragma weak PMPI_Dist_graph_neighbors_count_f = ompi_dist_graph_neighbors_count_f
#pragma weak PMPI_Dist_graph_neighbors_count_f08 = ompi_dist_graph_neighbors_count_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_DIST_GRAPH_NEIGHBORS_COUNT,
                            pmpi_dist_graph_neighbors_count,
                            pmpi_dist_graph_neighbors_count_,
                            pmpi_dist_graph_neighbors_count__,
                            pompi_dist_graph_neighbors_count_f,
                            (MPI_Fint *comm, MPI_Fint *inneighbors, MPI_Fint *outneighbors, ompi_fortran_logical_t *weighted, MPI_Fint *ierr),
                            (comm, inneighbors, outneighbors, weighted, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_DIST_GRAPH_NEIGHBORS_COUNT = ompi_dist_graph_neighbors_count_f
#pragma weak mpi_dist_graph_neighbors_count = ompi_dist_graph_neighbors_count_f
#pragma weak mpi_dist_graph_neighbors_count_ = ompi_dist_graph_neighbors_count_f
#pragma weak mpi_dist_graph_neighbors_count__ = ompi_dist_graph_neighbors_count_f

#pragma weak MPI_Dist_graph_neighbors_count_f = ompi_dist_graph_neighbors_count_f
#pragma weak MPI_Dist_graph_neighbors_count_f08 = ompi_dist_graph_neighbors_count_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_DIST_GRAPH_NEIGHBORS_COUNT,
                            mpi_dist_graph_neighbors_count,
                            mpi_dist_graph_neighbors_count_,
                            mpi_dist_graph_neighbors_count__,
                            ompi_dist_graph_neighbors_count_f,
                            (MPI_Fint *comm, MPI_Fint *inneighbors, MPI_Fint *outneighbors, ompi_fortran_logical_t *weighted, MPI_Fint *ierr),
                            (comm, inneighbors, outneighbors, weighted, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_dist_graph_neighbors_count_f(MPI_Fint *comm, MPI_Fint *inneighbors,
                                       MPI_Fint *outneighbors, ompi_fortran_logical_t *weighted,
                                       MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    OMPI_SINGLE_NAME_DECL(inneighbors);
    OMPI_SINGLE_NAME_DECL(outneighbors);
    OMPI_LOGICAL_NAME_DECL(weighted);

    c_comm = MPI_Comm_f2c(*comm);

    *ierr = OMPI_INT_2_FINT(MPI_Dist_graph_neighbors_count(c_comm, 
                                                           OMPI_SINGLE_NAME_CONVERT(inneighbors),
                                                           OMPI_SINGLE_NAME_CONVERT(outneighbors),
                                                           OMPI_LOGICAL_SINGLE_NAME_CONVERT(weighted)));
    OMPI_SINGLE_INT_2_LOGICAL(weighted);
    if (OMPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
          OMPI_SINGLE_INT_2_FINT(inneighbors);
          OMPI_SINGLE_INT_2_FINT(outneighbors);
    }
}

