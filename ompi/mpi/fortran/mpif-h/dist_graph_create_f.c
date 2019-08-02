/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2013 Inria.  All rights reserved.
 * Copyright (c) 2011-2013 Université Bordeaux 1
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
#pragma weak PMPI_DIST_GRAPH_CREATE = ompi_dist_graph_create_f
#pragma weak pmpi_dist_graph_create = ompi_dist_graph_create_f
#pragma weak pmpi_dist_graph_create_ = ompi_dist_graph_create_f
#pragma weak pmpi_dist_graph_create__ = ompi_dist_graph_create_f

#pragma weak PMPI_Dist_graph_create_f = ompi_dist_graph_create_f
#pragma weak PMPI_Dist_graph_create_f08 = ompi_dist_graph_create_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_DIST_GRAPH_CREATE,
                            pmpi_dist_graph_create,
                            pmpi_dist_graph_create_,
                            pmpi_dist_graph_create__,
                            pompi_dist_graph_create_f,
                            (MPI_Fint *comm_old, MPI_Fint *n, MPI_Fint *sources, MPI_Fint *degrees, MPI_Fint *destinations, MPI_Fint *weights, MPI_Fint *info,  ompi_fortran_logical_t *reorder, MPI_Fint *comm_graph,  MPI_Fint *ierr),
                            (comm_old, n, sources, degrees, destinations, weights, info,  reorder, comm_graph, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_DIST_GRAPH_CREATE = ompi_dist_graph_create_f
#pragma weak mpi_dist_graph_create = ompi_dist_graph_create_f
#pragma weak mpi_dist_graph_create_ = ompi_dist_graph_create_f
#pragma weak mpi_dist_graph_create__ = ompi_dist_graph_create_f

#pragma weak MPI_Dist_graph_create_f = ompi_dist_graph_create_f
#pragma weak MPI_Dist_graph_create_f08 = ompi_dist_graph_create_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_DIST_GRAPH_CREATE,
                            mpi_dist_graph_create,
                            mpi_dist_graph_create_,
                            mpi_dist_graph_create__,
                            ompi_dist_graph_create_f,
                            (MPI_Fint *comm_old, MPI_Fint *n, MPI_Fint *sources, MPI_Fint *degrees, MPI_Fint *destinations, MPI_Fint *weights, MPI_Fint *info,  ompi_fortran_logical_t *reorder, MPI_Fint *comm_graph,  MPI_Fint *ierr),
                            (comm_old, n, sources, degrees, destinations, weights, info, reorder, comm_graph, ierr) )
#endif

#if OMPI_BUILD_MPI_PROFILING && ! OPAL_HAVE_WEAK_SYMBOLS
#define ompi_dist_graph_create_f pompi_dist_graph_create_f
#endif
#endif


void ompi_dist_graph_create_f(MPI_Fint *comm_old, MPI_Fint *n, MPI_Fint *sources,
                              MPI_Fint *degrees, MPI_Fint *destinations, MPI_Fint *weights,
                              MPI_Fint *info,  ompi_fortran_logical_t *reorder, MPI_Fint *comm_graph,
                              MPI_Fint *ierr)
{
    MPI_Comm c_comm_old, c_comm_graph;
    int count = 0, i;
    MPI_Info c_info;
    int *c_weights;
    int c_ierr;

    OMPI_ARRAY_NAME_DECL(sources);
    OMPI_ARRAY_NAME_DECL(degrees);
    OMPI_ARRAY_NAME_DECL(destinations);

    c_comm_old = PMPI_Comm_f2c(*comm_old);
    c_info = PMPI_Info_f2c(*info);
    OMPI_ARRAY_FINT_2_INT(sources, *n);
    OMPI_ARRAY_FINT_2_INT(degrees, *n);
    for( i = 0; i < OMPI_FINT_2_INT(*n); i++ )
        count += OMPI_ARRAY_NAME_CONVERT(degrees)[i];
    OMPI_ARRAY_FINT_2_INT(destinations, count);

    if (OMPI_IS_FORTRAN_UNWEIGHTED(weights)) {
        c_weights = MPI_UNWEIGHTED;
    } else if (OMPI_IS_FORTRAN_WEIGHTS_EMPTY(weights)) {
        c_weights = MPI_WEIGHTS_EMPTY;
    } else {
        OMPI_ARRAY_FINT_2_INT(weights, count);
        c_weights = OMPI_ARRAY_NAME_CONVERT(weights);
    }


    c_ierr = PMPI_Dist_graph_create(c_comm_old, OMPI_FINT_2_INT(*n), OMPI_ARRAY_NAME_CONVERT(sources),
                                    OMPI_ARRAY_NAME_CONVERT(degrees), OMPI_ARRAY_NAME_CONVERT(destinations),
                                    c_weights, c_info, OMPI_LOGICAL_2_INT(*reorder), &c_comm_graph);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (OMPI_SUCCESS == c_ierr) {
        *comm_graph = PMPI_Comm_c2f(c_comm_graph);
    }

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sources);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(degrees);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(destinations);
    if( MPI_UNWEIGHTED != c_weights && MPI_WEIGHTS_EMPTY != c_weights ) {
        OMPI_ARRAY_FINT_2_INT_CLEANUP(weights);
    }
}
