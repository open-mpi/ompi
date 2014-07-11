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
#pragma weak PMPI_DIST_GRAPH_NEIGHBORS = ompi_dist_graph_neighbors_f
#pragma weak pmpi_dist_graph_neighbors = ompi_dist_graph_neighbors_f
#pragma weak pmpi_dist_graph_neighbors_ = ompi_dist_graph_neighbors_f
#pragma weak pmpi_dist_graph_neighbors__ = ompi_dist_graph_neighbors_f

#pragma weak PMPI_Dist_graph_neighbors_f = ompi_dist_graph_neighbors_f
#pragma weak PMPI_Dist_graph_neighbors_f08 = ompi_dist_graph_neighbors_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_DIST_GRAPH_NEIGHBORS,
                            pmpi_dist_graph_neighbors,
                            pmpi_dist_graph_neighbors_,
                            pmpi_dist_graph_neighbors__,
                            pompi_dist_graph_neighbors_f,
                            (MPI_Fint* comm, MPI_Fint* maxindegree, MPI_Fint* sources, MPI_Fint* sourceweights, MPI_Fint* maxoutdegree, MPI_Fint* destinations, MPI_Fint* destweights, MPI_Fint *ierr),
                            (comm, maxindegree, sources, sourceweights, maxoutdegree, destinations, destweights, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_DIST_GRAPH_NEIGHBORS = ompi_dist_graph_neighbors_f
#pragma weak mpi_dist_graph_neighbors = ompi_dist_graph_neighbors_f
#pragma weak mpi_dist_graph_neighbors_ = ompi_dist_graph_neighbors_f
#pragma weak mpi_dist_graph_neighbors__ = ompi_dist_graph_neighbors_f

#pragma weak MPI_Dist_graph_neighbors_f = ompi_dist_graph_neighbors_f
#pragma weak MPI_Dist_graph_neighbors_f08 = ompi_dist_graph_neighbors_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_DIST_GRAPH_NEIGHBORS,
                            mpi_dist_graph_neighbors,
                            mpi_dist_graph_neighbors_,
                            mpi_dist_graph_neighbors__,
                            ompi_dist_graph_neighbors_f,
                            (MPI_Fint* comm, MPI_Fint* maxindegree, MPI_Fint* sources, MPI_Fint* sourceweights, MPI_Fint* maxoutdegree, MPI_Fint* destinations, MPI_Fint* destweights, MPI_Fint *ierr),
                            (comm, maxindegree, sources, sourceweights, maxoutdegree, destinations, destweights, ierr) )
#endif

#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_dist_graph_neighbors_f(MPI_Fint* comm, MPI_Fint* maxindegree,
                                 MPI_Fint* sources, MPI_Fint* sourceweights,
                                 MPI_Fint* maxoutdegree, MPI_Fint* destinations,
                                 MPI_Fint* destweights,
                                 MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    OMPI_ARRAY_NAME_DECL(sources);
    OMPI_ARRAY_NAME_DECL(sourceweights);
    OMPI_ARRAY_NAME_DECL(destinations);
    OMPI_ARRAY_NAME_DECL(destweights);

    c_comm = MPI_Comm_f2c(*comm);

    OMPI_ARRAY_FINT_2_INT_ALLOC(sources, *maxindegree);
    if( !OMPI_IS_FORTRAN_UNWEIGHTED(sourceweights) ) {
        OMPI_ARRAY_FINT_2_INT_ALLOC(sourceweights, *maxindegree);
    }
    OMPI_ARRAY_FINT_2_INT_ALLOC(destinations, *maxoutdegree);
    if( !OMPI_IS_FORTRAN_UNWEIGHTED(destweights) ) {
        OMPI_ARRAY_FINT_2_INT_ALLOC(destweights, *maxoutdegree);
    }

    *ierr = OMPI_INT_2_FINT(MPI_Dist_graph_neighbors(c_comm, OMPI_FINT_2_INT(*maxindegree),
                                                     OMPI_ARRAY_NAME_CONVERT(sources),
                                                     OMPI_IS_FORTRAN_UNWEIGHTED(sourceweights) ? MPI_UNWEIGHTED : OMPI_ARRAY_NAME_CONVERT(sourceweights),
                                                     OMPI_FINT_2_INT(*maxoutdegree), OMPI_ARRAY_NAME_CONVERT(destinations),
                                                     OMPI_IS_FORTRAN_UNWEIGHTED(destweights) ? MPI_UNWEIGHTED : OMPI_ARRAY_NAME_CONVERT(destweights)));
    if (OMPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_ARRAY_INT_2_FINT(sources, *maxindegree);
        if( !OMPI_IS_FORTRAN_UNWEIGHTED(sourceweights) ) {
            OMPI_ARRAY_INT_2_FINT(sourceweights, *maxindegree);
        }
        OMPI_ARRAY_INT_2_FINT(destinations, *maxoutdegree);
        if( !OMPI_IS_FORTRAN_UNWEIGHTED(destweights) ) {
            OMPI_ARRAY_INT_2_FINT(destweights, *maxoutdegree);
        }
    } else {
        OMPI_ARRAY_FINT_2_INT_CLEANUP(sources);
        if( !OMPI_IS_FORTRAN_UNWEIGHTED(sourceweights) ) {
            OMPI_ARRAY_FINT_2_INT_CLEANUP(sourceweights);
        }
        OMPI_ARRAY_FINT_2_INT_CLEANUP(destinations);
        if( !OMPI_IS_FORTRAN_UNWEIGHTED(destweights) ) {
            OMPI_ARRAY_FINT_2_INT_CLEANUP(destweights);
        }
    }
}

