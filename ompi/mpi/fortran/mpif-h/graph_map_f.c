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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GRAPH_MAP = ompi_graph_map_f
#pragma weak pmpi_graph_map = ompi_graph_map_f
#pragma weak pmpi_graph_map_ = ompi_graph_map_f
#pragma weak pmpi_graph_map__ = ompi_graph_map_f

#pragma weak PMPI_Graph_map_f = ompi_graph_map_f
#pragma weak PMPI_Graph_map_f08 = ompi_graph_map_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GRAPH_MAP,
                           pmpi_graph_map,
                           pmpi_graph_map_,
                           pmpi_graph_map__,
                           pompi_graph_map_f,
                           (MPI_Fint *comm, MPI_Fint *nnodes, MPI_Fint *indx, MPI_Fint *edges, MPI_Fint *newrank, MPI_Fint *ierr),
                           (comm, nnodes, indx, edges, newrank, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GRAPH_MAP = ompi_graph_map_f
#pragma weak mpi_graph_map = ompi_graph_map_f
#pragma weak mpi_graph_map_ = ompi_graph_map_f
#pragma weak mpi_graph_map__ = ompi_graph_map_f

#pragma weak MPI_Graph_map_f = ompi_graph_map_f
#pragma weak MPI_Graph_map_f08 = ompi_graph_map_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GRAPH_MAP,
                           mpi_graph_map,
                           mpi_graph_map_,
                           mpi_graph_map__,
                           ompi_graph_map_f,
                           (MPI_Fint *comm, MPI_Fint *nnodes, MPI_Fint *indx, MPI_Fint *edges, MPI_Fint *newrank, MPI_Fint *ierr),
                           (comm, nnodes, indx, edges, newrank, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_graph_map_f(MPI_Fint *comm, MPI_Fint *nnodes, MPI_Fint *indx,
		     MPI_Fint *edges, MPI_Fint *nrank, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    OMPI_ARRAY_NAME_DECL(indx);
    OMPI_ARRAY_NAME_DECL(edges);
    OMPI_SINGLE_NAME_DECL(nrank);

    c_comm = MPI_Comm_f2c(*comm);

    /* Number of edges is equal to the last entry in the index array */
    OMPI_ARRAY_FINT_2_INT(edges, indx[*nnodes - 1]);
    OMPI_ARRAY_FINT_2_INT(indx, *nnodes);

    c_ierr = MPI_Graph_map(c_comm, OMPI_FINT_2_INT(*nnodes),
                           OMPI_ARRAY_NAME_CONVERT(indx),
                           OMPI_ARRAY_NAME_CONVERT(edges),
                           OMPI_SINGLE_NAME_CONVERT(nrank));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(nrank);
    }
    OMPI_ARRAY_FINT_2_INT_CLEANUP(edges);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(indx);
}
