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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/mca/topo/topo.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Graph_neighbors_count = PMPI_Graph_neighbors_count
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Graph_neighbors_count";


int MPI_Graph_neighbors_count(MPI_Comm comm, int rank, int *nneighbors) 
{
    int err;
    mca_topo_base_module_graph_neighbors_count_fn_t func;

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (MPI_COMM_NULL == comm) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                           FUNC_NAME);
        }
        if (OMPI_COMM_IS_INTER(comm)) {
            return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_COMM,
                                           FUNC_NAME);
        }
        if (!OMPI_COMM_IS_GRAPH(comm)) {
            return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_TOPOLOGY,
                                           FUNC_NAME);
        }
        if ((0 > rank) || (rank > ompi_group_size(comm->c_local_group))) {
            return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_RANK,
                                           FUNC_NAME);
        }
        if (NULL == nneighbors) {
            return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_ARG,
                                           FUNC_NAME);
        }
    }
    /* get the function pointer to do the right thing */
    func = comm->c_topo->topo_graph_neighbors_count;

    /* call the function */
    if ( MPI_SUCCESS != 
            (err = func(comm, rank, nneighbors))) {
        return OMPI_ERRHANDLER_INVOKE(comm, err, FUNC_NAME);
    }
    
    /* All done */
    return MPI_SUCCESS;
}
