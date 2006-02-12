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
#pragma weak MPI_Graph_map = PMPI_Graph_map
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Graph_map";


int MPI_Graph_map(MPI_Comm comm, int nnodes, int *index, int *edges,
                  int *newrank) 
{
    int err;
    mca_topo_base_module_graph_map_fn_t func;

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
        if (1 > nnodes || NULL == index || NULL == edges || NULL == newrank) {
            return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_ARG,
                                           FUNC_NAME);
        }
    }


    if(!OMPI_COMM_IS_GRAPH(comm)) {
	/* In case the communicator has no topo-module attached to 
	   it, we just return the "default" value suggested by MPI:
	   newrank = rank */
	*newrank = ompi_comm_rank(comm);
    }
    else {
	/* map the function pointer to do the right thing */
	func = comm->c_topo->topo_graph_map;
	
	/* call the function */
	if ( MPI_SUCCESS != 
	     (err = func(comm, nnodes, index, edges, newrank))) {
	    return OMPI_ERRHANDLER_INVOKE(comm, err, FUNC_NAME);
	}
    }

    /* All done */
    return MPI_SUCCESS;
}
