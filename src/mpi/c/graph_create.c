/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"
#include "mca/topo/topo.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Graph_create = PMPI_Graph_create
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Graph_create(MPI_Comm comm_old, int nnodes, int *index,
                     int *edges, int reorder, MPI_Comm *comm_graph) {

    int err;
    mca_topo_base_graph_create_fn_t func;

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        if (MPI_COMM_NULL == comm_old) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Graph_create");
        }
        if (LAM_COMM_IS_INTER(comm_old)) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Graph_create");
        }
        if (1 > nnodes || NULL == index || NULL == edges) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Graph_create");
        }
    }
    /* get the function pointer to do the right thing */
    func = comm_old->c_topo.topo_graph_create;
    if (NULL == func) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, 
                                     "MPI_Graph_create");
    }

    /* call the function */
    if ( MPI_SUCCESS != 
            (err = func(comm_old, nnodes, index, edges, reorder, comm_graph))) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err, "MPI_Graph_create");
    }
    
    /* All done */
    return MPI_SUCCESS;
}
