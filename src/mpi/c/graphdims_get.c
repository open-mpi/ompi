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
#pragma weak MPI_Graphdims_get = PMPI_Graphdims_get
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Graphdims_get(MPI_Comm comm, int *nnodes, int *nedges) {
    int err;
    mca_topo_base_graphdims_get_fn_t func;

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        if (MPI_COMM_NULL == comm) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Graphdims_get");
        }
        if (LAM_COMM_IS_INTER(comm)) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          "MPI_Graphdims_get");
        }
        if (!LAM_COMM_IS_GRAPH(comm)) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_TOPOLOGY,
                                          "MPI_Graphdims_get");
        }
        if (NULL == nnodes || NULL == nedges) {
            return LAM_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                          "MPI_Graphdims_get");
        }
    }
    /* get the function pointer to do the right thing */
    func = comm->c_topo.topo_graphdims_get;
    if (NULL == func) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, 
                                     "MPI_Graphdims_get");
    }

    /* call the function */
    if ( MPI_SUCCESS != 
            (err = func(comm, nnodes, nedges))) {
        return LAM_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err, "MPI_Graphdims_get");
    }
    
    /* All done */
    return MPI_SUCCESS;
}
