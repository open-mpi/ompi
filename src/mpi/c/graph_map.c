/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"
#include "mca/topo/topo.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Graph_map = PMPI_Graph_map
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Graph_map";


int MPI_Graph_map(MPI_Comm comm, int nnodes, int *index, int *edges,
                  int *newrank) 
{
    int err;
    mca_topo_base_graph_map_fn_t func;

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (MPI_COMM_NULL == comm) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                           FUNC_NAME);
        }
        if (OMPI_COMM_IS_INTER(comm)) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                           FUNC_NAME);
        }
        if (1 > nnodes || NULL == index || NULL == edges || NULL == newrank) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                           FUNC_NAME);
        }
    }
    /* map the function pointer to do the right thing */
    func = comm->c_topo->topo_graph_map;
    if (NULL == func) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, 
                                      FUNC_NAME);
    }

    /* call the function */
    if ( MPI_SUCCESS != 
            (err = func(comm, nnodes, index, edges, newrank))) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err, FUNC_NAME);
    }
    
    /* All done */
    return MPI_SUCCESS;
}
