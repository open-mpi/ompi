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
#pragma weak MPI_Graph_get = PMPI_Graph_get
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Graph_get";


int MPI_Graph_get(MPI_Comm comm, int maxindex, int maxedges,
                  int *index, int *edges) 
{
    int err;
    mca_topo_base_module_graph_get_fn_t func;

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        if (MPI_COMM_NULL == comm) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                           FUNC_NAME);
        }
        if (OMPI_COMM_IS_INTER(comm)) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                           FUNC_NAME);
        }
        if (!OMPI_COMM_IS_GRAPH(comm)) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_TOPOLOGY,
                                           FUNC_NAME);
        }
        if (0 > maxindex || 0 > maxedges || NULL == index || NULL == edges) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_ARG,
                                           FUNC_NAME);
        }
    }
    /* get the function pointer to do the right thing */
    func = comm->c_topo->topo_graph_get;

    /* call the function */
    if ( MPI_SUCCESS != 
            (err = func(comm, maxindex, maxedges, index, edges))) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err, FUNC_NAME);
    }
    
    /* All done */
    return MPI_SUCCESS;
}
