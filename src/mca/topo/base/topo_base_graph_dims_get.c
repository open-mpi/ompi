/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"

/*
 * function - Retrieves graph topology information associated with a
 *            communicator
 *            
 * @param comm - communicator for group with graph structure (handle)
 * @param nodes - number of nodes in graph (integer)
 * @param nedges - number of edges in graph (integer)
 * 
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_TOPOLOGY
 * @retval MPI_ERR_COMM
 * @retval MPI_ERR_ARG
 */
int topo_base_graph_dims_get (lam_communicator_t *comm,
                              int *nodes,
                              int *nedges){

    *nodes = comm->c_topo_nprocs;
    *nedges = comm->c_topo_nedges;

    return MPI_SUCCESS;
}

    


