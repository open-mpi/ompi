/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"
#include "communicator/communicator.h"
#include "mca/topo/topo.h"

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
int mca_topo_base_graphdims_get (MPI_Comm comm,
                              int *nodes,
                              int *nedges){

    *nodes = ompi_comm_size(comm);
    *nedges = comm->c_topo_comm->mtc_dims_or_index[*nodes -1]; 

    return MPI_SUCCESS;
}
