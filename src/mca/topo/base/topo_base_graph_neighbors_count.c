/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"

/*
 * function - returns the number of neighbors of a node
 *            associated with a graph topology
 *
 * @param comm communicator with graph topology (handle)
 * @param rank rank of process in group of 'comm' (integer)
 * @param nneighbors number of neighbors of specified process (integer)
 * 
 * @retval MPI_SUCCESS
 */                             

int topo_base_graph_neighbors_count (lam_communicator_t *comm,
                                     int rank,
                                     int *nneighbors){
   *nneighbors = comm->c_topo_index[rank];
   if (rank > 0) {
      *nneighbors -= comm->c_topo_index[rank - 1];
    }

    return MPI_SUCCESS;
}
