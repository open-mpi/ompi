/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"

/*
 * function - returns the neighbors of a node associated
 *            with a graph topology
 *
 * @param comm communicator with graph topology (handle)
 * @param rank rank of process in group of comm (integer)
 * @param maxneighbors size of array neighbors (integer)
 * @param neighbors ranks of processes that are neighbors to specified process
 *               (array of integer)
 *
 * @retval MPI_SUCCESS
 */

int topo_base_graph_neighbors (lam_communicator_t *comm,
                               int rank,
                               int maxneighbors,
                               int *neighbors){
    int nnbrs;
    int i;
    int *p;

    /*
     * Fill the neighbours.
     */
     nnbrs = comm->c_topo_index[rank];
     p = comm->c_topo_edges;

     if (rank > 0) {
        i = comm->c_topo_index[rank - 1];
        nnbrs -= i;
        p += i;
     }

     for (i = 0; (i < maxneighbors) && (i < nnbrs); ++i, ++p) {
        *neighbors++ = *p;
     }

     return MPI_SUCCESS;
}
