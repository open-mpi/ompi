/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"
#include "communicator/communicator.h"
#include "mca/topo/topo.h"

/*
 * function - retrieves graph topology information associated with a
 *            communicator
 *
 * @param comm communicator with graph structure (handle)
 * @param maxindex length of vector 'index' in the calling program  (integer)
 * @param maxedges length of vector 'edges' in the calling program  (integer)
 * @param nodes array of integers containing the graph structure (for details see
 * @param edges array of integers containing the graph structure
 *
 * @retval MPI_SUCCESS
 */                 

int topo_base_graph_get (MPI_Comm comm,
                         int maxindex,
                         int maxedges,
                         int *index,
                         int *edges){
    int i;
    int *p;

    /*
     * Fill the nodes and edges arrays.
     */
     p = comm->c_topo_comm->mtc_index;
     for (i = 0; (i < comm->c_topo_comm->mtc_nprocs) && (i < maxindex); ++i, ++p) {
         *index++ = *p;
      }

      p = comm->c_topo_comm->mtc_edges;
      for (i = 0; (i < comm->c_topo_comm->mtc_nedges) && (i < maxedges); ++i, ++p) {
         *edges++ = *p;
      }

      return MPI_SUCCESS;
}
