/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"
#include "communicator/communicator.h"
#include "mca/topo/topo.h"

/*
 * function - Determines process rank in communicator given Cartesian
 *                 location
 *
 * @param comm communicator with cartesian structure (handle)
 * @param coords integer array (of size  'ndims') specifying the cartesian
 *               coordinates of a process
 * @param rank rank of specified process (integer)
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_COMM
 * @retval MPI_ERR_TOPOLOGY
 * @retval MPI_ERR_ARG
 */                   

int mca_topo_base_cart_rank (MPI_Comm comm, 
                         int *coords, 
                         int *rank){
   int prank;
   int dim;
   int ord;
   int factor;
   int i;
   int *d;
   int *c;

   /*
    * Loop over coordinates computing the rank.
    */
    factor = 1;
    prank = 0;

    i = comm->c_topo_comm->mtc_ndims_or_nnodes - 1;
    d = comm->c_topo_comm->mtc_dims_or_index + i;
    c = coords + i;

   for (; i >= 0; --i, --c, --d) {
       dim = (*d > 0) ? *d : -(*d);
       ord = *c;
        if ((ord < 0) || (ord >= dim)) {
          if (*d > 0) {
             return MPI_ERR_ARG;
          }
          ord %= dim;
          if (ord < 0) {
             ord += dim;
          }
       }
       prank += factor * ord;
       factor *= dim;
    }
    *rank = prank;

    return(MPI_SUCCESS);
}
