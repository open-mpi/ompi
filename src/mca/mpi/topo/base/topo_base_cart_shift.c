/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"

/*
 * function - Returns the shifted source and destination ranks, given a
 *            shift direction and amount
 *
 * @param comm communicator with cartesian structure (handle)
 * @param direction coordinate directionension of shift (integer)
 * @param disp displacement (> 0: upwards shift, < 0: downwards shift) (integer)
 * @param rank_source rank of source process (integer)
 * @param rank_dest rank of destination process (integer)
 *
 * The 'direction' argument is in the range '[0,n-1]' for an n-directionensional
 * Cartesian mesh.
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_TOPOLOGY
 * @retval MPI_ERR_DIMS
 * @retval MPI_ERR_COMM
 * @retval MPI_ERR_ARG
 */                  
int topo_base_cart_shift (lam_communicator_t *comm,
                          int direction,
                          int disp,
                          int *rank_source,
                          int *rank_dest){
    int factor;
    int thisdirection = 0;
    int thisperiod = 0;
    int ord;
    int srcord;
    int destord;
    int i;
    int *p;

   /*
    * Handle the trivial case.
    */
    ord = comm->c_group->g_myrank;
    if (disp == 0) {
        *rank_dest = *rank_source = ord;
        return MPI_SUCCESS;
    }
   /*
    * Compute the rank factor and ordinate.
    */
    factor = comm->c_topo_nprocs;
    p = comm->c_topo_dims;
    for (i = 0; (i < comm->c_topo_ndims) && (i <= direction); ++i, ++p) {
        if ((thisdirection = *p) > 0) {
            thisperiod = 0;
        } else {
          thisperiod = 1;
          thisdirection = -thisdirection;
        }

        ord %= factor;
        factor /= thisdirection;
     }

    ord /= factor;
    /*
     * Check the displacement value and compute the new ranks.
     */
    *rank_source = *rank_dest = MPI_UNDEFINED;

    srcord = ord - disp;
    destord = ord + disp;
    if ( ((destord < 0) || (destord >= thisdirection)) && (!thisperiod) ) {
         *rank_dest = MPI_PROC_NULL;
    } else {
       destord %= thisdirection;
       if (destord < 0) destord += thisdirection;
       *rank_dest = comm->c_group->g_myrank + ((destord - ord) * factor);
    }
    if ( ((srcord < 0) || (srcord >= thisdirection)) && (!thisperiod) ) {
         *rank_source = MPI_PROC_NULL;
    } else {
       srcord %= thisdirection;
       if (srcord < 0) srcord += thisdirection;
       *rank_source = comm->c_group->g_myrank + ((srcord - ord) * factor);
    }

    return MPI_SUCCESS;
}
