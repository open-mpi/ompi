/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"

/*
 * function - determines process coords in cartesian topology given
 *            rank in group
 *
 * @param comm - communicator with cartesian structure (handle)
 * @param rank - rank of a process within group of 'comm' (integer)
 * @param maxdims - length of vector 'coords' in the calling program (integer)
 * @param coords - integer array (of size 'ndims') containing the cartesian
 *                   coordinates of specified process (integer)
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_COMM
 * @retval MPI_ERR_TOPOLOGY
 * @retval MPI_ERR_RANK
 * @retval MPI_ERR_DIMS
 * @retval MPI_ERR_ARG
 */                   

int topo_base_cart_coords (lam_communicator_t *comm,
                           int rank,
                           int maxdims,
                           int *coords){
    int dim;
    int remprocs;
    int i;
    int *d;

    /*
     * loop computing the co-ordinates
     */ 
    d = comm->c_topo_dims;
    remprocs = comm->c_topo_nprocs;
    for (i = 0; (i < comm->c_topo_ndims) && (i < maxdims); ++i, ++d) {
        dim = (*d > 0) ? *d : -(*d);
        remprocs /= dim;
        *coords++ = rank / remprocs;
        rank %= remprocs;
    }
    return MPI_SUCCESS;
}
