/*
 * $HEADER$
 */ 

#include "ompi_config.h"
#include "mca/topo/unity/src/topo_unity.h"

#include "communicator/communicator.h"

/*
 * function - mca_topo_unity_cart_map
 *
 *  @param comm input communicator (handle)
 *  @param ndims number of dimensions of cartesian structure (integer)
 *  @param dims integer array of size 'ndims' specifying the number of
 *              processes in each coordinate direction
 *  @param periods logical array of size 'ndims' specifying the 
 *                 periodicity specification in each coordinate direction
 *  @param newrank reordered rank of the calling process; 'MPI_UNDEFINED' 
 *                 if calling process does not belong to grid (integer)
 *
 *  @retval MPI_SUCCESS               
 *  @retval MPI_ERR_DIMS               
 */

int mca_topo_unity_cart_map (MPI_Comm comm,
                             int ndims,
                             int *dims,
                             int *periods,
                             int *newrank)
{
    int nprocs;
    int rank;
    int size;
    int i;
    int *p;

    /*
     * Compute the # of processes in the grid.
     */
    nprocs = 1;
    for (i = 0, p = dims; i < ndims; ++i, ++p) {
        if (*p <= 0) {
            return MPI_ERR_DIMS;
        }
        nprocs *= *p;
    }
    /*
     * Check that number of processes <= size of communicator.
     */
    size = ompi_comm_size(comm);
    if (nprocs > size) {
        return MPI_ERR_DIMS;
    }
    /*
     * Compute my new rank.
     */
    rank = ompi_comm_rank(comm);
    *newrank = ((rank < 0) || (rank >= nprocs)) ? MPI_UNDEFINED : rank;

    return MPI_SUCCESS;
}
