/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"

/*
 * function - retrieves Cartesian topology information associated with a
 *            communicator
 *
 * @param comm communicator with cartesian structure (handle)
 * @param ndims number of dimensions of the cartesian structure (integer)
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_COMM
 */
int topo_base_cartdim_get (lam_communicator_t *comm,
                           int *ndims){
 
    *ndims = comm->c_topo_ndims;
    return MPI_SUCCESS;
}


