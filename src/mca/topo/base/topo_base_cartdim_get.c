/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"
#include "communicator/communicator.h"
#include "mca/topo/topo.h"

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
int topo_base_cartdim_get (MPI_Comm comm,
                           int *ndims){
 
    *ndims = comm->c_topo_comm->mtc_ndims;
    return MPI_SUCCESS;
}


