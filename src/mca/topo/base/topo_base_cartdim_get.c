/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
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
int mca_topo_base_cartdim_get (MPI_Comm comm,
                           int *ndims){
 
    *ndims = comm->c_topo_comm->mtc_ndims_or_nnodes;
    return MPI_SUCCESS;
}


