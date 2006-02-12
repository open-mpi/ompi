/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/topo/topo.h"

/*
 * function - retrieves Cartesian topology information associated with a
 *            communicator
 *
 * @param comm communicator with cartesian structure (handle)
 * @param maxdims length of vectors  'dims', 'periods', and 'coords'
 *                 in the calling program (integer)
 * @param dims number of processes for each cartesian dimension (array of integer)
 * @param periods periodicity (true/false) for each cartesian dimension
 *                (array of logical)
 * @param coords coordinates of calling process in cartesian structure
 *               (array of integer)
 *
 * @retval MPI_SUCCESS
 */
int mca_topo_base_cart_get (MPI_Comm comm,
                        int maxdims,
                        int *dims,
                        int *periods,
                        int *coords){
    int i;
    int *d;
    int *c;

    d = comm->c_topo_comm->mtc_dims_or_index;
    c = comm->c_topo_comm->mtc_coords;

    for (i = 0; (i < comm->c_topo_comm->mtc_ndims_or_nnodes) && (i < maxdims); ++i) {
         
        if (*d > 0) {
            *dims++ = *d++;
            *periods++ = 0;
        } else {
           *dims++ = -(*d++);
           *periods++ = 1;
        }
        *coords++ = *c++;
    }

    return MPI_SUCCESS;
}
