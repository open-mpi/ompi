/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "coll_sm.h"


/*      
 *      bcast
 *
 *      Function: - broadcast
 *      Accepts: - same as MPI_Bcast()
 *      Returns: - MPI_SUCCESS or error code
 */
int mca_coll_sm_bcast_intra(void *buff, int count, 
                            struct ompi_datatype_t *datatype, int root, 
                            struct ompi_communicator_t *comm)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
