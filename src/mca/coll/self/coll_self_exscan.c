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
#include "coll_self.h"


/*
 *	exscan_intra
 *
 *	Function:	- exscan
 *	Accepts:	- same arguments as MPI_Exccan()
 *	Returns:	- MPI_SUCCESS
 */
int mca_coll_self_exscan_intra(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype, 
                               struct ompi_op_t *op, 
                               struct ompi_communicator_t *comm)
{
    /* Since there's only one process, there's nothing to do */

    return MPI_SUCCESS;
}
