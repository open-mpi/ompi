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
#include "datatype/datatype.h"
#include "coll_self.h"


/*
 *	reduce_lin_intra
 *
 *	Function:	- reduction 
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_self_reduce_intra(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype, 
                               struct ompi_op_t *op,
                               int root, struct ompi_communicator_t *comm)
{
    return ompi_ddt_sndrcv(sbuf, count, dtype, 
                           rbuf, count, dtype);
}
