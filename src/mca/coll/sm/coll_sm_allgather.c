/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "include/constants.h"
#include "coll_sm.h"


/*
 *	allgather
 *
 *	Function:	- allgather
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_sm_allgather_intra(void *sbuf, int scount,
                                struct ompi_datatype_t *sdtype, void *rbuf,
                                int rcount, struct ompi_datatype_t *rdtype,
                                struct ompi_communicator_t *comm) 
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
