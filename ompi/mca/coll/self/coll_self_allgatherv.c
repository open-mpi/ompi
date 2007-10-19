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

#include "ompi/constants.h"
#include "ompi/datatype/datatype.h"
#include "coll_self.h"


/*
 *	allgatherv_intra
 *
 *	Function:	- allgather
 *	Accepts:	- same as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_self_allgatherv_intra(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, 
                                   void * rbuf, int *rcounts, int *disps, 
                                   struct ompi_datatype_t *rdtype, 
                                   struct ompi_communicator_t *comm)
{
    if (MPI_IN_PLACE == sbuf) {
        return MPI_SUCCESS;
    } else {
        int err;        
        ptrdiff_t lb, extent;
        err = ompi_ddt_get_extent(rdtype, &lb, &extent);
        if (OMPI_SUCCESS != err) {
            return OMPI_ERROR;
        }
        return ompi_ddt_sndrcv(sbuf, scount, sdtype,
                               ((char *) rbuf) + disps[0], rcounts[0], rdtype);
    }
}
