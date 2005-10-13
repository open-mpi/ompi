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
#include "coll_hierarch.h"

#include <stdio.h>

#include "mpi.h"
#include "ompi/include/constants.h"
#include "opal/util/output.h"
#include "communicator/communicator.h"
#include "datatype/datatype.h"
#include "mca/coll/coll.h"


/*
 *	reduce_intra
 *
 *	Function:	- reduction using two level hierarchy algorithm
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_hierarch_reduce_intra(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype, 
                               struct ompi_op_t *op,
                               int root, struct ompi_communicator_t *comm)
{
    struct mca_coll_base_comm_t *data=NULL;
    struct ompi_communicator_t *llcomm=NULL;
    struct ompi_communicator_t *lcomm=NULL;
    int rank, lrank;
    int ret;
    int lleader, llroot;
    long extent, true_extent, lb, true_lb;
    char *tmpbuf=NULL, *tbuf=NULL;

    rank   = ompi_comm_rank ( comm );
    data   = comm->c_coll_selected_data;
    lcomm  = data->hier_lcomm;

    if ( MPI_COMM_NULL != lcomm ) {
	mca_coll_hierarch_get_lleader ( root, data, &lleader );
	
	lrank = ompi_comm_rank (lcomm);
	if ( lrank == lleader ) {
	    ompi_ddt_get_extent(dtype, &lb, &extent);
	    ompi_ddt_get_true_extent(dtype, &true_lb, &true_extent);

	    tbuf = malloc(true_extent + (count - 1) * extent);
	    if (NULL == tbuf) {
		return OMPI_ERR_OUT_OF_RESOURCE;
	    }
	    tmpbuf = tbuf - lb;
	}

	if ( MPI_IN_PLACE != sbuf ) {
	    ret = lcomm->c_coll.coll_reduce (sbuf, tmpbuf, count, dtype, 
					     op, lleader, lcomm);
	}
	else {
            ret = lcomm->c_coll.coll_reduce (rbuf, tmpbuf, count, dtype, 
					     op, lleader, lcomm);
	}
	if ( OMPI_SUCCESS != ret ) {
	    goto exit;
	}
    }

    llcomm = mca_coll_hierarch_get_llcomm ( root, data, &llroot);
    if ( MPI_UNDEFINED != llroot ) {
	ret = llcomm->c_coll.coll_reduce (tmpbuf, rbuf, count, dtype,
					  op, llroot, llcomm);
    }

 exit:
    if ( NULL != tmpbuf ) {
	free ( tmpbuf );
    }

    return ret;
}
