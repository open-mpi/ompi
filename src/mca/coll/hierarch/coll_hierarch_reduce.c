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
#include "include/constants.h"
#include "util/output.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_hierarch.h"


#ifdef SIMPLE_HIERARCH
/*
 *	reduce_intra
 *
 *	Function:	- reduction using O(N) algorithm
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
    long true_lb, true_extent, lb, extent;
    char *free_buffer = NULL;
    char *pml_buffer = NULL;
    int i, rank, ret;

    rank   = ompi_comm_rank ( comm );
    data   = comm->c_coll_selected_data;
    llcomm = data->hier_llcomm;


    /* 
     * collect the data from the low-level communicators. Result will be stored
     * on the local leaders.
     */
    if ( MPI_COMM_NULL != llcomm ) {
	ret = llcomm->c_coll.coll_reduce(sbuf, rbuf, count, dtype, op,
					 data->hier_my_lleader, llcomm );
    }


    /* trivial linear reduction receiving the data from all local leaders.
       need something significantly better */
    if ( rank == root ) {
	/* Root receives and reduces messages  */
	ompi_ddt_get_extent(dtype, &lb, &extent);
	ompi_ddt_get_true_extent(dtype, &true_lb, &true_extent);
	
	free_buffer = malloc(true_extent + (count - 1) * extent);
	if (NULL == free_buffer) {
	    return OMPI_ERR_OUT_OF_RESOURCE;
	}
	pml_buffer = free_buffer - lb;
	
	if ( !data->hier_am_lleader ) {
	    /* Initialize the receive buffer. */
	    ret = mca_pml.pml_recv(rbuf, count, dtype, data->hier_lleader[0],
				   MCA_COLL_BASE_TAG_REDUCE, comm, 
				   MPI_STATUS_IGNORE);
	    if (MPI_SUCCESS != ret) {
		goto exit;
	    }
	}

	/* Loop receiving and calling reduction function (C or Fortran). */
	for (i = 1; i < data->hier_num_lleaders; i++) {
	    if ( data->hier_lleader[i] == rank ) {
		continue;
	    }
	    ret = mca_pml.pml_recv(pml_buffer, count, dtype, data->hier_lleader[i], 
				   MCA_COLL_BASE_TAG_REDUCE, comm, 
				   MPI_STATUS_IGNORE);
	    if (MPI_SUCCESS != ret) {
		goto exit;
	    }
	    
          /* Perform the reduction */
          ompi_op_reduce(op, pml_buffer, rbuf, count, dtype);
	}
    }
    else if ( data->hier_am_lleader ) {
	if ( MPI_COMM_NULL != llcomm ) {
	    ret = mca_pml.pml_send ( rbuf, count, dtype, root, 
				     MCA_COLL_BASE_TAG_REDUCE, 
				     MCA_PML_BASE_SEND_STANDARD, 
				     comm);
	}
	else {
	    ret = mca_pml.pml_send ( sbuf, count, dtype, root, 
				     MCA_COLL_BASE_TAG_REDUCE, 
				     MCA_PML_BASE_SEND_STANDARD, 
				     comm);
	}	    
    }
    
 exit:
    if ( NULL != free_buffer ) {
	free ( free_buffer);
    }


    return  ret;
}

#else

int mca_coll_hierarch_reduce_intra(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype, 
                               struct ompi_op_t *op,
                               int root, struct ompi_communicator_t *comm)
{
  ompi_output_verbose(10, mca_coll_base_output, "In hierarch reduce_intra");
  return comm->c_coll_basic_module->coll_reduce(sbuf, rbuf, count, dtype,
                                                op, root, comm);
}

#endif

