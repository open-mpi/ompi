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
#include "coll_hierarch.h"

#include "mpi.h"
#include "include/constants.h"
#include "util/output.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "mca/coll/base/coll_tags.h"
#include "coll_hierarch.h"


/*
 *	bcast_intra
 *
 *	Function:	- broadcast using O(N) algorithm
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_hierarch_bcast_intra(void *buff, int count,
				  struct ompi_datatype_t *datatype, int root,
				  struct ompi_communicator_t *comm)
{
    struct mca_coll_base_comm_t *data=NULL;
    struct ompi_communicator_t *llcomm=NULL;
    int i, rank, ret;

    rank   = ompi_comm_rank ( comm );
    data   = comm->c_coll_selected_data;
    llcomm = data->hier_llcomm;

    /* trivial linear distribution of the data to all local leaders.
       need something significantly better */
    if ( rank == root ) {
	for (i=0; i< data->hier_num_lleaders; i++) {
	    if ( data->hier_lleaders[i] == data->hier_my_lleader ) {
		data->hier_reqs[i] = MPI_REQUEST_NULL;
		continue;
	    }
	    ret = mca_pml.pml_isend (buff, count, datatype, data->hier_lleaders[i],
				     MCA_COLL_BASE_TAG_BCAST, 
				     MCA_PML_BASE_SEND_STANDARD, 
				     comm, &(data->hier_reqs[i]));
	    if ( OMPI_SUCCESS != ret ) {
		return ret;
	    }
	}
	ret = ompi_request_wait_all ( data->hier_num_lleaders, data->hier_reqs, 
				      MPI_STATUSES_IGNORE);
	if ( OMPI_SUCCESS != ret ) {
	    return ret;
	}
    }
    else if ( data->hier_am_lleader ) {
	ret = mca_pml.pml_recv ( buff, count, datatype, root, 
				 MCA_COLL_BASE_TAG_BCAST, comm, 
				 MPI_STATUS_IGNORE );
	if ( OMPI_SUCCESS != ret ) {
	    return ret;
	}
    }

    if ( MPI_COMM_NULL != llcomm ) {
	ret = llcomm->c_coll.coll_bcast(buff, count, datatype, 
					data->hier_my_lleader, llcomm );
    }

    return  ret;
 }
