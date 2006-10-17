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
#include "coll_hierarch.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "opal/util/output.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"


/*
 *	barrier_intra
 *
 *	Function:	- barrier using hierarchical algorithm
 *	Accepts:	- same as MPI_Barrier()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_hierarch_barrier_intra(struct ompi_communicator_t *comm)
{
    struct mca_coll_base_comm_t *data=NULL;
    struct ompi_communicator_t *llcomm=NULL;
    struct ompi_communicator_t *lcomm=NULL;
    int root=0;
    int lroot, llroot;
    int rank, ret=OMPI_SUCCESS;
    
    rank   = ompi_comm_rank ( comm );
    data   = comm->c_coll_selected_data;
    lcomm  = data->hier_lcomm;

    if ( mca_coll_hierarch_verbose_param ) { 
      printf("%s:%d: executing hierarchical barrier\n", comm->c_name, rank );
    } 

    llcomm = mca_coll_hierarch_get_llcomm ( root, data, &llroot, &lroot);

    /* 
     * Barrier consists of three steps:
     * - barrier on the low-level communicators
     * - barrier among the local leaders
     * - barrier on the low-level communicators. This step is 
     *   necessary to avoid that any non local leaders exit too early.
     */
    if ( MPI_COMM_NULL != lcomm ) {
	ret = lcomm->c_coll.coll_barrier ( lcomm );
        if ( OMPI_SUCCESS != ret ) {
           return ret;
	}
    }

    if ( MPI_UNDEFINED != llroot ) {
	ret = llcomm->c_coll.coll_barrier ( llcomm );
    }

    if ( MPI_COMM_NULL != lcomm ) {
	ret = lcomm->c_coll.coll_barrier ( lcomm );
    }

    return  ret;
}


