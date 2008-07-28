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
 * Copyright (c) 2007      University of Houston. All rights reserved.
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
#include "orte/util/show_help.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"

/*
 *	bcast_intra
 *
 *	Function:	- broadcast using hierarchical algorithm
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */


int mca_coll_hierarch_bcast_intra(void *buff, 
				  int count,
				  struct ompi_datatype_t *datatype, 
				  int root,
				  struct ompi_communicator_t *comm, 
				  mca_coll_base_module_t *module)
{
    struct ompi_communicator_t *llcomm=NULL;
    struct ompi_communicator_t *lcomm=NULL;
    mca_coll_hierarch_module_t *hierarch_module = (mca_coll_hierarch_module_t *) module;
    int lroot, llroot;
    int rank, ret=OMPI_SUCCESS;
    

    rank   = ompi_comm_rank ( comm );
    lcomm  = hierarch_module->hier_lcomm;

    if ( mca_coll_hierarch_verbose_param ) { 
      printf("%s:%d: executing hierarchical bcast with cnt=%d and root=%d\n",
	     comm->c_name, rank, count, root );
    } 

    /* 
     * This function returns the local leader communicator
     * which *always* contains the root of this operation.
     * This might involve creating a new communicator. This is 
     * also the reason, that *every* process in comm has to call 
     * this function
     */
    llcomm = mca_coll_hierarch_get_llcomm ( root, hierarch_module, &llroot, &lroot);

    /* Bcast on the upper level among the local leaders */
    if ( MPI_UNDEFINED != llroot ) {
	ret = llcomm->c_coll.coll_bcast(buff, count, datatype, llroot, llcomm, 
					llcomm->c_coll.coll_bcast_module);
        if ( OMPI_SUCCESS != ret ) {
           return ret;
	}
    }

    /* once the local leaders got the data from the root, they can distribute
     * it to the processes in their local, low-leve communicator.
     */
    if ( MPI_COMM_NULL != lcomm ) {
	ret = lcomm->c_coll.coll_bcast(buff, count, datatype, lroot, lcomm, 
				       lcomm->c_coll.coll_bcast_module );
    }

    return  ret;
}
