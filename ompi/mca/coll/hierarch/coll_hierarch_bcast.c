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

#include "mpi.h"
#include "ompi/include/constants.h"
#include "opal/util/output.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"

/*
 *	bcast_intra
 *
 *	Function:	- broadcast using hierarchical algorithm
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */

extern int mca_coll_hier_verbose;


int mca_coll_hierarch_bcast_intra(void *buff, 
				  int count,
				  struct ompi_datatype_t *datatype, 
				  int root,
				  struct ompi_communicator_t *comm)
{
    struct mca_coll_base_comm_t *data=NULL;
    struct ompi_communicator_t *llcomm=NULL;
    struct ompi_communicator_t *lcomm=NULL;
    int lroot, llroot;
    int rank, ret;
    

    rank   = ompi_comm_rank ( comm );
    data   = comm->c_coll_selected_data;
    lcomm  = data->hier_lcomm;

    if ( mca_coll_hier_verbose ) {
      printf("%s:%d: executing hierarchical bcast with cnt=%d and root=%d\n",
	     comm->c_name, rank, count, root );
    }

    /* This function returns the local leader communicator
       which *always* contains the root of this operation.
       This might involve creating a new communicator. This is 
       also the reason, that *every* process in comm has to call 
       this function
    */
    llcomm = mca_coll_hierarch_get_llcomm ( root, data, &llroot, &lroot);

    /* Bcast on the upper level among the local leaders */
    if ( MPI_UNDEFINED != llroot ) {
	ret = llcomm->c_coll.coll_bcast(buff, count, datatype, llroot, llcomm);
        if ( OMPI_SUCCESS != ret ) {
           return ret;
	}
    }

    /* once the local leaders got the data from the root, they can distribute
       it to the processes in their local, low-leve communicator.
    */

    if ( MPI_COMM_NULL != lcomm ) {
	ret = lcomm->c_coll.coll_bcast(buff, count, datatype, lroot, lcomm );
    }

    return  ret;
}
