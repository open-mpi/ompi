/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include "mpi.h"
#include "include/constants.h"
#include "coll_basic.h"
#include "datatype/datatype.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "mca/pml/pml.h"

/*
 *	gather_intra
 *
 *	Function:	- basic gather operation
 *	Accepts:	- same arguments as MPI_Gather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_gather_intra(void *sbuf, int scount, 
                                struct ompi_datatype_t *sdtype, 
                                void *rbuf, int rcount, 
                                struct ompi_datatype_t *rdtype, 
                                int root, struct ompi_communicator_t *comm)
{
    int i;
    int err;
    int rank;
    int size;
    char *ptmp;
    MPI_Aint incr;
    MPI_Aint extent;
    MPI_Aint lb;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* Everyone but root sends data and returns. */

    if (rank != root) {
        return mca_pml.pml_send(sbuf, scount, sdtype, root,
                                MCA_COLL_BASE_TAG_GATHER, 
                                MCA_PML_BASE_SEND_STANDARD, comm);
    }

    /* I am the root, loop receiving the data. */

    if (OMPI_SUCCESS != (err = ompi_ddt_get_extent(rdtype, &lb, &extent))) {
	return err;
    }

    incr = extent * rcount;
    for (i = 0, ptmp = (char *) rbuf; i < size; ++i, ptmp += incr) {

	/* simple optimization */

	if (i == rank) {
	    err = ompi_ddt_sndrcv(sbuf, scount, sdtype, ptmp,
				 rcount, rdtype, 
				 MCA_COLL_BASE_TAG_GATHER, comm);
	} else {
	    err = mca_pml.pml_recv(ptmp, rcount, rdtype, i,
				   MCA_COLL_BASE_TAG_GATHER, 
				   comm, MPI_STATUS_IGNORE);
	}
	if (MPI_SUCCESS != err) {
	    return err;
	}
    }

    /* All done */
  
    return MPI_SUCCESS;
}


/*
 *	gather_inter
 *
 *	Function:	- basic gather operation
 *	Accepts:	- same arguments as MPI_Gather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_gather_inter(void *sbuf, int scount,
                                struct ompi_datatype_t *sdtype, 
                                void *rbuf, int rcount, 
                                struct ompi_datatype_t *rdtype, 
                                int root, struct ompi_communicator_t *comm)
{
    int i;
    int err;
    int rank;
    int size;
    char *ptmp;
    MPI_Aint incr;
    MPI_Aint extent;
    MPI_Aint lb;

    size = ompi_comm_remote_size(comm);
    rank = ompi_comm_rank(comm);


    if ( MPI_PROC_NULL == root ) {
        /* do nothing */
        err = OMPI_SUCCESS;
    }
    else if ( MPI_ROOT != root ) {
        /* Everyone but root sends data and returns. */
	err = mca_pml.pml_send(sbuf, scount, sdtype, root,
			       MCA_COLL_BASE_TAG_GATHER, 
			       MCA_PML_BASE_SEND_STANDARD, comm);
    }
    else {
        /* I am the root, loop receiving the data. */
        err = ompi_ddt_get_extent(rdtype, &lb, &extent);
        if (OMPI_SUCCESS != err) {
            return OMPI_ERROR;
        }

        incr = extent * rcount;
        for (i = 0, ptmp = (char *) rbuf; i < size; ++i, ptmp += incr) {
	    err = mca_pml.pml_recv(ptmp, rcount, rdtype, i,
				   MCA_COLL_BASE_TAG_GATHER, 
				   comm, MPI_STATUS_IGNORE);
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
    }
    
    /* All done */
    return err;
}
