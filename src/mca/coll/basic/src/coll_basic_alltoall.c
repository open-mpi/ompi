/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include <stdio.h>
#include <errno.h>

#include "constants.h"
#include "mpi.h"
#include "datatype/datatype.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "coll_basic.h"
#include "mca/pml/pml.h"


/*
 *	alltoall
 *
 *	Function:	- MPI_Alltoall 
 *	Accepts:	- same as MPI_Alltoall()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int mca_coll_basic_alltoall(void *sbuf, int scount,
                            MPI_Datatype sdtype, void *rbuf,
                            int rcount, MPI_Datatype rdtype,
                            MPI_Comm comm)
{
    int i;
    int rank;
    int size;
    int nreqs;
    int err;
    char *psnd;
    char *prcv;
    MPI_Aint lb;
    MPI_Aint sndinc;
    MPI_Aint rcvinc;

    ompi_request_t **req;
    ompi_request_t **sreq;
    ompi_request_t **rreq;

    /* Initialize. */

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);


    err = ompi_ddt_get_extent(sdtype, &lb, &sndinc);
    if (0 != err) {
	return OMPI_ERROR;
    }

    err = ompi_ddt_get_extent(rdtype, &lb, &rcvinc);
    if (0 != err) {
	return OMPI_ERROR;
    }

    sndinc *= scount;
    rcvinc *= rcount;

    /* Allocate arrays of requests. */

    nreqs = 2 * (size - 1);
    if (nreqs > 0) {
	req = malloc(nreqs * sizeof(ompi_request_t *));
	if (NULL == req) {
	    return ENOMEM;
	}
    } else {
	req = NULL;
    }
    
    /* simple optimization */

    psnd = ((char *) sbuf) + (rank * sndinc);
    prcv = ((char *) rbuf) + (rank * rcvinc);

    err = ompi_ddt_sndrcv(psnd, scount, sdtype,
			 prcv, rcount, rdtype, 
			 MCA_COLL_BASE_TAG_ALLTOALL, comm);

    if (MPI_SUCCESS != err) {
	if (NULL != req)
	    free(req);
	return err;
    }

    /* If only one process, we're done. */

    if (1 == size) {
	return MPI_SUCCESS;
    }

    /* Initiate all send/recv to/from others. */

    rreq = req;
    sreq = req + size - 1;

    prcv = (char*) rbuf;
    psnd = (char*) sbuf;

    for (i = (rank + 1) % size; i != rank; 
	 i = (i + 1) % size, ++rreq, ++sreq) {
	
	err = mca_pml.pml_irecv_init(prcv + (i * rcvinc), rcount, rdtype, i,
				     MCA_COLL_BASE_TAG_ALLTOALL, comm, rreq);
	if (MPI_SUCCESS != err) {
	    free(req);
	    return err;
	}

	err = mca_pml.pml_isend(psnd + (i * sndinc), scount, sdtype, i,
				MCA_COLL_BASE_TAG_ALLTOALL, 
				MCA_PML_BASE_SEND_STANDARD, comm, sreq);
	if (MPI_SUCCESS != err) {
	    free(req);
	    return err;
	}
    }

    if (MPI_SUCCESS != err) {
	free(req);
	return err;
    }

    /* Wait for them all. */

    err = mca_pml.pml_wait_all(nreqs, req, MPI_STATUSES_IGNORE);
    if (MPI_SUCCESS != err) {
	free(req);
	return err;
    }

    /* Free the reqs */

    for (i = 0, rreq = req; i < nreqs; ++i, ++rreq) {
	mca_pml.pml_free(rreq);
    }

    /* All done */

    free(req);
    return MPI_SUCCESS;
}
