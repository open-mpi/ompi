/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include "constants.h"
#include "mpi.h"
#include "datatype/datatype.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "coll_basic.h"
#include "mca/pml/pml.h"
#include "util/hibit.h"


/*
 *	bcast_lin
 *
 *	Function:	- broadcast using O(N) algorithm
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_bcast_lin(void *buff, int count,
			     MPI_Datatype datatype, int root,
			     MPI_Comm comm)
{
    int i;
    int size;
    int rank;
    int err;
    ompi_request_t **preq;
    ompi_request_t **reqs = comm->bcast_lin_reqs;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
  
    /* Non-root receive the data. */

    if (rank != root) {
	return mca_pml.pml_recv(buff, count, datatype, root,
				MCA_COLL_BASE_TAG_BCAST, comm, 
				MPI_STATUS_IGNORE);
    }

    /* Root sends data to all others. */

    /* VPS: as per Tim's suggestion there is no advantage of having
       isend_init/start over normal isend. So just trying a normal isend  */

    for (i = 0, preq = reqs; i < size; ++i) {
	if (i == rank)
	    continue;

	err = mca_pml.pml_isend(buff, count, datatype, i, 
				MCA_COLL_BASE_TAG_BCAST,
				MCA_PML_BASE_SEND_STANDARD, 
				comm, preq++);

	if (MPI_SUCCESS != err) {
	    return err;
	}
    }
    
    /* Free the requests. */
    
    for (i = 0, preq = reqs; i < size; ++i) {
	if (i == rank)
	    continue;

	err = mca_pml.pml_free(preq);
	++preq;
    }

    /* All done */

    return MPI_SUCCESS;
}



/*
 *	bcast_log
 *
 *	Function:	- broadcast using O(log(N)) algorithm
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_bcast_log(void *buff, int count,
			     MPI_Datatype datatype, int root,
			     MPI_Comm comm)
{
    int i;
    int size;
    int rank;
    int vrank;
    int peer;
    int dim;
    int hibit;
    int mask;
    int err;
    int nreqs;
    ompi_request_t **preq;
    ompi_request_t **reqs = comm->bcast_log_reqs;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    vrank = (rank + size - root) % size;

    dim = comm->c_cube_dim;
    hibit = ompi_hibit(vrank, dim);
    --dim;

    /* Receive data from parent in the tree. */

    if (vrank > 0) {
	peer = ((vrank & ~(1 << hibit)) + root) % size;

	err = mca_pml.pml_recv(buff, count, datatype, peer,
			       MCA_COLL_BASE_TAG_BCAST,
			       comm, MPI_STATUS_IGNORE);
	if (MPI_SUCCESS != err) {
	    return err;
	}
    }

    /* Send data to the children. */

    preq = reqs;
    nreqs = 0;
    for (i = hibit + 1, mask = 1 << i; i <= dim; ++i, mask <<= 1) {
	peer = vrank | mask;
	if (peer < size) {
	    peer = (peer + root) % size;
	    ++nreqs;

	    err = mca_pml.pml_isend(buff, count, datatype, peer,
				    MCA_COLL_BASE_TAG_BCAST, 
				    MCA_PML_BASE_SEND_STANDARD, 
				    comm, preq++);

	    if (MPI_SUCCESS != err) {
		return err;
	    }
	}
    }

    /* Start and wait on all requests. */

    if (nreqs > 0) {

	err = mca_pml.pml_wait_all(nreqs, reqs, MPI_STATUSES_IGNORE);
	if (MPI_SUCCESS != err) {
	    return err;
	}

	for (i = 0, preq = reqs; i < nreqs; ++i, ++preq) {
	    mca_pml.pml_free(preq);
	}
    }

    /* All done */

    return MPI_SUCCESS;
}
