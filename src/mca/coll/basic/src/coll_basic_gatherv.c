/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic.h"

#include "constants.h"
#include "mpi.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	gatherv
 *
 *	Function:	- basic gatherv operation
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_gatherv(void *sbuf, int scount, MPI_Datatype sdtype,
                           void *rbuf, int *rcounts, int *disps,
                           MPI_Datatype rdtype, int root,
                           MPI_Comm comm)
{
#if 1
  return LAM_ERR_NOT_IMPLEMENTED;
#else
  int i;
  int rank;
  int size;
  int err;
  char *ptmp;
  MPI_Aint extent;

  /* JMS: Need to replace lots things in this file: lam_dt* stuff with
     lam_datatype_*() functions.  Also need to replace lots of
     MPI_Send/MPI_Recv with negative tags and PML entry points. */

  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);

  /* Everyone but root sends data and returns. */

  if (rank != root) {
    err = MPI_Send(sbuf, scount, sdtype, root, BLKMPIGATHERV, comm);
    return err;
  }

  /* I am the root, loop receiving data. */

  MPI_Type_extent(rdtype, &extent);
  for (i = 0; i < size; ++i) {
    ptmp = ((char *) rbuf) + (extent * disps[i]);

    /* simple optimization */

    if (i == rank) {
      err = lam_dtsndrcv(sbuf, scount, sdtype,
			 ptmp, rcounts[i], rdtype, BLKMPIGATHERV, comm);
    } else {
      err = MPI_Recv(ptmp, rcounts[i], rdtype, i,
		     BLKMPIGATHERV, comm, MPI_STATUS_IGNORE);
    }

    if (MPI_SUCCESS != err) {
      return err;
    }
  }

  /* All done */

  return MPI_SUCCESS;
#endif
}
