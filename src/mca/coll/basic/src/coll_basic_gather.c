/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic.h"

#include "lam/constants.h"
#include "mpi.h"
#include "mca/mpi/coll/coll.h"
#include "mca/mpi/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	gather
 *
 *	Function:	- basic gather operation
 *	Accepts:	- same arguments as MPI_Gather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_gather(void *sbuf, int scount, MPI_Datatype sdtype, 
                          void *rbuf, int rcount, MPI_Datatype rdtype, 
                          int root, MPI_Comm comm)
{
#if 1
  return LAM_ERR_NOT_IMPLEMENTED;
#else
  int i;
  int err;
  int rank;
  int size;
  char *ptmp;
  MPI_Aint incr;
  MPI_Aint extent;

  /* JMS: Need to replace lots things in this file: lam_dt* stuff with
     lam_datatype_*() functions.  Also need to replace lots of
     MPI_Send/MPI_Recv with negative tags and PML entry points. */

  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  /* Everyone but root sends data and returns. */

  if (rank != root) {
    err = MPI_Send(sbuf, scount, sdtype, root, BLKMPIGATHER, comm);
    return err;
  }

  /* I am the root, loop receiving the data. */

  MPI_Type_extent(rdtype, &extent);
  incr = extent * rcount;
  for (i = 0, ptmp = (char *) rbuf; i < size; ++i, ptmp += incr) {

    /* simple optimization */

    if (i == rank) {
      err = lam_dtsndrcv(sbuf, scount, sdtype, ptmp,
			 rcount, rdtype, BLKMPIGATHER, comm);
    } else {
      err = MPI_Recv(ptmp, rcount, rdtype, i,
		     BLKMPIGATHER, comm, MPI_STATUS_IGNORE);
    }
    if (MPI_SUCCESS != err) {
      return err;
    }
  }

  /* All done */
  
  return MPI_SUCCESS;
#endif
}
