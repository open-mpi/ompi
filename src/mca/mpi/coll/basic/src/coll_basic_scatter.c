/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic_config.h"

#include "lam/constants.h"
#include "mpi.h"
#include "mca/mpi/coll/coll.h"
#include "mca/mpi/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	scatter
 *
 *	Function:	- scatter operation
 *	Accepts:	- same arguments as MPI_Scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_scatter(void *sbuf, int scount, MPI_Datatype sdtype,
                           void *rbuf, int rcount, MPI_Datatype rdtype,
                           int root, MPI_Comm comm)
{
#if 1
  return LAM_ERR_NOT_IMPLEMENTED;
#else
  int i;
  int rank;
  int size;
  int err;
  char *ptmp;
  MPI_Aint incr;

  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  /* If not root, receive data. */

  if (rank != root) {
#if 0
    /* JMS Need to replace with negative tags and direct PML calls */
    err = MPI_Recv(rbuf, rcount, rdtype,
		   root, BLKMPISCATTER, comm, MPI_STATUS_IGNORE);
#endif
    return err;
  }

  /* I am the root, loop sending data. */

  MPI_Type_extent(sdtype, &incr);
  incr *= scount;
  for (i = 0, ptmp = (char *) sbuf; i < size; ++i, ptmp += incr) {

    /* simple optimization */

    if (i == rank) {
#if 0
      /* JMS Need to replace this with lam_datatype_*() functions */
      err = lam_dtsndrcv(ptmp, scount, sdtype, rbuf,
			 rcount, rdtype, BLKMPISCATTER, comm);
#endif
    } else {
#if 0
    /* JMS Need to replace this with negative tags and direct PML calls */
      err = MPI_Send(ptmp, scount, sdtype, i, BLKMPISCATTER, comm);
#endif
    }
    if (MPI_SUCCESS != err) {
      return err;
    }
  }

  /* All done */

  return MPI_SUCCESS;
#endif
}
