/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic_config.h"

#include <stdio.h>
#include <errno.h>

#include "lam/constants.h"
#include "mpi.h"
#include "mca/mpi/coll/coll.h"
#include "mca/mpi/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	reduce_scatter
 *
 *	Function:	- reduce then scatter
 *	Accepts:	- same as MPI_Reduce_scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_reduce_scatter(void *sbuf, void *rbuf, int *rcounts,
                                  MPI_Datatype dtype, MPI_Op op,
                                  MPI_Comm comm)
{
#if 1
  return LAM_ERR_NOT_IMPLEMENTED;
#else
  int i;
  int err;
  int rank;
  int size;
  int count;
  int *disps = NULL;
  char *buffer = NULL;
  char *origin = NULL;

  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);

  /* Initialize reduce & scatterv info at the root (rank 0). */

  for (i = 0, count = 0; i < size; ++i) {
    if (rcounts[i] < 0) {
      return EINVAL;
    }
    count += rcounts[i];
  }

  if (0 == rank) {
    disps = malloc((unsigned) size * sizeof(int));
    if (NULL == disps) {
      free(disps);
      return errno;
    }

#if 0
    /* JMS Need to replace this with lam_datatype_*() functions */
    err = lam_dtbuffer(dtype, count, &buffer, &origin);
    if (MPI_SUCCESS != err) {
      free(disps);
      return err;
    }
#endif

    disps[0] = 0;
    for (i = 0; i < (size - 1); ++i)
      disps[i + 1] = disps[i] + rcounts[i];
  }

  /* reduction */

  err = MPI_Reduce(sbuf, origin, count, dtype, op, 0, comm);
  if (MPI_SUCCESS != err) {
    if (NULL != disps)
      free(disps);
    if (NULL != buffer)
      free(buffer);
    return err;
  }

  /* scatter */

  err = MPI_Scatterv(origin, rcounts, disps, dtype,
		     rbuf, rcounts[rank], dtype, 0, comm);
  if (NULL != disps)
    free(disps);
  if (NULL != buffer)
    free(buffer);

  return err;
#endif
}
