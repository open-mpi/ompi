/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic_config.h"

#include <stdio.h>
#include <errno.h>

#include "lam/constants.h"
#include "mpi.h"
#include "lam/mem/malloc.h"
#include "mpi/datatype/datatype.h"
#include "mca/mpi/coll/coll.h"
#include "mca/mpi/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	alltoallv
 *
 *	Function:	- MPI_Alltoallv for non-lamd RPIs
 *	Accepts:	- same as MPI_Alltoallv()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int
mca_coll_basic_alltoallv(void *sbuf, int *scounts, int *sdisps,
                         MPI_Datatype sdtype, void *rbuf,
                         int *rcounts, int *rdisps,
                         MPI_Datatype rdtype, MPI_Comm comm)
{
#if 1
  return LAM_ERR_NOT_IMPLEMENTED;
#else
  int i;
  int size;
  int rank;
  int nreqs;
  int err;
  char *psnd;
  char *prcv;
  MPI_Aint sndextent;
  MPI_Aint rcvextent;
  MPI_Request *req;
  MPI_Request *preq;

  /* Initialize. */

  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);
  MPI_Type_extent(sdtype, &sndextent);
  MPI_Type_extent(rdtype, &rcvextent);

  /* Allocate arrays of requests. */

  nreqs = 2 * (size - 1);
  if (nreqs > 0) {
    req = malloc(nreqs * sizeof(MPI_Request));
    if (NULL == req) {
      free(req);
      return ENOMEM;
    }
  } else {
    req = NULL;
  }

  /* simple optimization */

  psnd = ((char *) sbuf) + (sdisps[rank] * sndextent);
  prcv = ((char *) rbuf) + (rdisps[rank] * rcvextent);
#if 0
  /* JMS: Need a lam_datatype_something() here that allows two
     different datatypes */
  err = lam_dtsndrcv(psnd, scounts[rank], sdtype,
		     prcv, rcounts[rank], rdtype, BLKMPIALLTOALLV, comm);
  if (MPI_SUCCESS != err) {
    if (NULL != req)
      LAM_FREE(req);
    return err;
  }
#endif

  /* If only one process, we're done. */

  if (1 == size) {
    return MPI_SUCCESS;
  }

  /* Initiate all send/recv to/from others. */

  preq = req;
  for (i = 0; i < size; ++i) {
    if (i == rank)
      continue;

    prcv = ((char *) rbuf) + (rdisps[i] * rcvextent);
#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
    err = MPI_Recv_init(prcv, rcounts[i], rdtype,
			i, BLKMPIALLTOALLV, comm, preq++);
    if (MPI_SUCCESS != err) {
      LAM_FREE(req);
      return err;
    }
#endif
  }

  for (i = 0; i < size; ++i) {
    if (i == rank)
      continue;

    psnd = ((char *) sbuf) + (sdisps[i] * sndextent);
#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
    err = MPI_Send_init(psnd, scounts[i], sdtype,
			i, BLKMPIALLTOALLV, comm, preq++);
    if (MPI_SUCCESS != err) {
      LAM_FREE(req);
      return err;
    }
#endif
  }

  /* Start all requests. */

  err = MPI_Startall(nreqs, req);
  if (MPI_SUCCESS != err) {
    free(req);
    return err;
  }

  /* Wait for them all. */

  err = MPI_Waitall(nreqs, req, MPI_STATUSES_IGNORE);
  if (MPI_SUCCESS != err) {
    free(req);
    return err;
  }

  /* Free the requests. */

  for (i = 0, preq = req; i < nreqs; ++i, ++preq) {
    err = MPI_Request_free(preq);
    if (err != MPI_SUCCESS) {
      free(req);
      return err;
    }
  }

  /* All done */

  free(req);
  return MPI_SUCCESS;
#endif
}
