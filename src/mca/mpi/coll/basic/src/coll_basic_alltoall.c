/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic_config.h"

#include <stdio.h>
#include <errno.h>

#include "lam/constants.h"
#include "mpi.h"
#include "lam/util/malloc.h"
#include "mpi/datatype/datatype.h"
#include "mca/mpi/coll/coll.h"
#include "coll_basic.h"


/*
 *	alltoall
 *
 *	Function:	- MPI_Alltoall for non-lamd RPI's
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
  MPI_Aint sndinc;
  MPI_Aint rcvinc;
  MPI_Request *req;
  MPI_Request *preq;
  MPI_Request *qreq;

  /* Initialize. */

  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);
  MPI_Type_extent(sdtype, &sndinc);
  MPI_Type_extent(rdtype, &rcvinc);
  sndinc *= scount;
  rcvinc *= rcount;

  /* Allocate arrays of requests. */

  nreqs = 2 * (size - 1);
  if (nreqs > 0) {
    req = (MPI_Request *) LAM_MALLOC(nreqs * sizeof(MPI_Request));
    if (NULL == req) {
      LAM_FREE(req);
      return ENOMEM;
    }
  } else {
    req = NULL;
  }

  /* simple optimization */

  psnd = ((char *) sbuf) + (rank * sndinc);
  prcv = ((char *) rbuf) + (rank * rcvinc);
#if 0
  /* JMS: Need a lam_datatype_something() here that allows two
     different datatypes */
  err = lam_dtsndrcv(psnd, scount, sdtype,
		     prcv, rcount, rdtype, BLKMPIALLTOALL, comm);
  if (MPI_SUCCESS != err) {
    if (NULL != req)
      LAM_FREE(req);
    lam_mkpt(comm);
    return err;
  }
#endif

  /* If only one process, we're done. */

  if (1 == size) {
    return MPI_SUCCESS;
  }

  /* Initiate all send/recv to/from others. */

  preq = req;
  qreq = req + size - 1;
  prcv = (char*) rbuf;
  psnd = (char*) sbuf;
  for (i = (rank + 1) % size; i != rank; 
       i = (i + 1) % size, ++preq, ++qreq) {
#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
    err = MPI_Recv_init(prcv + (i * rcvinc), rcount, rdtype, i,
			BLKMPIALLTOALL, comm, preq);
    if (MPI_SUCCESS != err) {
      LAM_FREE(req);
      return err;
    }
#endif

#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
    err = MPI_Send_init(psnd + (i * sndinc), scount, sdtype, i,
			BLKMPIALLTOALL, comm, qreq);
    if (MPI_SUCCESS != err) {
      LAM_FREE(req);
      return err;
    }
#endif
  }

  /* Start all the requests. */

  err = MPI_Startall(nreqs, req);
  if (MPI_SUCCESS != err) {
    LAM_FREE(req);
    return err;
  }

  /* Wait for them all. */

  err = MPI_Waitall(nreqs, req, MPI_STATUSES_IGNORE);
  if (MPI_SUCCESS != err) {
    LAM_FREE(req);
    return err;
  }

  for (i = 0, preq = req; i < nreqs; ++i, ++preq) {
    err = MPI_Request_free(preq);
    if (MPI_SUCCESS != err) {
      LAM_FREE(req);
      return err;
    }
  }

  /* All done */

  LAM_FREE(req);
  return MPI_SUCCESS;
}
