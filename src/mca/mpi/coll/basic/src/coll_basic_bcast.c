/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic_config.h"

#include "lam/constants.h"
#include "mpi.h"
#include "mca/mpi/coll/coll.h"
#include "coll_basic.h"


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
  MPI_Request *preq;
  /* JMS: Need to define this somewhere */
#define LAM_COLLMAXLIN 4
  MPI_Request reqs[LAM_COLLMAXLIN];

  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);

  /* Non-root receive the data. */

  if (rank != root) {
#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
    return  MPI_Recv(buff, count, datatype, root,
                     BLKMPIBCAST, comm, MPI_STATUS_IGNORE);
#endif
  }

  /* Root sends data to all others. */

  for (i = 0, preq = reqs; i < size; ++i) {
    if (i == rank)
      continue;

#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
    err = MPI_Send_init(buff, count, datatype, i, BLKMPIBCAST,
			comm, preq++);
    if (MPI_SUCCESS != err) {
      return err;
    }
#endif
  }

  /* Start and wait on all requests. */

  err = MPI_Startall(size - 1, reqs);
  if (MPI_SUCCESS != err) {
    return err;
  }

  err = MPI_Waitall(size - 1, reqs, MPI_STATUSES_IGNORE);
  if (MPI_SUCCESS != err) {
    return err;
  }

  /* Free the requests. */

  for (i = 0, preq = reqs; i < size; ++i) {
    if (i == rank)
      continue;

    err = MPI_Request_free(preq);
    if (MPI_SUCCESS != err)
      return err;

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
  MPI_Request *preq;
  /* JMS: Need to define this somewhere */
#define LAM_COLLMAXDIM 64
  MPI_Request reqs[LAM_COLLMAXDIM];

  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  vrank = (rank + size - root) % size;
#if 0
  /* JMS Need to cache this somewhere */
  dim = comm->c_cube_dim;
  hibit = lam_hibit(vrank, dim);
#endif
  --dim;

  /* Receive data from parent in the tree. */

  if (vrank > 0) {
    peer = ((vrank & ~(1 << hibit)) + root) % size;
#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
    err = MPI_Recv(buff, count, datatype, peer,
		   BLKMPIBCAST, comm, MPI_STATUS_IGNORE);
    if (MPI_SUCCESS != err) {
      return err;
    }
#endif
  }

  /* Send data to the children. */

  preq = reqs;
  nreqs = 0;
  for (i = hibit + 1, mask = 1 << i; i <= dim; ++i, mask <<= 1) {
    peer = vrank | mask;
    if (peer < size) {
      peer = (peer + root) % size;
      ++nreqs;

#if 0
      /* JMS: Need to replace this with negative tags and and direct PML
         calls */
      err = MPI_Send_init(buff, count, datatype, peer, BLKMPIBCAST,
			  comm, preq++);
      if (MPI_SUCCESS != err) {
	return err;
      }
#endif
    }
  }

  /* Start and wait on all requests. */

  if (nreqs > 0) {
    err = MPI_Startall(nreqs, reqs);
    if (MPI_SUCCESS != err) {
      return err;
    }

    err = MPI_Waitall(nreqs, reqs, MPI_STATUSES_IGNORE);
    if (MPI_SUCCESS != err) {
      return err;
    }

    for (i = 0, preq = reqs; i < nreqs; ++i, ++preq) {
      err = MPI_Request_free(preq);
      if (MPI_SUCCESS != err) {
	return err;
      }
    }
  }

  /* All done */

  return MPI_SUCCESS;
}
