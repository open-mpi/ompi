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
 *	barrier_lin
 *
 *	Function:	- barrier using O(N) algorithm
 *	Accepts:	- same as MPI_Barrier()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_barrier_lin(MPI_Comm comm)
{
  int size;
  int rank;
  int err;
  int i;

  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);

  /* All non-root send & receive zero-length message. */

  if (rank > 0) {
#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
    err = MPI_Send((void *) 0, 0, MPI_BYTE, 0, BLKMPIBARRIER, comm);
    if (MPI_SUCCESS != err) {
      return err;
    }
#endif

#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
    err = MPI_Recv((void *) 0, 0, MPI_BYTE, 0, BLKMPIBARRIER, comm, 
		   MPI_STATUS_IGNORE);
    if (MPI_SUCCESS != err) {
      return err;
    }
#endif
  }

  /* The root collects and broadcasts the messages. */

  else {
    for (i = 1; i < size; ++i) {
#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
      err = MPI_Recv((void *) 0, 0, MPI_BYTE, MPI_ANY_SOURCE,
		     BLKMPIBARRIER, comm, MPI_STATUS_IGNORE);
      if (MPI_SUCCESS != err) {
        return err;
      }
#endif
    }

    for (i = 1; i < size; ++i) {
#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
      err = MPI_Send((void *) 0, 0, MPI_BYTE, i, BLKMPIBARRIER, comm);
      if (MPI_SUCCESS != err) {
        return err;
      }
#endif
    }
  }

  /* All done */

  return MPI_SUCCESS;
}


/*
 *	barrier_log
 *
 *	Function:	- barrier using O(log(N)) algorithm
 *	Accepts:	- same as MPI_Barrier()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_barrier_log(MPI_Comm comm)
{
  int size;
  int rank;
  int peer;
  int dim;
  int hibit;
  int mask;
  int err;
  int i;

  /* Send null-messages up and down the tree.  Synchronization at the
     root (rank 0). */

  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
#if 0
  /* JMS Need to cache this info somewhere */
  dim = comm->c_cube_dim;
  hibit = lam_hibit(rank, dim);
#endif
  --dim;

  /* Receive from children. */

  for (i = dim, mask = 1 << i; i > hibit; --i, mask >>= 1) {
    peer = rank | mask;
    if (peer < size) {
#if 0
      /* JMS: Need to replace this with negative tags and and direct PML
         calls */
      err = MPI_Recv((void *) 0, 0, MPI_BYTE,
		     peer, BLKMPIBARRIER, comm, MPI_STATUS_IGNORE);
      if (MPI_SUCCESS != err) {
	return err;
      }
#endif
    }
  }

  /* Send to and receive from parent. */

  if (rank > 0) {
    peer = rank & ~(1 << hibit);
#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
    err = MPI_Send((void *) 0, 0, MPI_BYTE, peer, BLKMPIBARRIER, comm);
    if (MPI_SUCCESS != err) {
      return err;
    }
#endif

#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
    err = MPI_Recv((void *) 0, 0, MPI_BYTE, peer,
		   BLKMPIBARRIER, comm, MPI_STATUS_IGNORE);
    if (MPI_SUCCESS != err) {
      return err;
    }
#endif
  }

  /* Send to children. */

  for (i = hibit + 1, mask = 1 << i; i <= dim; ++i, mask <<= 1) {
    peer = rank | mask;
    if (peer < size) {
#if 0
      /* JMS: Need to replace this with negative tags and and direct PML
         calls */
      err = MPI_Send((void *) 0, 0, MPI_BYTE, peer, BLKMPIBARRIER, comm);
      if (MPI_SUCCESS != err) {
	return err;
      }
#endif
    }
  }

  /* All done */

  return MPI_SUCCESS;
}
