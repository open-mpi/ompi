/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic.h"

#include "constants.h"
#include "communicator/communicator.h"
#include "util/hibit.h"
#include "mpi.h"
#include "mca/pml/pml.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
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
  int i;
  int err;
  int size = lam_comm_size(comm);
  int rank = lam_comm_rank(comm);

  /* All non-root send & receive zero-length message. */

  if (rank > 0) {
    err = mca_pml.pml_send(NULL, 0, MPI_BYTE, 0, MCA_COLL_BASE_TAG_BARRIER, 
                           MCA_PML_BASE_SEND_STANDARD, comm);
    if (MPI_SUCCESS != err) {
      return err;
    }

    err = mca_pml.pml_recv(NULL, 0, MPI_BYTE, 0, MCA_COLL_BASE_TAG_BARRIER, 
                           comm, MPI_STATUS_IGNORE);
    if (MPI_SUCCESS != err) {
      return err;
    }
  }

  /* The root collects and broadcasts the messages. */

  else {
    for (i = 1; i < size; ++i) {
      err = mca_pml.pml_recv(NULL, 0, MPI_BYTE, MPI_ANY_SOURCE,
                             MCA_COLL_BASE_TAG_BARRIER, 
                             comm, MPI_STATUS_IGNORE);
      if (MPI_SUCCESS != err) {
        return err;
      }
    }

    for (i = 1; i < size; ++i) {
      err = mca_pml.pml_send(NULL, 0, MPI_BYTE, i, MCA_COLL_BASE_TAG_BARRIER, 
                             MCA_PML_BASE_SEND_STANDARD, comm);
      if (MPI_SUCCESS != err) {
        return err;
      }
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
  int i;
  int err;
  int peer;
  int dim;
  int hibit;
  int mask;
  int size = lam_comm_size(comm);
  int rank = lam_comm_rank(comm);

  /* Send null-messages up and down the tree.  Synchronization at the
     root (rank 0). */

  dim = comm->c_cube_dim;
  hibit = lam_hibit(rank, dim);
  --dim;

  /* Receive from children. */

  for (i = dim, mask = 1 << i; i > hibit; --i, mask >>= 1) {
    peer = rank | mask;
    if (peer < size) {
      err = mca_pml.pml_recv(NULL, 0, MPI_BYTE, peer,
                             MCA_COLL_BASE_TAG_BARRIER, 
                             comm, MPI_STATUS_IGNORE);
      if (MPI_SUCCESS != err) {
	return err;
      }
    }
  }

  /* Send to and receive from parent. */

  if (rank > 0) {
    peer = rank & ~(1 << hibit);
    err = mca_pml.pml_send(NULL, 0, MPI_BYTE, peer, MCA_COLL_BASE_TAG_BARRIER, 
                           MCA_PML_BASE_SEND_STANDARD, comm);
    if (MPI_SUCCESS != err) {
      return err;
    }

    err = mca_pml.pml_recv(NULL, 0, MPI_BYTE, peer,
                           MCA_COLL_BASE_TAG_BARRIER, 
                           comm, MPI_STATUS_IGNORE);
  }

  /* Send to children. */

  for (i = hibit + 1, mask = 1 << i; i <= dim; ++i, mask <<= 1) {
    peer = rank | mask;
    if (peer < size) {
      err = mca_pml.pml_send(NULL, 0, MPI_BYTE, peer,
                             MCA_COLL_BASE_TAG_BARRIER, 
                             MCA_PML_BASE_SEND_STANDARD, comm);
      if (MPI_SUCCESS != err) {
	return err;
      }
    }
  }

  /* All done */

  return MPI_SUCCESS;
}
