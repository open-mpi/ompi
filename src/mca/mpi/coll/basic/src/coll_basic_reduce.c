/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic_config.h"

#include <stdio.h>

#include "lam/constants.h"
#include "mpi.h"
#include "mca/mpi/coll/coll.h"
#include "mca/mpi/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	reduce_lin
 *
 *	Function:	- reduction using O(N) algorithm
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_reduce_lin(void *sbuf, void *rbuf, int count,
                              MPI_Datatype dtype, MPI_Op op,
                              int root, MPI_Comm comm)
{
#if 1
  return LAM_ERR_NOT_IMPLEMENTED;
#else
  int i;
  int size;
  int rank;
  int err;
  char *buffer = NULL;
  char *origin = NULL;
  char *inbuf;

  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  /* If not root, send data to the root. */

  if (rank != root) {
#if 0
    /* JMS This needs to be replaced with negative tags and direct
       calls into the PML */
    err = MPI_Send(sbuf, count, dtype, root, BLKMPIREDUCE, comm);
#endif
    return err;
  }

  /* Root receives and reduces messages.  Allocate buffer to receive
     messages. */

  if (size > 1) {
#if 0
    /* JMS Needs to be replaced with lam_datatype_*() functions */
    err = lam_dtbuffer(dtype, count, &buffer, &origin);
    if (MPI_SUCCESS != err)
      return err;
#endif
  }

  /* Initialize the receive buffer. */

  if (rank == (size - 1)) {
#if 0
    /* JMS Needs to be replaced with lam_datatype_*() functions */
    err = lam_dtsndrcv(sbuf, count, dtype, rbuf, count,
		       dtype, BLKMPIREDUCE, comm);
#endif
  } else {
#if 0
    /* JMS This needs to be replaced with negative tags and direct
       calls into the PML */
    err = MPI_Recv(rbuf, count, dtype, size - 1,
		   BLKMPIREDUCE, comm, MPI_STATUS_IGNORE);
#endif
  }
  if (MPI_SUCCESS != err) {
    if (NULL != buffer)
      free(buffer);
    return err;
  }

  /* Loop receiving and calling reduction function (C or Fortran). */

  for (i = size - 2; i >= 0; --i) {
    if (rank == i) {
      inbuf = sbuf;
    } else {
#if 0
      /* JMS This needs to be replaced with negative tags and direct
         calls into the PML */
      err = MPI_Recv(origin, count, dtype, i, BLKMPIREDUCE, comm, 
		     MPI_STATUS_IGNORE);
#endif
      if (MPI_SUCCESS != err) {
	if (NULL != buffer)
	  free(buffer);
	return err;
      }

      inbuf = origin;
    }

    /* Call reduction function. */

#if 0
    /* JMS Need MPI_Op */
    if (op->op_flags & LAM_LANGF77) {
      (op->op_func)(inbuf, rbuf, &count, &dtype->dt_f77handle);
    } else {
      (op->op_func)(inbuf, rbuf, &count, &dtype);
    }
#endif
  }

  if (NULL != buffer)
    free(buffer);

  /* All done */

  return (MPI_SUCCESS);
#endif
}


/*
 *	reduce_log
 *
 *	Function:	- reduction using O(log N) algorithm
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_reduce_log(void *sbuf, void *rbuf, int count,
                              MPI_Datatype dtype, MPI_Op op,
                              int root, MPI_Comm comm)
{
#if 1
  return LAM_ERR_NOT_IMPLEMENTED;
#else
  int i;
  int size;
  int rank;
  int vrank;
  int err;
  int peer;
  int dim;
  int mask;
  int fl_recv;
  char *buf1;
  char *buf2;
  char *origin1;
  char *origin2;
  void *inmsg;
  void *resmsg;

  /* Allocate the incoming and resulting message buffers. */

#if 0
    /* JMS Needs to be replaced with lam_datatype_*() functions */
  err = lam_dtbuffer(dtype, count, &buf1, &origin1);
  if (MPI_SUCCESS != err)
    return err;

  err = lam_dtbuffer(dtype, count, &buf2, &origin2);
  if (MPI_SUCCESS != err) {
    if (NULL != buf1)
      free(buf1);
    return err;
  }
#endif

  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);
#if 0
  /* JMS Need MPI_Op */
  vrank = (op->op_commute) ? (rank - root + size) % size : rank;
#endif
#if 0
  /* JMS Need to cache this somewhere */
  dim = comm->c_cube_dim;
#endif

  /* Loop over cube dimensions. High processes send to low ones in the
     dimension. */

  inmsg = origin1;
  resmsg = origin2;
  fl_recv = 0;
  for (i = 0, mask = 1; i < dim; ++i, mask <<= 1) {

    /* A high-proc sends to low-proc and stops. */

    if (vrank & mask) {
      peer = vrank & ~mask;
#if 0
      /* JMS Need MPI_Op */
      if (op->op_commute)
	peer = (peer + root) % size;
#endif

#if 0
      /* JMS This needs to be replaced with negative tags and direct
         calls into the PML */
      err = MPI_Send((fl_recv) ? resmsg : sbuf, count,
		     dtype, peer, BLKMPIREDUCE, comm);
#endif
      if (MPI_SUCCESS != err) {
	if (NULL != buf1)
	  free(buf1);
	if (NULL != buf2)
	  free(buf2);
	return err;
      }

      break;
    }

    /* A low-proc receives, reduces, and moves to a higher
       dimension. */

    else {
      peer = vrank | mask;
      if (peer >= size)
	continue;
#if 0
      /* JMS Need MPI_Op */
      if (op->op_commute)
	peer = (peer + root) % size;
#endif

      fl_recv = 1;
#if 0
      /* JMS This needs to be replaced with negative tags and direct
         calls into the PML */
      err = MPI_Recv(inmsg, count, dtype, peer,
		     BLKMPIREDUCE, comm, MPI_STATUS_IGNORE);
#endif
      if (MPI_SUCCESS != err) {
	if (NULL != buf1)
	  free(buf1);
	if (NULL != buf2)
	  free(buf2);
	return err;
      }

#if 0
      /* JMS Need MPI_Op */
      if (op->op_flags & LAM_LANGF77) {
	(*op->op_func)((i > 0) ? resmsg : sbuf,
                       inmsg, &count, &dtype->dt_f77handle);
      } else {
	(*op->op_func)((i > 0) ? resmsg : sbuf, inmsg, &count, &dtype);
      }
#endif

      if (inmsg == origin1) {
	resmsg = origin1;
	inmsg = origin2;
      } else {
	resmsg = origin2;
	inmsg = origin1;
      }
    }
  }

  /* Get the result to the root if needed. */

  err = MPI_SUCCESS;
  if (0 == vrank) {
    if (root == rank) {
#if 0
      /* JMS Needs to be replaced with lam_datatype_*() functions */
      lam_dtcpy(rbuf, (i > 0) ? resmsg : sbuf, count, dtype);
#endif
    } else {
#if 0
      /* JMS This needs to be replaced with negative tags and direct
         calls into the PML */
      err = MPI_Send((i > 0) ? resmsg : sbuf, count,
		     dtype, root, BLKMPIREDUCE, comm);
#endif
    }
  } else if (rank == root) {
#if 0
      /* JMS This needs to be replaced with negative tags and direct
         calls into the PML */
    err = MPI_Recv(rbuf, count, dtype, 0, BLKMPIREDUCE, comm, 
		   MPI_STATUS_IGNORE);
#endif
  }

  if (NULL != buf1)
    free(buf1);
  if (NULL != buf2)
    free(buf2);

  /* All done */

  return err;
#endif
}
