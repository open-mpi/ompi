/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include <stdio.h>

#include "mpi.h"
#include "include/constants.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	scan
 *
 *	Function:	- basic scan operation
 *	Accepts:	- same arguments as MPI_Scan()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_scan_intra(void *sbuf, void *rbuf, int count,
                              struct ompi_datatype_t *dtype, 
                              struct ompi_op_t *op, 
                              struct ompi_communicator_t *comm)
{
#if 1
  return OMPI_ERR_NOT_IMPLEMENTED;
#else
  int size;
  int rank;
  int err;
  char *tmpbuf = NULL;
  char *origin;

  /* Initialize */

  rank = ompi_comm_rank(comm);
  size = ompi_comm_size(comm);

  /* If I'm rank 0, initialize the recv buffer. */

  if (0 == rank) {
#if 0
    /* JMS Need to replace this with ompi_datatype_*() functions */
    err = ompi_dtsndrcv(sbuf, count, dtype,
		       rbuf, count, dtype, BLKMPISCAN, comm);
    if (MPI_SUCCESS != err) {
      return err;
    }
#endif
  }

  /* Otherwise receive previous buffer and reduce. */

  else {
#if 0
      /* JMS Need struct ompi_op_t **/
    if (!op->op_commute) {
#else
    if (1) {
#endif
      /* Allocate a temporary buffer. */

#if 0
      /* JMS Need to replace this with ompi_datatype_*() functions */
      err = ompi_dtbuffer(dtype, count, &tmpbuf, &origin);
      if (MPI_SUCCESS != err) {
	return err;
      }
#endif

      /* Copy the send buffer into the receive buffer. */

#if 0
      /* JMS Need to replace this with ompi_datatype_*() functions */
      err = ompi_dtsndrcv(sbuf, count, dtype, rbuf,
			 count, dtype, BLKMPISCAN, comm);
      if (MPI_SUCCESS != err) {
	if (NULL != tmpbuf)
	  free(tmpbuf);
	return err;
      }
#endif

#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
      err = MPI_Recv(origin, count, dtype,
		     rank - 1, BLKMPISCAN, comm, MPI_STATUS_IGNORE);
#endif
    } else {
      origin = sbuf;

#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
      err = MPI_Recv(rbuf, count, dtype,
		     rank - 1, BLKMPISCAN, comm, MPI_STATUS_IGNORE);
#endif
    }

    if (MPI_SUCCESS != err) {
      if (NULL != tmpbuf)
	free(tmpbuf);
      return err;
    }

#if 0
    /* JMS Need struct ompi_op_t **/
    if (op->op_flags & OMPI_LANGF77) {
      (op->op_func)(origin, rbuf, &count, &dtype->dt_f77handle);
    } else {
      (op->op_func)(origin, rbuf, &count, &dtype);
    }
#endif

    if (NULL != tmpbuf)
      free(tmpbuf);
  }

  /* Send result to next process. */

  if (rank < (size - 1)) {
#if 0
    /* JMS: Need to replace this with negative tags and and direct PML
       calls */
    err = MPI_Send(rbuf, count, dtype, rank + 1, BLKMPISCAN, comm);
#endif
    if (MPI_SUCCESS != err) {
      return err;
    }
  }

  /* All done */

  return MPI_SUCCESS;
#endif
}
