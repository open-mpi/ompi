/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic_config.h"

#include <stdio.h>

#include "lam/constants.h"
#include "lam/mem/malloc.h"
#include "mpi.h"
#include "mca/mpi/coll/coll.h"
#include "mca/mpi/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	exscan
 *
 *	Function:	- basic exscan operation
 *	Accepts:	- same arguments as MPI_Exccan()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_exscan(void *sbuf, void *rbuf, int count,
                          MPI_Datatype dtype, MPI_Op op, MPI_Comm comm)
{
#if 1
  return LAM_ERR_NOT_IMPLEMENTED;
#else
  int size;
  int rank;
  int err;
  char *origin, *tmpbuf = NULL;
  char *gathered_buffer = NULL, *gathered_origin;

  /* Initialize. */

  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  /* JMS: Need to replace lots things in this file: lam_dt* stuff with
     lam_datatype_*() functions.  Also need to replace lots of
     MPI_Send/MPI_Recv with negative tags and PML entry points. */

  /* Otherwise receive previous buffer and reduce. Store the recieved
     buffer in different array and then send the reduced array to the
     next process */
  
  /* JMS Need to replace this with some lam_datatype_*() function */
  err = lam_dtbuffer(dtype, count, &gathered_buffer, &gathered_origin);
  if (MPI_SUCCESS != err) {
    return err;
  }

  if (0 != rank) {
    if (!op->op_commute) {

      /* JMS Need to replace with this some lam_datatype_*() function */
      err = lam_dtbuffer(dtype, count, &tmpbuf, &origin);
      if (MPI_SUCCESS != err) {
	if (NULL != gathered_buffer) {
	  LAM_FREE(gathered_buffer);
        }
	return err;
      }
      
      /* Copy the send buffer into the receive buffer. */
      
      /* JMS Need to replace with this some lam_datatype_*() function */
      err = lam_dtsndrcv(sbuf, count, dtype, rbuf,
			 count, dtype, BLKMPIEXSCAN, comm);
      if (MPI_SUCCESS != err) {
	if (NULL != gathered_buffer) {
	  LAM_FREE(gathered_buffer);
        }
	if (NULL != tmpbuf) {
	  LAM_FREE(tmpbuf);
        }
	return err;
      }
      
      /* JMS Need to replace this with negative tags and PML entry
         point */
      err = MPI_Recv(origin, count, dtype,
		     rank - 1, BLKMPIEXSCAN, comm, MPI_STATUS_IGNORE);
      /* JMS Need to add error checking here */

      /* JMS Need to replace with this some lam_datatype_*() function */
      err = lam_dtsndrcv(origin, count, dtype, gathered_origin,
			 count, dtype, BLKMPIEXSCAN, comm);
    } else {
      origin = sbuf;
      
      /* JMS Need to replace this with negative tags and PML entry
         point */
      err = MPI_Recv(rbuf, count, dtype,
		     rank - 1, BLKMPIEXSCAN, comm, MPI_STATUS_IGNORE);
      
      if (MPI_SUCCESS != err) {
	if (NULL != gathered_buffer) {
	  LAM_FREE(gathered_buffer);
        }
	if (NULL != tmpbuf) {
	  LAM_FREE(tmpbuf);
        }
	return err;
      }
      
      /* JMS Need to replace with this some lam_datatype_*() function */
      err = lam_dtsndrcv(rbuf, count, dtype, gathered_origin,
			 count, dtype, BLKMPIEXSCAN, comm);
    }
    
    if (err != MPI_SUCCESS) {
      if (NULL != gathered_buffer) {
	LAM_FREE(gathered_buffer);
      }
      if (NULL != tmpbuf) {
	LAM_FREE(tmpbuf);
      }
      return err;
    }
    
    if (op->op_flags & LAM_LANGF77) {
      (op->op_func)(origin, rbuf, &count, &dtype->dt_f77handle);
    } else {
      (op->op_func)(origin, rbuf, &count, &dtype);
    }
  }  

  /* Send the result to next process. */
  
  if (rank < (size - 1)) {
    if (0 == rank)
      err = MPI_Send(sbuf, count, dtype, rank + 1, BLKMPIEXSCAN, comm);
    else 
      err = MPI_Send(rbuf, count, dtype, rank + 1, BLKMPIEXSCAN, comm);
    if (MPI_SUCCESS != err) {
      if (NULL != gathered_buffer) {
	LAM_FREE(gathered_buffer);
      }
      if (NULL != tmpbuf) {
	LAM_FREE(tmpbuf);
      }
      return err;
    }
  }
  
  if (rank != 0) {
    err = lam_dtsndrcv(gathered_origin, count, dtype, rbuf,
		       count, dtype, BLKMPIEXSCAN, comm);
    if (MPI_SUCCESS != err) {
      if (NULL != gathered_buffer) {
	LAM_FREE(gathered_buffer);
      }
      if (NULL != tmpbuf) {
	LAM_FREE(tmpbuf);
      }
      return err;
    }
  }
  
  if (NULL != gathered_buffer)
    LAM_FREE(gathered_buffer);
  if (NULL != tmpbuf)
    LAM_FREE(tmpbuf);

  /* All done */

  return MPI_SUCCESS;
#endif /* 0 */
}
