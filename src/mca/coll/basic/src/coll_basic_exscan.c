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
#include "mca/op/op.h"
#include "coll_basic.h"


/*
 *	exscan_intra
 *
 *	Function:	- basic exscan operation
 *	Accepts:	- same arguments as MPI_Exccan()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_exscan_intra(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op, 
                                struct ompi_communicator_t *comm)
{
#if 0
  int size;
  int rank;
  int err;
  char *origin, *tmpbuf = NULL;
  char *gathered_buffer = NULL, *gathered_origin;

  /* Initialize. */

  rank = ompi_comm_rank(comm);
  size = ompi_comm_size(comm);

  /* Otherwise receive previous buffer and reduce. Store the recieved
     buffer in different array and then send the reduced array to the
     next process */

  /* JMS Need to replace this with some ompi_datatype_*() function */
  err = ompi_dtbuffer(dtype, count, &gathered_buffer, &gathered_origin);
  if (MPI_SUCCESS != err) {
    return err;
  }

  if (0 != rank) {
    if (!op->op_commute) {

      /* JMS Need to replace with this some ompi_datatype_*() function */
      err = ompi_dtbuffer(dtype, count, &tmpbuf, &origin);
      if (MPI_SUCCESS != err) {
	if (NULL != gathered_buffer) {
	  OMPI_FREE(gathered_buffer);
        }
	return err;
      }
      
      /* Copy the send buffer into the receive buffer. */
      
      /* JMS Need to replace with this some ompi_datatype_*() function */
      err = ompi_dtsndrcv(sbuf, count, dtype, rbuf,
			 count, dtype, BLKMPIEXSCAN, comm);
      if (MPI_SUCCESS != err) {
	if (NULL != gathered_buffer) {
	  OMPI_FREE(gathered_buffer);
        }
	if (NULL != tmpbuf) {
	  OMPI_FREE(tmpbuf);
        }
	return err;
      }
      
      /* JMS Need to replace this with negative tags and PML entry
         point */
      err = MPI_Recv(origin, count, dtype,
		     rank - 1, BLKMPIEXSCAN, comm, MPI_STATUS_IGNORE);
      /* JMS Need to add error checking here */

      /* JMS Need to replace with this some ompi_datatype_*() function */
      err = ompi_dtsndrcv(origin, count, dtype, gathered_origin,
			 count, dtype, BLKMPIEXSCAN, comm);
    } else {
      origin = sbuf;
      
      /* JMS Need to replace this with negative tags and PML entry
         point */
      err = MPI_Recv(rbuf, count, dtype,
		     rank - 1, BLKMPIEXSCAN, comm, MPI_STATUS_IGNORE);
      
      if (MPI_SUCCESS != err) {
	if (NULL != gathered_buffer) {
	  OMPI_FREE(gathered_buffer);
        }
	if (NULL != tmpbuf) {
	  OMPI_FREE(tmpbuf);
        }
	return err;
      }
      
      /* JMS Need to replace with this some ompi_datatype_*() function */
      err = ompi_dtsndrcv(rbuf, count, dtype, gathered_origin,
			 count, dtype, BLKMPIEXSCAN, comm);
    }
    
    if (err != MPI_SUCCESS) {
      if (NULL != gathered_buffer) {
	OMPI_FREE(gathered_buffer);
      }
      if (NULL != tmpbuf) {
	OMPI_FREE(tmpbuf);
      }
      return err;
    }
    
    if (op->op_flags & OMPI_LANGF77) {
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
	OMPI_FREE(gathered_buffer);
      }
      if (NULL != tmpbuf) {
	OMPI_FREE(tmpbuf);
      }
      return err;
    }
  }
  
  if (rank != 0) {
    err = ompi_dtsndrcv(gathered_origin, count, dtype, rbuf,
		       count, dtype, BLKMPIEXSCAN, comm);
    if (MPI_SUCCESS != err) {
      if (NULL != gathered_buffer) {
	OMPI_FREE(gathered_buffer);
      }
      if (NULL != tmpbuf) {
	OMPI_FREE(tmpbuf);
      }
      return err;
    }
  }
  
  if (NULL != gathered_buffer)
    OMPI_FREE(gathered_buffer);
  if (NULL != tmpbuf)
    OMPI_FREE(tmpbuf);
#endif

  /* All done */

  return MPI_SUCCESS;
}


/*
 *	exscan_inter
 *
 *	Function:	- basic exscan operation
 *	Accepts:	- same arguments as MPI_Exccan()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_exscan_inter(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op, 
                                struct ompi_communicator_t *comm)
{
  return OMPI_ERR_NOT_IMPLEMENTED;
}
