/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include <stdio.h>

#include "mpi.h"
#include "include/constants.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "op/op.h"


/*
 *	reduce_lin_intra
 *
 *	Function:	- reduction using O(N) algorithm
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_reduce_lin_intra(void *sbuf, void *rbuf, int count,
                                    struct ompi_datatype_t *dtype, 
                                    struct ompi_op_t *op,
                                    int root, struct ompi_communicator_t *comm)
{
  int i;
  int rank;
  int err;
  int size;
  long true_lb, true_extent, lb, extent;
  char *free_buffer = NULL;
  char *pml_buffer = NULL;
  char *inbuf;

  /* Initialize */

  rank = ompi_comm_rank(comm);
  size = ompi_comm_size(comm);

  /* If not root, send data to the root. */

  if (rank != root) {
    err = mca_pml.pml_send(sbuf, count, dtype, root, 
                           MCA_COLL_BASE_TAG_REDUCE, 
                           MCA_PML_BASE_SEND_STANDARD, comm);
    return err;
  }

  /* Root receives and reduces messages.  Allocate buffer to receive
     messages.  This comment applies to all collectives in this basic
     module where we allocate a temporary buffer.  For the next few
     lines of code, it's tremendously complicated how we decided that
     this was the Right Thing to do.  Sit back and enjoy.  And prepare
     to have your mind warped. :-)

     Recall some definitions (I always get these backwards, so I'm
     going to put them here):

     extent: the length from the lower bound to the upper bound -- may
     be considerably larger than the buffer required to hold the data
     (or smaller!  But it's easiest to think about when it's larger).

     true extent: the exact number of bytes required to hold the data
     in the layout pattern in the datatype.

     For example, consider the following buffer (just talking about
     LB, extent, and true extent -- extrapolate for UB; i.e., assume
     the UB equals exactly where the data ends):

         A              B                                       C
         --------------------------------------------------------
         |              |                                       |
         --------------------------------------------------------

     There are multiple cases:

     1. A is what we give to MPI_Send (and friends), and A is where
        the data starts, and C is where the data ends.  In this case:

        - extent: C-A
        - true extent: C-A
        - LB: 0

         A                                                      C
         --------------------------------------------------------
         |                                                      |
         --------------------------------------------------------
         <=======================extent=========================>
         <======================true extent=====================>

     2. A is what we give to MPI_Send (and friends), B is where the
        data starts, and C is where the data ends.  In this case:

        - extent: C-A
        - true extent: C-B
        - LB: positive

         A              B                                       C
         --------------------------------------------------------
         |              |           User buffer                 |
         --------------------------------------------------------
         <=======================extent=========================>
                        <===============true extent=============>

     3. B is what we give to MPI_Send (and friends), A is where the
        data starts, and C is where the data ends.  In this case:

        - extent: C-A
        - true extent: C-A
        - LB: negative

         A              B                                       C
         --------------------------------------------------------
         |              |           User buffer                 |
         --------------------------------------------------------
         <=======================extent=========================>
         <======================true extent=====================>

     4. MPI_BOTTOM is what we give to MPI_Send (and friends), B is
        where the data starts, and C is where the data ends.  In this
        case:

        - extent: C-MPI_BOTTOM
        - true extent: C-B
        - LB: [potentially very large] positive

         MPI_BOTTOM     B                                       C
         --------------------------------------------------------
         |              |           User buffer                 |
         --------------------------------------------------------
         <=======================extent=========================>
                        <===============true extent=============>

     So in all cases, for a temporary buffer, all we need to malloc()
     is a buffer of size true_extent.  We therefore need to know two
     pointer values: what value to give to MPI_Send (and friends) and
     what value to give to free(), because they might not be the same.

     Clearly, what we give to free() is exactly what was returned from
     malloc().  That part is easy.  :-)

     What we give to MPI_Send (and friends) is a bit more complicated.
     Let's take the 4 cases from above:

     1. If A is what we give to MPI_Send and A is where the data
        starts, then clearly we give to MPI_Send what we got back from
        malloc().

     2. If B is what we get back from malloc, but we give A to
        MPI_Send, then the buffer range [A,B) represents "dead space"
        -- no data will be put there.  So it's safe to give B-LB to
        MPI_Send.  More specifically, the LB is positive, so B-LB is
        actually A.

     3. If A is what we get back from malloc, and B is what we give to
        MPI_Send, then the LB is negative, so A-LB will actually equal
        B.

     4. Although this seems like the weirdest case, it's actually
        quite similar to case #2 -- the pointer we give to MPI_Send is
        smaller than the pointer we got back from malloc().

     Hence, in all cases, we give (return_from_malloc - LB) to MPI_Send.

     This works fine and dandy if we only have (count==1), which we
     rarely do.  ;-) So we really need to allocate (true_extent +
     ((count - 1) * extent)) to get enough space for the rest.  This may
     be more than is necessary, but it's ok.

     Simple, no?  :-)

  */

  if (size > 1) {
    ompi_ddt_get_extent(dtype, &lb, &extent);
    ompi_ddt_get_true_extent(dtype, &true_lb, &true_extent);

    free_buffer = malloc(true_extent + (count - 1) * extent);
    if (NULL == free_buffer) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
    pml_buffer = free_buffer - lb;
  }

  /* Initialize the receive buffer. */

  if (rank == (size - 1)) {
    err = ompi_ddt_sndrcv(sbuf, count, dtype, rbuf, count, dtype);
  } else {
    err = mca_pml.pml_recv(rbuf, count, dtype, size - 1,
                           MCA_COLL_BASE_TAG_REDUCE, comm, MPI_STATUS_IGNORE);
  }
  if (MPI_SUCCESS != err) {
    if (NULL != free_buffer) {
      free(free_buffer);
    }
    return err;
  }

  /* Loop receiving and calling reduction function (C or Fortran). */

  for (i = size - 2; i >= 0; --i) {
    if (rank == i) {
      inbuf = sbuf;
    } else {
      err = mca_pml.pml_recv(pml_buffer, count, dtype, i, 
                             MCA_COLL_BASE_TAG_REDUCE, comm, 
                             MPI_STATUS_IGNORE);
      if (MPI_SUCCESS != err) {
	if (NULL != free_buffer) {
	  free(free_buffer);
        }
	return err;
      }

      inbuf = pml_buffer;
    }

    /* Perform the reduction */

    ompi_op_reduce(op, inbuf, rbuf, count, dtype);
  }

  if (NULL != free_buffer) {
    free(free_buffer);
  }

  /* All done */

  return MPI_SUCCESS;
}


/*
 *	reduce_log_intra
 *
 *	Function:	- reduction using O(log N) algorithm
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_reduce_log_intra(void *sbuf, void *rbuf, int count,
                                    struct ompi_datatype_t *dtype, 
                                    struct ompi_op_t *op,
                                    int root, struct ompi_communicator_t *comm)
{
   int i;
   int size;
   int rank;
   int vrank;
   int err;
   int peer;
   int dim;
   int mask;
   long true_lb, true_extent, lb, extent;
   char *free_buffer = NULL;
   char *pml_buffer = NULL;
   char *snd_buffer = sbuf;
   char *rcv_buffer = rbuf;

   /* JMS Codearound for now -- if the operations is not communative,
      just call the linear algorithm.  Need to talk to Edgar / George
      about fixing this algorithm here to work with non-communative
      operations. */

   if (!ompi_op_is_commute(op)) {
       return mca_coll_basic_reduce_lin_intra(sbuf, rbuf, count, dtype, 
                                              op, root, comm);
   }

   /* Some variables */
   size = ompi_comm_size(comm);
   rank = ompi_comm_rank(comm);                       
   vrank = ompi_op_is_commute(op) ? (rank - root + size) % size : rank;
   dim = comm->c_cube_dim;

   /* Allocate the incoming and resulting message buffers.  See lengthy
      rationale above. */

   if (size > 1) {
      ompi_ddt_get_extent(dtype, &lb, &extent);
      ompi_ddt_get_true_extent(dtype, &true_lb, &true_extent);

      free_buffer = malloc(true_extent + (count - 1) * extent);
      if (NULL == free_buffer) {
         return OMPI_ERR_OUT_OF_RESOURCE;
      }

      pml_buffer = free_buffer - lb;
      /* read the comment about commutative operations (few lines down
         the page) */
      if( ompi_op_is_commute(op) ) {
        rcv_buffer = pml_buffer;
      }
   }

   /* Loop over cube dimensions. High processes send to low ones in the
      dimension. */

   for (i = 0, mask = 1; i < dim; ++i, mask <<= 1) {

       /* A high-proc sends to low-proc and stops. */
       if (vrank & mask) {
          peer = vrank & ~mask;
          if (ompi_op_is_commute(op)) {
             peer = (peer + root) % size;
          }

          err = mca_pml.pml_send( snd_buffer, count,
                                  dtype, peer, MCA_COLL_BASE_TAG_REDUCE, 
                                  MCA_PML_BASE_SEND_STANDARD, comm);
          if (MPI_SUCCESS != err) {
             if (NULL != free_buffer) {
                free(free_buffer);
             }
            return err;
          }
          snd_buffer = rbuf;
          break;
       }

       /* A low-proc receives, reduces, and moves to a higher
          dimension. */

      else {
         peer = vrank | mask;
         if (peer >= size) {
            continue;
         }
         if (ompi_op_is_commute(op)) {
            peer = (peer + root) % size;
         }

         /* Most of the time (all except the first one for commutative
            operations) we receive in the user provided buffer
            (rbuf). But the exception is here to allow us to dont have
            to copy from the sbuf to a temporary location. If the
            operation is commutative we dont care in which order we
            apply the operation, so for the first time we can receive
            the data in the pml_buffer and then apply to operation
            between this buffer and the user provided data. */

         err = mca_pml.pml_recv( rcv_buffer, count, dtype, peer,
                                 MCA_COLL_BASE_TAG_REDUCE, comm,
                                 MPI_STATUS_IGNORE );
         if (MPI_SUCCESS != err) {
            if (NULL != free_buffer) {
              free(free_buffer);
            }
            return err;
         }
         /* Perform the operation. The target is always the user
            provided buffer We do the operation only if we receive it
            not in the user buffer */
         if( snd_buffer != sbuf ) {
            /* the target buffer is the locally allocated one */
            ompi_op_reduce(op, rcv_buffer, pml_buffer, count, dtype);
         } else {
            /* If we're commutative, we don't care about the order of
               operations and we can just reduce the operations now.
               If we are not commutative, we have to copy the send
               buffer into a temp buffer (pml_buffer) and then reduce
               what we just received against it. */
            if( !ompi_op_is_commute(op) ) {
               ompi_ddt_sndrcv( sbuf, count, dtype, pml_buffer, count, dtype);
               ompi_op_reduce( op, rbuf, pml_buffer, count, dtype );
            } else {
               ompi_op_reduce(op, sbuf, pml_buffer, count, dtype);
            }
            /* now we have to send the buffer containing the computed data */
            snd_buffer = pml_buffer;  
            /* starting from now we always receive in the user
               provided buffer */
            rcv_buffer = rbuf;  
         }
      }
   }

   /* Get the result to the root if needed. */
   err = MPI_SUCCESS;
   if (0 == vrank) {
      if (root == rank) {
         ompi_ddt_sndrcv( snd_buffer, count, dtype, rbuf, count, dtype);
      } else {
         err = mca_pml.pml_send( snd_buffer, count,
                                 dtype, root, MCA_COLL_BASE_TAG_REDUCE,
                                 MCA_PML_BASE_SEND_STANDARD, comm);
      }
   } else if (rank == root) {
      err = mca_pml.pml_recv( rcv_buffer, count, dtype, 0, 
                              MCA_COLL_BASE_TAG_REDUCE,
                              comm, MPI_STATUS_IGNORE);
      if( rcv_buffer != rbuf ) {
         ompi_op_reduce(op, rcv_buffer, rbuf, count, dtype);
      }
   }

   if (NULL != free_buffer) {
      free(free_buffer);
   }

   /* All done */

   return err;
}


/*
 *	reduce_lin_inter
 *
 *	Function:	- reduction using O(N) algorithm
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_reduce_lin_inter(void *sbuf, void *rbuf, int count,
                                    struct ompi_datatype_t *dtype, 
                                    struct ompi_op_t *op,
                                    int root, struct ompi_communicator_t *comm)
{
  int i;
  int rank;
  int err;
  int size;
  long true_lb, true_extent, lb, extent;
  char *free_buffer = NULL;
  char *pml_buffer = NULL;

  /* Initialize */
  rank = ompi_comm_rank(comm);
  size = ompi_comm_remote_size(comm);

  if ( MPI_PROC_NULL == root ) {
      /* do nothing */
      err = OMPI_SUCCESS;
  }
  else if ( MPI_ROOT != root ) {
      /* If not root, send data to the root. */
      err = mca_pml.pml_send(sbuf, count, dtype, root, 
                             MCA_COLL_BASE_TAG_REDUCE, 
                             MCA_PML_BASE_SEND_STANDARD, comm);
  }
  else {
      /* Root receives and reduces messages  */
      ompi_ddt_get_extent(dtype, &lb, &extent);
      ompi_ddt_get_true_extent(dtype, &true_lb, &true_extent);

      free_buffer = malloc(true_extent + (count - 1) * extent);
      if (NULL == free_buffer) {
          return OMPI_ERR_OUT_OF_RESOURCE;
      }
      pml_buffer = free_buffer - lb;
      

      /* Initialize the receive buffer. */
      err = mca_pml.pml_recv(rbuf, count, dtype, 0,
                             MCA_COLL_BASE_TAG_REDUCE, comm, 
                             MPI_STATUS_IGNORE);
      if (MPI_SUCCESS != err) {
          if (NULL != free_buffer) {
              free(free_buffer);
          }
          return err;
      }

      /* Loop receiving and calling reduction function (C or Fortran). */
      for (i = 1; i < size; i++) {
          err = mca_pml.pml_recv(pml_buffer, count, dtype, i, 
                                 MCA_COLL_BASE_TAG_REDUCE, comm, 
                                 MPI_STATUS_IGNORE);
          if (MPI_SUCCESS != err) {
              if (NULL != free_buffer) {
                  free(free_buffer);
              }
              return err;
          }

          /* Perform the reduction */
          ompi_op_reduce(op, pml_buffer, rbuf, count, dtype);
      }
  
      if (NULL != free_buffer) {
          free(free_buffer);
      }
  }
  
  /* All done */
  return err;
}


/*
 *	reduce_log_inter
 *
 *	Function:	- reduction using O(N) algorithm
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_basic_reduce_log_inter(void *sbuf, void *rbuf, int count,
                                    struct ompi_datatype_t *dtype, 
                                    struct ompi_op_t *op,
                                    int root, struct ompi_communicator_t *comm)
{
  return OMPI_ERR_NOT_IMPLEMENTED;
}
