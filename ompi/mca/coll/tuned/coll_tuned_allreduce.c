/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/op/op.h"
#include "coll_tuned.h"
#include "coll_tuned_topo.h"
#include "coll_tuned_util.h"

/*
 * ompi_coll_tuned_allreduce_intra_nonoverlapping
 *
 * This function just calls a reduce followed by a broadcast
 * both called functions are tuned but they complete sequentially,
 * i.e. no additional overlapping
 * meaning if the number of segments used is greater than the topo depth 
 * then once the first segment of data is fully 'reduced' it is not broadcast
 * while the reduce continues (cost = cost-reduce + cost-bcast + decision x 3)
 *
 */
int
ompi_coll_tuned_allreduce_intra_nonoverlapping(void *sbuf, void *rbuf, int count,
                                               struct ompi_datatype_t *dtype,
                                               struct ompi_op_t *op,
                                               struct ompi_communicator_t *comm)
{
    int err;
    int rank;

    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:allreduce_intra_nonoverlapping rank %d", rank));

    /* Reduce to 0 and broadcast. */

    if (MPI_IN_PLACE == sbuf) {
        if (0 == ompi_comm_rank(comm)) {
            err = comm->c_coll.coll_reduce (MPI_IN_PLACE, rbuf, count, dtype, 
                                            op, 0, comm);
        } else {
            err = comm->c_coll.coll_reduce (rbuf, NULL, count, dtype, op, 0, 
                                            comm);
        }
    } else {
        err = comm->c_coll.coll_reduce (sbuf, rbuf, count, dtype, op, 0, comm);
    }
    if (MPI_SUCCESS != err) {
        return err;
    }

    return comm->c_coll.coll_bcast (rbuf, count, dtype, 0, comm);
}

/*
 *   ompi_coll_tuned_allreduce_intra_recursivedoubling
 *
 *   Function:       Recursive doubling algorithm for allreduce operation
 *   Accepts:        Same as MPI_Allreduce()
 *   Returns:        MPI_SUCCESS or error code
 *
 *   Description:    Implements recursive doubling algorithm for allreduce.  
 *                   Original (non-segmented) implementation is used in MPICH-2 
 *                   for small and intermediate size messages.
 *                   The algorithm preserves order of operations so it can 
 *                   be used both by commutative and non-commutative operations.
 *
 *         Example on 7 nodes:
 *         Initial state
 *         #      0       1      2       3      4       5      6 
 *               [0]     [1]    [2]     [3]    [4]     [5]    [6]
 *         Initial adjustment step for non-power of two nodes.
 *         old rank      1              3              5      6
 *         new rank      0              1              2      3
 *                     [0+1]          [2+3]          [4+5]   [6]
 *         Step 1
 *         old rank      1              3              5      6
 *         new rank      0              1              2      3
 *                     [0+1+]         [0+1+]         [4+5+]  [4+5+]
 *                     [2+3+]         [2+3+]         [6   ]  [6   ]
 *         Step 2
 *         old rank      1              3              5      6
 *         new rank      0              1              2      3
 *                     [0+1+]         [0+1+]         [0+1+]  [0+1+]
 *                     [2+3+]         [2+3+]         [2+3+]  [2+3+]  
 *                     [4+5+]         [4+5+]         [4+5+]  [4+5+]
 *                     [6   ]         [6   ]         [6   ]  [6   ]
 *         Final adjustment step for non-power of two nodes
 *         #      0       1      2       3      4       5      6 
 *              [0+1+] [0+1+] [0+1+]  [0+1+] [0+1+]  [0+1+] [0+1+]
 *              [2+3+] [2+3+] [2+3+]  [2+3+] [2+3+]  [2+3+] [2+3+] 
 *              [4+5+] [4+5+] [4+5+]  [4+5+] [4+5+]  [4+5+] [4+5+]
 *              [6   ] [6   ] [6   ]  [6   ] [6   ]  [6   ] [6   ]
 *
 */
int 
ompi_coll_tuned_allreduce_intra_recursivedoubling(void *sbuf, void *rbuf, 
                                                  int count,
                                                  struct ompi_datatype_t *dtype,
                                                  struct ompi_op_t *op,
                                                  struct ompi_communicator_t *comm) 
{
   int ret, line;
   int rank, size, adjsize, remote, distance;
   int newrank, newremote, extra_ranks;
   char *tmpsend = NULL, *tmprecv = NULL, *tmpswap = NULL, *inplacebuf = NULL;
   ptrdiff_t true_lb, true_extent, lb, extent;
   ompi_request_t *reqs[2] = {NULL, NULL};

   size = ompi_comm_size(comm);
   rank = ompi_comm_rank(comm);

   OPAL_OUTPUT((ompi_coll_tuned_stream,
                "coll:tuned:allreduce_intra_recursivedoubling rank %d", rank));
   
   /* Special case for size == 1 */
   if (1 == size) {
      if (MPI_IN_PLACE != sbuf) {
         ret = ompi_ddt_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
         if (ret < 0) { line = __LINE__; goto error_hndl; }
      }
      return MPI_SUCCESS;
   }

   /* Allocate and initialize temporary send buffer */
   ret = ompi_ddt_get_extent(dtype, &lb, &extent);
   if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
   ret = ompi_ddt_get_true_extent(dtype, &true_lb, &true_extent);
   if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

   inplacebuf = (char*) malloc(true_extent + (count - 1) * extent);
   if (NULL == inplacebuf) { ret = -1; line = __LINE__; goto error_hndl; }

   if (MPI_IN_PLACE == sbuf) {
      ret = ompi_ddt_copy_content_same_ddt(dtype, count, inplacebuf, (char*)rbuf);
      if (ret < 0) { line = __LINE__; goto error_hndl; }
   } else {
      ret = ompi_ddt_copy_content_same_ddt(dtype, count, inplacebuf, (char*)sbuf);
      if (ret < 0) { line = __LINE__; goto error_hndl; }
   }

   tmpsend = (char*) inplacebuf;
   tmprecv = (char*) rbuf;

   /* Determine nearest power of two less than or equal to size */
   for (adjsize = 0x1; adjsize <= size; adjsize <<= 1); adjsize = adjsize >> 1;

   /* Handle non-power-of-two case:
      - Even ranks less than 2 * extra_ranks send their data to (rank + 1), and 
        sets new rank to -1.
      - Odd ranks less than 2 * extra_ranks receive data from (rank - 1), 
        apply appropriate operation, and set new rank to rank/2
      - Everyone else sets rank to rank - extra_ranks
    */
   extra_ranks = size - adjsize;
   if (rank <  (2 * extra_ranks)) {
      if (0 == (rank % 2)) {
         ret = MCA_PML_CALL(send(tmpsend, count, dtype, (rank + 1), 
                                 MCA_COLL_BASE_TAG_ALLREDUCE,
                                 MCA_PML_BASE_SEND_STANDARD, comm));
         if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
         newrank = -1;
      } else {
         ret = MCA_PML_CALL(recv(tmprecv, count, dtype, (rank - 1),
                                 MCA_COLL_BASE_TAG_ALLREDUCE, comm,
                                 MPI_STATUS_IGNORE));
         if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
         /* tmpsend = tmprecv (op) tmpsend */
         ompi_op_reduce(op, tmprecv, tmpsend, count, dtype);
         newrank = rank >> 1;
      }
   } else {
      newrank = rank - extra_ranks;
   }

   /* Communication/Computation loop 
      - Exchange message with remote node.
      - Perform appropriate operation taking in account order of operations:
        result = value (op) result
    */
   for (distance = 0x1; distance < adjsize; distance <<=1) {
      if (newrank < 0) break;
      /* Determine remote node */
      newremote = newrank ^ distance;
      remote = (newremote < extra_ranks)? 
         (newremote * 2 + 1):(newremote + extra_ranks);

      /* Exchange the data */
      ret = MCA_PML_CALL(irecv(tmprecv, count, dtype, remote,
                               MCA_COLL_BASE_TAG_ALLREDUCE, comm, &reqs[0]));
      if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
      ret = MCA_PML_CALL(isend(tmpsend, count, dtype, remote, 
                               MCA_COLL_BASE_TAG_ALLREDUCE,
                               MCA_PML_BASE_SEND_STANDARD, comm, &reqs[1]));
      if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
      ret = ompi_request_wait_all(2, reqs, MPI_STATUSES_IGNORE);
      if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

      /* Apply operation */
      if (rank < remote) {
         /* tmprecv = tmpsend (op) tmprecv */
         ompi_op_reduce(op, tmpsend, tmprecv, count, dtype);
         tmpswap = tmprecv;
         tmprecv = tmpsend;
         tmpsend = tmpswap;
      } else {
         /* tmpsend = tmprecv (op) tmpsend */
         ompi_op_reduce(op, tmprecv, tmpsend, count, dtype);
      }
   }

   /* Handle non-power-of-two case:
      - Odd ranks less than 2 * extra_ranks send result from tmpsend to 
        (rank - 1)
      - Even ranks less than 2 * extra_ranks receive result from (rank + 1)
   */
   if (rank < (2 * extra_ranks)) {
      if (0 == (rank % 2)) {
         ret = MCA_PML_CALL(recv(rbuf, count, dtype, (rank + 1),
                                 MCA_COLL_BASE_TAG_ALLREDUCE, comm, 
                                 MPI_STATUS_IGNORE));
         if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
         tmpsend = (char*)rbuf;
      } else {
         ret = MCA_PML_CALL(send(tmpsend, count, dtype, (rank - 1),
                                 MCA_COLL_BASE_TAG_ALLREDUCE,
                                 MCA_PML_BASE_SEND_STANDARD, comm));
         if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
      }
   }

   /* Ensure that the final result is in rbuf */
   if (tmpsend != rbuf) {
      ret = ompi_ddt_copy_content_same_ddt(dtype, count, (char*)rbuf, tmpsend);
      if (ret < 0) { line = __LINE__; goto error_hndl; }
   }

   if (NULL != inplacebuf) free(inplacebuf);
   return MPI_SUCCESS;

 error_hndl:
   OPAL_OUTPUT((ompi_coll_tuned_stream, "%s:%4d\tRank %d Error occurred %d\n",
                __FILE__, line, rank, ret));
   if (NULL != inplacebuf) free(inplacebuf);
   return ret;
}

/*
 *   ompi_coll_tuned_allreduce_intra_ring
 *
 *   Function:       Ring algorithm for allreduce operation
 *   Accepts:        Same as MPI_Allreduce()
 *   Returns:        MPI_SUCCESS or error code
 *
 *   Description:    Implements ring algorithm for allreduce: the message is
 *                   automatically segmented to segment of size M/N.
 *                   Algorithm requires 2*N - 1 steps.
 *
 *   Limitations:    The algorithm DOES NOT preserve order of operations so it 
 *                   can be used only for commutative operations.
 *                   In addition, algorithm cannot work if the total count is 
 *                   less than size.
 *         Example on 5 nodes:
 *         Initial state
 *   #      0              1             2              3             4
 *        [00]           [10]          [20]           [30]           [40]
 *        [01]           [11]          [21]           [31]           [41]
 *        [02]           [12]          [22]           [32]           [42]
 *        [03]           [13]          [23]           [33]           [43]
 *        [04]           [14]          [24]           [34]           [44]
 *
 *        COMPUTATION PHASE
 *         Step 0: rank r sends block r to rank (r+1) and receives bloc (r-1) 
 *                 from rank (r-1) [with wraparound].
 *    #     0              1             2              3             4
 *        [00]          [00+10]        [20]           [30]           [40]
 *        [01]           [11]         [11+21]         [31]           [41]
 *        [02]           [12]          [22]          [22+32]         [42]
 *        [03]           [13]          [23]           [33]         [33+43]
 *      [44+04]          [14]          [24]           [34]           [44]
 *
 *         Step 1: rank r sends block (r-1) to rank (r+1) and receives bloc 
 *                 (r-2) from rank (r-1) [with wraparound].
 *    #      0              1             2              3             4
 *         [00]          [00+10]     [01+10+20]        [30]           [40]
 *         [01]           [11]         [11+21]      [11+21+31]        [41]
 *         [02]           [12]          [22]          [22+32]      [22+32+42]
 *      [33+43+03]        [13]          [23]           [33]         [33+43]
 *        [44+04]       [44+04+14]       [24]           [34]           [44]
 *
 *         Step 2: rank r sends block (r-2) to rank (r+1) and receives bloc 
 *                 (r-2) from rank (r-1) [with wraparound].
 *    #      0              1             2              3             4
 *         [00]          [00+10]     [01+10+20]    [01+10+20+30]      [40]
 *         [01]           [11]         [11+21]      [11+21+31]    [11+21+31+41]
 *     [22+32+42+02]      [12]          [22]          [22+32]      [22+32+42]
 *      [33+43+03]    [33+43+03+13]     [23]           [33]         [33+43]
 *        [44+04]       [44+04+14]  [44+04+14+24]      [34]           [44]
 *
 *         Step 3: rank r sends block (r-3) to rank (r+1) and receives bloc 
 *                 (r-3) from rank (r-1) [with wraparound].
 *    #      0              1             2              3             4
 *         [00]          [00+10]     [01+10+20]    [01+10+20+30]     [FULL]
 *        [FULL]           [11]        [11+21]      [11+21+31]    [11+21+31+41]
 *     [22+32+42+02]     [FULL]          [22]         [22+32]      [22+32+42]
 *      [33+43+03]    [33+43+03+13]     [FULL]          [33]         [33+43]
 *        [44+04]       [44+04+14]  [44+04+14+24]      [FULL]         [44]
 *         
 *        DISTRIBUTION PHASE: ring ALLGATHER with ranks shifted by 1.
 *
 */
int 
ompi_coll_tuned_allreduce_intra_ring(void *sbuf, void *rbuf, int count,
                                     struct ompi_datatype_t *dtype,
                                     struct ompi_op_t *op,
                                     struct ompi_communicator_t *comm) 
{
   int ret, line;
   int rank, size, k, recvfrom, sendto;
   int segcount, lastsegcount, maxsegcount;
   int blockcount, inbi;
   size_t typelng;
   char *tmpsend = NULL, *tmprecv = NULL;
   char *inbuf[2] = {NULL, NULL};
   ptrdiff_t true_lb, true_extent, lb, extent, realsegsize, maxrealsegsize;
   ompi_request_t *reqs[2] = {NULL, NULL};

   size = ompi_comm_size(comm);
   rank = ompi_comm_rank(comm);

   OPAL_OUTPUT((ompi_coll_tuned_stream,
                "coll:tuned:allreduce_intra_ring rank %d, count %d", rank, count));
      
   /* Special case for size == 1 */
   if (1 == size) {
      if (MPI_IN_PLACE != sbuf) {
         ret = ompi_ddt_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
         if (ret < 0) { line = __LINE__; goto error_hndl; }
      }
      return MPI_SUCCESS;
   }

   /* Special case for count less than size - 1 - use recursive doubling */
   if (count < size - 1) {
      OPAL_OUTPUT((ompi_coll_tuned_stream, "coll:tuned:allreduce_ring rank %d/%d, count %d, switching to recursive doubling", rank, size, count));
      return (ompi_coll_tuned_allreduce_intra_recursivedoubling(sbuf, rbuf, 
                                                                count,
                                                                dtype, op, 
                                                                comm));
   }

   /* Allocate and initialize temporary buffers */
   ret = ompi_ddt_get_extent(dtype, &lb, &extent);
   if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
   ret = ompi_ddt_get_true_extent(dtype, &true_lb, &true_extent);
   if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
   ret = ompi_ddt_type_size( dtype, &typelng);
   if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

   /* Determine number of elements per block.  
      This is not the same computation as the one for number of elements 
      per segment - and we may end up having last block larger any other block.
    */
   segcount = count / size;
   if (0 != count % size) segcount++;
   lastsegcount = count - (size - 1) * segcount;
   if (lastsegcount <= 0) {
      segcount--;
      lastsegcount = count - (size - 1) * segcount;
   }
   realsegsize = segcount * extent;
   maxsegcount = (segcount > lastsegcount)? segcount : lastsegcount;
   maxrealsegsize = true_extent + (maxsegcount - 1) * extent;

   inbuf[0] = (char*)malloc(maxrealsegsize);
   if (NULL == inbuf[0]) { ret = -1; line = __LINE__; goto error_hndl; }
   if (size > 2) {
      inbuf[1] = (char*)malloc(maxrealsegsize);
      if (NULL == inbuf[1]) { ret = -1; line = __LINE__; goto error_hndl; }
   }

   /* Handle MPI_IN_PLACE */
   if (MPI_IN_PLACE != sbuf) {
      ret = ompi_ddt_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
      if (ret < 0) { line = __LINE__; goto error_hndl; }
   }

   /* Computation loop */

   /* 
      For each of the remote nodes:
      - post irecv for block (r-1)
      - send block (r)
      - in loop for every step k = 2 .. n
         - post irecv for block (r + n - k) % n
         - wait on block (r + n - k + 1) % n to arrive
         - compute on block (r + n - k + 1) % n
         - send block (r + n - k + 1) % n
     - wait on block (r + 1)
     - compute on block (r + 1)
     - send block (r + 1) to rank (r + 1)
     Note that for send operations and computation we must compute the exact 
     block size.
    */
   sendto = (rank + 1) % size;
   recvfrom = (rank + size - 1) % size;

   inbi = 0;
   tmpsend = ((char*)rbuf) + rank * realsegsize;
   /* Initialize first receive from the neighbor on the left */
   ret = MCA_PML_CALL(irecv(inbuf[inbi], maxsegcount, dtype, recvfrom,
                            MCA_COLL_BASE_TAG_ALLREDUCE, comm, &reqs[inbi]));
   if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
   /* Send first block to the neighbor on the right */
   blockcount = segcount;
   if ((size - 1) == rank) { blockcount = lastsegcount; }
   ret = MCA_PML_CALL(send(tmpsend, blockcount, dtype, sendto,
                           MCA_COLL_BASE_TAG_ALLREDUCE,
                           MCA_PML_BASE_SEND_STANDARD, comm));
   if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
   
   for (k = 2; k < size; k++) {
      const int prevblock = (rank + size - k + 1) % size;
      
      inbi = inbi ^ 0x1;
      
      /* Post irecv for the current block */
      ret = MCA_PML_CALL(irecv(inbuf[inbi], maxsegcount, dtype, recvfrom,
                               MCA_COLL_BASE_TAG_ALLREDUCE, comm, &reqs[inbi]));
      if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
      
      /* Wait on previous block to arrive */
      ret = ompi_request_wait(&reqs[inbi ^ 0x1], MPI_STATUS_IGNORE);
      if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
      
      /* Apply operation on previous block: result goes to rbuf
         rbuf[prevblock] = inbuf[inbi ^ 0x1] (op) rbuf[prevblock]
      */
      blockcount = segcount;
      if ((size - 1) == prevblock) { blockcount = lastsegcount; }
      tmprecv = ((char*)rbuf) + prevblock * realsegsize;
      ompi_op_reduce(op, inbuf[inbi ^ 0x1], tmprecv, blockcount, dtype);
      
      /* send previous block to sendto */
      ret = MCA_PML_CALL(send(tmprecv, blockcount, dtype, sendto,
                              MCA_COLL_BASE_TAG_ALLREDUCE,
                              MCA_PML_BASE_SEND_STANDARD, comm));
      if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
   }

   /* Wait on the last block to arrive */
   ret = ompi_request_wait(&reqs[inbi], MPI_STATUS_IGNORE);
   if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

   /* Apply operation on the last block (from neighbor (rank + 1) 
      rbuf[rank+1] = inbuf[inbi] (op) rbuf[rank + 1] */
   blockcount = segcount;
   if ((size - 1) == (rank + 1) % size) { blockcount = lastsegcount; }
   tmprecv = ((char*)rbuf) + ((rank + 1) % size) * realsegsize;
   ompi_op_reduce(op, inbuf[inbi], tmprecv, blockcount, dtype);
   
   /* Distribution loop - variation of ring allgather */
   for (k = 0; k < size - 1; k++) {
      const int recvdatafrom = (rank + size - k) % size;
      const int senddatafrom = (rank + 1 + size - k) % size;

      blockcount = segcount;
      if ((size - 1) == senddatafrom) blockcount = lastsegcount;

      tmprecv = (char*)rbuf + recvdatafrom * realsegsize;
      tmpsend = (char*)rbuf + senddatafrom * realsegsize;

      ret = ompi_coll_tuned_sendrecv(tmpsend, blockcount, dtype, sendto,
                                     MCA_COLL_BASE_TAG_ALLREDUCE,
                                     tmprecv, maxsegcount, dtype, recvfrom,
                                     MCA_COLL_BASE_TAG_ALLREDUCE,
                                     comm, MPI_STATUS_IGNORE, rank);
      if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl;}

   }

   if (NULL != inbuf[0]) free(inbuf[0]);
   if (NULL != inbuf[1]) free(inbuf[1]);

   return MPI_SUCCESS;

 error_hndl:
   OPAL_OUTPUT((ompi_coll_tuned_stream, "%s:%4d\tRank %d Error occurred %d\n",
                __FILE__, line, rank, ret));
   if (NULL != inbuf[0]) free(inbuf[0]);
   if (NULL != inbuf[1]) free(inbuf[1]);
   return ret;
}

/*
 * Linear functions are copied from the BASIC coll module
 * they do not segment the message and are simple implementations
 * but for some small number of nodes and/or small data sizes they 
 * are just as fast as tuned/tree based segmenting operations 
 * and as such may be selected by the decision functions
 * These are copied into this module due to the way we select modules
 * in V1. i.e. in V2 we will handle this differently and so will not
 * have to duplicate code.
 * GEF Oct05 after asking Jeff.
 */

/* copied function (with appropriate renaming) starts here */


/*
 *	allreduce_intra
 *
 *	Function:	- allreduce using other MPI collectives
 *	Accepts:	- same as MPI_Allreduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
ompi_coll_tuned_allreduce_intra_basic_linear(void *sbuf, void *rbuf, int count,
                                             struct ompi_datatype_t *dtype,
                                             struct ompi_op_t *op,
                                             struct ompi_communicator_t *comm)
{
    int err;
    int rank;

    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:allreduce_intra_basic_linear rank %d", rank));

    /* Reduce to 0 and broadcast. */

    if (MPI_IN_PLACE == sbuf) {
        if (0 == ompi_comm_rank(comm)) {
            err = ompi_coll_tuned_reduce_intra_basic_linear (MPI_IN_PLACE, rbuf, count, dtype, op, 0, comm);
        } else {
            err = ompi_coll_tuned_reduce_intra_basic_linear(rbuf, NULL, count, dtype, op, 0, comm);
        }
    } else {
        err = ompi_coll_tuned_reduce_intra_basic_linear(sbuf, rbuf, count, dtype, op, 0, comm);
    }
    if (MPI_SUCCESS != err) {
        return err;
    }

    return ompi_coll_tuned_bcast_intra_basic_linear(rbuf, count, dtype, 0, comm);
}


/* copied function (with appropriate renaming) ends here */

/* The following are used by dynamic and forced rules */

/* publish details of each algorithm and if its forced/fixed/locked in */
/* as you add methods/algorithms you must update this and the query/map routines */

/* this routine is called by the component only */
/* this makes sure that the mca parameters are set to their initial values and perms */
/* module does not call this they call the forced_getvalues routine instead */

int ompi_coll_tuned_allreduce_intra_check_forced_init (coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices)
{
    int rc, max_alg = 4, requested_alg;

    ompi_coll_tuned_forced_max_algorithms[ALLREDUCE] = max_alg;

    rc = mca_base_param_reg_int (&mca_coll_tuned_component.super.collm_version,
                                 "allreduce_algorithm_count",
                                 "Number of allreduce algorithms available",
                                 false, true, max_alg, NULL);

    mca_param_indices->algorithm_param_index
        = mca_base_param_reg_int( &mca_coll_tuned_component.super.collm_version,
                                  "allreduce_algorithm",
                                  "Which allreduce algorithm is used. Can be locked down to any of: 0 ignore, 1 basic linear, 2 nonoverlapping (tuned reduce + tuned bcast), 3 recursive doubling, 4 ring",
                                  false, false, 0, NULL);
    mca_base_param_lookup_int( mca_param_indices->algorithm_param_index, &(requested_alg));
    if( requested_alg > max_alg ) {
        if( 0 == ompi_comm_rank( MPI_COMM_WORLD ) ) {
            opal_output( 0, "Allreduce algorithm #%d is not available (range [0..%d]). Switching back to ignore(0)\n",
                         requested_alg, max_alg );
        }
        mca_base_param_set_int( mca_param_indices->algorithm_param_index, 0);
    }

    mca_param_indices->segsize_param_index
        = mca_base_param_reg_int( &mca_coll_tuned_component.super.collm_version,
                                  "allreduce_algorithm_segmentsize",
                                  "Segment size in bytes used by default for allreduce algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation.",
                                  false, false, 0, NULL);
  
    mca_param_indices->tree_fanout_param_index
        = mca_base_param_reg_int( &mca_coll_tuned_component.super.collm_version,
                                  "allreduce_algorithm_tree_fanout",
                                  "Fanout for n-tree used for allreduce algorithms. Only has meaning if algorithm is forced and supports n-tree topo based operation.",
                                  false, false, ompi_coll_tuned_init_tree_fanout, /* get system wide default */
                                  NULL);

    mca_param_indices->chain_fanout_param_index
        = mca_base_param_reg_int( &mca_coll_tuned_component.super.collm_version,
                                  "allreduce_algorithm_chain_fanout",
                                  "Fanout for chains used for allreduce algorithms. Only has meaning if algorithm is forced and supports chain topo based operation.",
                                  false, false,
                                  ompi_coll_tuned_init_chain_fanout, /* get system wide default */
                                  NULL);

    return (MPI_SUCCESS);
}


int ompi_coll_tuned_allreduce_intra_do_forced(void *sbuf, void *rbuf, int count,
                                              struct ompi_datatype_t *dtype,
                                              struct ompi_op_t *op,
                                              struct ompi_communicator_t *comm)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:allreduce_intra_do_forced selected algorithm %d, segment size %d", 
                 comm->c_coll_selected_data->user_forced[ALLREDUCE].algorithm,
                 comm->c_coll_selected_data->user_forced[ALLREDUCE].segsize));

    switch (comm->c_coll_selected_data->user_forced[ALLREDUCE].algorithm) {
    case (0):  return ompi_coll_tuned_allreduce_intra_dec_fixed (sbuf, rbuf, count, dtype, op, comm);
    case (1):  return ompi_coll_tuned_allreduce_intra_basic_linear (sbuf, rbuf, count, dtype, op, comm);
    case (2):  return ompi_coll_tuned_allreduce_intra_nonoverlapping (sbuf, rbuf, count, dtype, op, comm);
    case (3):  return ompi_coll_tuned_allreduce_intra_recursivedoubling (sbuf, rbuf, count, dtype, op, comm);
    case (4):  return ompi_coll_tuned_allreduce_intra_ring (sbuf, rbuf, count, dtype, op, comm);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:allreduce_intra_do_forced attempt to select algorithm %d when only 0-%d is valid?",
                     comm->c_coll_selected_data->user_forced[ALLREDUCE].algorithm, 
                     ompi_coll_tuned_forced_max_algorithms[ALLREDUCE]));
        return (MPI_ERR_ARG);
    } /* switch */

}


int ompi_coll_tuned_allreduce_intra_do_this(void *sbuf, void *rbuf, int count,
                                            struct ompi_datatype_t *dtype,
                                            struct ompi_op_t *op,
                                            struct ompi_communicator_t *comm,
                                            int algorithm, int faninout, int segsize)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:allreduce_intra_do_this algorithm %d topo fan in/out %d segsize %d", 
                 algorithm, faninout, segsize));

    switch (algorithm) {
    case (0):   return ompi_coll_tuned_allreduce_intra_dec_fixed (sbuf, rbuf, count, dtype, op, comm);
    case (1):   return ompi_coll_tuned_allreduce_intra_basic_linear (sbuf, rbuf, count, dtype, op, comm);
    case (2):   return ompi_coll_tuned_allreduce_intra_nonoverlapping (sbuf, rbuf, count, dtype, op, comm);
    case (3):   return ompi_coll_tuned_allreduce_intra_recursivedoubling (sbuf, rbuf, count, dtype, op, comm);
    case (4):   return ompi_coll_tuned_allreduce_intra_ring (sbuf, rbuf, count, dtype, op, comm);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:allreduce_intra_do_this attempt to select algorithm %d when only 0-%d is valid?",
                     algorithm, ompi_coll_tuned_forced_max_algorithms[ALLREDUCE]));
        return (MPI_ERR_ARG);
    } /* switch */

}

