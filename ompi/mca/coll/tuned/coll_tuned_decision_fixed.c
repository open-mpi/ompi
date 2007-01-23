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
#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/op/op.h"
#include "coll_tuned.h"
#include "ompi/mca/pml/pml.h"
#include "opal/util/bit_ops.h"


/*
 *  allreduce_intra
 *
 *  Function:   - allreduce using other MPI collectives
 *  Accepts:    - same as MPI_Allreduce()
 *  Returns:    - MPI_SUCCESS or error code
 */
int
ompi_coll_tuned_allreduce_intra_dec_fixed (void *sbuf, void *rbuf, int count,
                                           struct ompi_datatype_t *dtype,
                                           struct ompi_op_t *op,
                                           struct ompi_communicator_t *comm)
{
    size_t dsize, block_dsize;
    const size_t intermediate_message = 10000;
    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_allreduce_intra_dec_fixed"));

    /**
     * Decision function based on MX results from the Grig cluster at UTK.
     * 
     * Currently, linear, recursive doubling, and nonoverlapping algorithms 
     * can handle both commutative and non-commutative operations.
     * Ring algorithm does not support non-commutative operations.
     */
    ompi_ddt_type_size(dtype, &dsize);
    block_dsize = dsize * count;

    if (block_dsize < intermediate_message) {
       return (ompi_coll_tuned_allreduce_intra_recursivedoubling (sbuf, rbuf, 
                                                                  count, dtype,
                                                                  op, comm));
    } 

    if( ompi_op_is_commute(op) ) {
       return (ompi_coll_tuned_allreduce_intra_ring (sbuf, rbuf, count, dtype, 
                                                     op, comm));
    }

    return (ompi_coll_tuned_allreduce_intra_nonoverlapping (sbuf, rbuf, count, 
                                                            dtype, op, comm));
}

/*
 *	alltoall_intra_dec 
 *
 *	Function:	- seletects alltoall algorithm to use
 *	Accepts:	- same arguments as MPI_Alltoall()
 *	Returns:	- MPI_SUCCESS or error code (passed from the bcast implementation)
 */

int ompi_coll_tuned_alltoall_intra_dec_fixed(void *sbuf, int scount, 
                                             struct ompi_datatype_t *sdtype,
                                             void* rbuf, int rcount, 
                                             struct ompi_datatype_t *rdtype, 
                                             struct ompi_communicator_t *comm)
{
    int communicator_size, rank;
    size_t dsize, block_dsize;
#if 0
    size_t total_dsize;
#endif

    communicator_size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* special case */
    if (communicator_size==2) {
        return ompi_coll_tuned_alltoall_intra_two_procs(sbuf, scount, sdtype, 
                                                         rbuf, rcount, rdtype, 
                                                         comm);
    }

    /* Decision function based on measurement on Grig cluster at 
       the University of Tennessee (2GB MX) up to 64 nodes.
       Has better performance for messages of intermediate sizes than the old one */
    /* determine block size */
    ompi_ddt_type_size(sdtype, &dsize);
    block_dsize = dsize * scount;

    if ((block_dsize < 200) && (communicator_size > 12)) {
       return ompi_coll_tuned_alltoall_intra_bruck(sbuf, scount, sdtype, 
                                                   rbuf, rcount, rdtype, comm);

    } else if (block_dsize < 3000) {
       return ompi_coll_tuned_alltoall_intra_basic_linear(sbuf, scount, sdtype, 
                                                          rbuf, rcount, rdtype, 
                                                          comm);
    }

    return ompi_coll_tuned_alltoall_intra_pairwise (sbuf, scount, sdtype, 
                                                    rbuf, rcount, rdtype, comm);

#if 0
    /* previous decision */

    /* else we need data size for decision function */
    ompi_ddt_type_size(sdtype, &dsize);
    total_dsize = dsize * scount * communicator_size;   /* needed for decision */

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_alltoall_intra_dec_fixed rank %d com_size %d msg_length %ld",
                 rank, communicator_size, total_dsize));

    if (communicator_size >= 12 && total_dsize <= 768) {
        return ompi_coll_tuned_alltoall_intra_bruck (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    }
    if (total_dsize <= 131072) {
        return ompi_coll_tuned_alltoall_intra_basic_linear (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    }
    return ompi_coll_tuned_alltoall_intra_pairwise (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
#endif
}


/*
 *	barrier_intra_dec 
 *
 *	Function:	- seletects barrier algorithm to use
 *	Accepts:	- same arguments as MPI_Barrier()
 *	Returns:	- MPI_SUCCESS or error code (passed from the barrier implementation)
 */
int ompi_coll_tuned_barrier_intra_dec_fixed(struct ompi_communicator_t *comm)
{
    int communicator_size = ompi_comm_size(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_barrier_intra_dec_fixed com_size %d",
                 communicator_size));

    if( 2 == communicator_size )
        return ompi_coll_tuned_barrier_intra_two_procs(comm);
    /**
     * Basic optimisation. If we have a power of 2 number of nodes
     * the use the recursive doubling algorithm, otherwise
     * bruck is the one we want.
     */
    {
        bool has_one = false;
        for( ; communicator_size > 0; communicator_size >>= 1 ) {
            if( communicator_size & 0x1 ) {
                if( has_one )
                    return ompi_coll_tuned_barrier_intra_bruck(comm);
                has_one = true;
            }
        }
    }
    return ompi_coll_tuned_barrier_intra_recursivedoubling(comm);
    /*     return ompi_coll_tuned_barrier_intra_linear(comm); */
    /*         return ompi_coll_tuned_barrier_intra_doublering(comm); */
}


/*
 *	bcast_intra_dec 
 *
 *	Function:	- seletects broadcast algorithm to use
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code (passed from the bcast implementation)
 */
int ompi_coll_tuned_bcast_intra_dec_fixed(void *buff, int count,
                                          struct ompi_datatype_t *datatype, int root,
                                          struct ompi_communicator_t *comm)
{
    /* Decision function based on MX results for 
    messages up to 36MB and communicator sizes up to 64 nodes */
    const size_t small_message_size = 2048;
    const size_t intermediate_message_size = 370728;
    const double a_p16  = 3.2118e-6; /* [1 / byte] */
    const double b_p16  = 8.7936;   
    const double a_p64  = 2.3679e-6; /* [1 / byte] */
    const double b_p64  = 1.1787;     
    const double a_p128 = 1.6134e-6; /* [1 / byte] */
    const double b_p128 = 2.1102;

    int communicator_size, rank;
    int segsize = 0;
    size_t message_size, dsize;

    communicator_size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* else we need data size for decision function */
    ompi_ddt_type_size(datatype, &dsize);
    message_size = dsize * (unsigned long)count;   /* needed for decision */

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_bcast_intra_dec_fixed root %d rank %d com_size %d msg_length %ld",
                 root, rank, communicator_size, message_size));

    /* Handle messages of small and intermediate size */
    if (message_size < small_message_size) {
       /* Binomial without segmentation */
       segsize = 0;
       return  ompi_coll_tuned_bcast_intra_binomial (buff, count, datatype, 
						     root, comm, segsize);

    } else if (message_size < intermediate_message_size) {
       /* SplittedBinary with 1KB segments */
       segsize = 1024;
       return ompi_coll_tuned_bcast_intra_split_bintree(buff, count, datatype, 
							root, comm, segsize);

    } 
    /* Handle large message sizes */
    else if (communicator_size < (a_p128 * message_size + b_p128)) {
       /* Pipeline with 128KB segments */
       segsize = 1024  << 7;
       return ompi_coll_tuned_bcast_intra_pipeline (buff, count, datatype, 
						    root, comm, segsize);

    } else if (communicator_size < 13) {
       /* Split Binary with 8KB segments */
       segsize = 1024 << 3;
       return ompi_coll_tuned_bcast_intra_split_bintree(buff, count, datatype, 
							root, comm, segsize);
       
    } else if (communicator_size < (a_p64 * message_size + b_p64)) {
       /* Pipeline with 64KB segments */
       segsize = 1024 << 6;
       return ompi_coll_tuned_bcast_intra_pipeline (buff, count, datatype, 
						    root, comm, segsize);

    } else if (communicator_size < (a_p16 * message_size + b_p16)) {
       /* Pipeline with 16KB segments */
       segsize = 1024 << 4;
       return ompi_coll_tuned_bcast_intra_pipeline (buff, count, datatype, 
						    root, comm, segsize);

    }

    /* Pipeline with 8KB segments */
    segsize = 1024 << 3;
    return ompi_coll_tuned_bcast_intra_pipeline (buff, count, datatype, 
						 root, comm, segsize);
#if 0
    /* this is based on gige measurements */

    if (communicator_size  < 4) {
        return ompi_coll_tuned_bcast_intra_basic_linear (buff, count, datatype, root, comm);
    }
    if (communicator_size == 4) {
        if (message_size < 524288) segsize = 0;
        else segsize = 16384;
        return ompi_coll_tuned_bcast_intra_bintree (buff, count, datatype, root, comm, segsize);
    }
    if (communicator_size <= 8 && message_size < 4096) {
        return ompi_coll_tuned_bcast_intra_basic_linear (buff, count, datatype, root, comm);
    }
    if (communicator_size > 8 && message_size >= 32768 && message_size < 524288) {
        segsize = 16384;
        return  ompi_coll_tuned_bcast_intra_bintree (buff, count, datatype, root, comm, segsize);
    }
    if (message_size >= 524288) {
        segsize = 16384;
        return ompi_coll_tuned_bcast_intra_pipeline (buff, count, datatype, root, comm, segsize);
    }
    segsize = 0;
    /* once tested can swap this back in */
    /* return ompi_coll_tuned_bcast_intra_bmtree (buff, count, datatype, root, comm, segsize); */
    return ompi_coll_tuned_bcast_intra_bintree (buff, count, datatype, root, comm, segsize);
#endif  /* 0 */
}

/*
 *	reduce_intra_dec 
 *
 *	Function:	- seletects reduce algorithm to use
 *	Accepts:	- same arguments as MPI_reduce()
 *	Returns:	- MPI_SUCCESS or error code (passed from the reduce implementation)
 *                                        
 */
int ompi_coll_tuned_reduce_intra_dec_fixed( void *sendbuf, void *recvbuf,
                                            int count, struct ompi_datatype_t* datatype,
                                            struct ompi_op_t* op, int root,
                                            struct ompi_communicator_t* comm)
{
    int communicator_size, rank, segsize = 0;
    size_t message_size, dsize;
    const double a1 =  0.6016 / 1024.0; /* [1/B] */
    const double b1 =  1.3496;
    const double a2 =  0.0410 / 1024.0; /* [1/B] */
    const double b2 =  9.7128;
    const double a3 =  0.0422 / 1024.0; /* [1/B] */
    const double b3 =  1.1614;
    const double a4 =  0.0033 / 1024.0; /* [1/B] */
    const double b4 =  1.6761;

    /**
     * If the operation is non commutative we only have one reduce algorithm right now.
     */
    if( !ompi_op_is_commute(op) ) {
        return ompi_coll_tuned_reduce_intra_basic_linear (sendbuf, recvbuf, count, datatype, op, root, comm); 
    }

    communicator_size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* need data size for decision function */
    ompi_ddt_type_size(datatype, &dsize);
    message_size = dsize * count;   /* needed for decision */

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_reduce_intra_dec_fixed"
                 "root %d rank %d com_size %d msg_length %ld",
                 root, rank, communicator_size, message_size));

    if (((communicator_size < 20) && (message_size < 512)) ||
        ((communicator_size < 10) && (message_size <= 1024))){
        /* Linear_0K */
        return ompi_coll_tuned_reduce_intra_basic_linear (sendbuf, recvbuf, count, datatype, op, root, comm); 
    } else if ((communicator_size < 8) && (message_size < 20480)) {
        /* Binomial_0K */
        segsize = 0;
        return ompi_coll_tuned_reduce_intra_binomial(sendbuf, recvbuf, count, datatype, op, root, comm, segsize);
    } else if (message_size < 2048) {
        /* Binary_0K */
        segsize = 0;
        return ompi_coll_tuned_reduce_intra_binary(sendbuf, recvbuf, count, datatype, op, root, comm, segsize);
    } else if (communicator_size > (a1 * message_size + b1)) {
        /* Binary_1K */
        segsize = 1024;
        return ompi_coll_tuned_reduce_intra_binary(sendbuf, recvbuf, count, datatype, op, root, comm, segsize);
    } else if (communicator_size > (a2 * message_size + b2)) {
        /* Pipeline_1K */
        segsize = 1024;
        return ompi_coll_tuned_reduce_intra_pipeline (sendbuf, recvbuf, count, datatype, op, root, comm, segsize);
    } else if (communicator_size > (a3 * message_size + b3)) {
        /* Binary_32K */
        segsize = 32*1024;
        return ompi_coll_tuned_reduce_intra_pipeline (sendbuf, recvbuf, count, datatype, op, root, comm, segsize);
    }
    if (communicator_size > (a4 * message_size + b4)) {
        /* Pipeline_32K */
        segsize = 32*1024;
    } else {
        /* Pipeline_64K */
        segsize = 64*1024;
    }
    return ompi_coll_tuned_reduce_intra_pipeline (sendbuf, recvbuf, count, datatype, op, root, comm, segsize);

#if 0
    /* for small messages use linear algorithm */
    if (message_size <= 4096) {
        segsize = 0;
        fanout = communicator_size - 1;
        /* when linear implemented or taken from basic put here, right now using chain as a linear system */
        /* it is implemented and I shouldn't be calling a chain with a fanout bigger than MAXTREEFANOUT from topo.h! */
        return ompi_coll_tuned_reduce_intra_basic_linear (sendbuf, recvbuf, count, datatype, op, root, comm); 
        /*        return ompi_coll_tuned_reduce_intra_chain (sendbuf, recvbuf, count, datatype, op, root, comm, segsize, fanout); */
    }
    if (message_size < 524288) {
        if (message_size <= 65536 ) {
            segsize = 32768;
            fanout = 8;
        } else {
            segsize = 1024;
            fanout = communicator_size/2;
        }
        /* later swap this for a binary tree */
        /*         fanout = 2; */
        return ompi_coll_tuned_reduce_intra_chain (sendbuf, recvbuf, count, datatype, op, root, comm, segsize, fanout);
    }
    segsize = 1024;
    return ompi_coll_tuned_reduce_intra_pipeline (sendbuf, recvbuf, count, datatype, op, root, comm, segsize);
#endif  /* 0 */
}

/*
 *	allgather_intra_dec 
 *
 *	Function:	- seletects allgather algorithm to use
 *	Accepts:	- same arguments as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code, passed from corresponding
 *                        internal allgather function.
 */

int ompi_coll_tuned_allgather_intra_dec_fixed(void *sbuf, int scount, 
                                              struct ompi_datatype_t *sdtype,
                                              void* rbuf, int rcount, 
                                              struct ompi_datatype_t *rdtype, 
                                              struct ompi_communicator_t *comm)
{
   int communicator_size, rank, pow2_size;
   size_t dsize, total_dsize;

   communicator_size = ompi_comm_size(comm);
   rank = ompi_comm_rank(comm);

   /* Special case for 2 processes */
   if (communicator_size == 2) {
      return ompi_coll_tuned_allgather_intra_two_procs (sbuf, scount, sdtype, 
                                                        rbuf, rcount, rdtype, 
                                                        comm);
    }

   /* Determine complete data size */
   ompi_ddt_type_size(sdtype, &dsize);
   total_dsize = dsize * scount * communicator_size;   
   
   OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_allgather_intra_dec_fixed rank %d com_size %d msg_length %ld", rank, communicator_size, total_dsize));

   for (pow2_size  = 1; pow2_size <= communicator_size; pow2_size <<=1); 
   pow2_size >>=1;

   /* Decision based on MX 2Gb results from Grig cluster at 
      The University of Tennesse, Knoxville 
      - if total message size is less than 50KB use either bruck or 
        recursive doubling for non-power of two and power of two nodes, 
        respectively.
      - else use ring and neighbor exchange algorithms for odd and even 
        number of nodes, respectively.
   */
   if (total_dsize < 50000) {
      if (pow2_size == communicator_size) {
         return ompi_coll_tuned_allgather_intra_recursivedoubling(sbuf, scount, 
                                                                  sdtype, 
                                                                  rbuf, rcount, 
                                                                  rdtype, comm);
      } else {
         return ompi_coll_tuned_allgather_intra_bruck(sbuf, scount, sdtype, 
                                                      rbuf, rcount, rdtype, 
                                                      comm);
      }
   } else {
      if (communicator_size % 2) {
         return ompi_coll_tuned_allgather_intra_ring(sbuf, scount, sdtype, 
                                                     rbuf, rcount, rdtype, 
                                                     comm);
      } else {
         return  ompi_coll_tuned_allgather_intra_neighborexchange(sbuf, scount, 
                                                                  sdtype,
                                                                  rbuf, rcount,
                                                                  rdtype, comm);
      }
   }
   
#if defined(USE_MPICH2_DECISION)
   /* Decision as in MPICH-2 
      presented in Thakur et.al. "Optimization of Collective Communication 
      Operations in MPICH", International Journal of High Performance Computing 
      Applications, Vol. 19, No. 1, 49-66 (2005)
      - for power-of-two processes and small and medium size messages 
        (up to 512KB) use recursive doubling
      - for non-power-of-two processes and small messages (80KB) use bruck,
      - for everything else use ring.
    */
   if ((pow2_size == communicator_size) && (total_dsize < 524288)) {
      return ompi_coll_tuned_allgather_intra_recursivedoubling(sbuf, scount, 
                                                               sdtype, 
                                                               rbuf, rcount, 
                                                               rdtype, 
                                                               comm);
   } else if (total_dsize <= 81920) { 
      return ompi_coll_tuned_allgather_intra_bruck(sbuf, scount, sdtype, 
                                                   rbuf, rcount, rdtype, comm);
   } 
   return ompi_coll_tuned_allgather_intra_ring(sbuf, scount, sdtype, 
                                               rbuf, rcount, rdtype, comm);
#endif  /* defined(USE_MPICH2_DECISION) */
}
