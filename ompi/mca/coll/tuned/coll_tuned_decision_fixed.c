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
    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_allreduce_intra_dec_fixed"));

    return (ompi_coll_tuned_allreduce_intra_nonoverlapping (sbuf, rbuf, count, dtype, op, comm));
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
    size_t dsize, total_dsize;

    communicator_size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* special case */
    if (communicator_size==2) {
        return ompi_coll_tuned_alltoall_intra_two_procs (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    }

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
    const double a0 = -7.8710;
    const double b0 = 41.1613;
    const double a1 = 0.0150;
    const double b1 = 11.2445;
    const double a2 = 0.0023;
    const double b2 = 3.8074;
    int communicator_size, rank;
    int segsize = 0;
    size_t message_size, dsize;

    communicator_size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* else we need data size for decision function */
    ompi_ddt_type_size(datatype, &dsize);
    message_size = dsize * (unsigned long)count;   /* needed for decision */

    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_bcast_intra_dec_fixed "
                 "root %d rank %d com_size %d msg_length %ld",
                 root, rank, communicator_size, message_size));

    if ((message_size <= 1024) && (communicator_size < 12)) {
        /* Linear_0K */
        return ompi_coll_tuned_bcast_intra_basic_linear (buff, count, datatype, root, comm);
    } else if (message_size < 8192) {
        if ((communicator_size < 12) || 
            (communicator_size < (a0 * (message_size / 1024.0) + b0))) {
            /* Binary_0K */
            segsize = 0;
        } else {
            /* Binary_1K */
            segsize = 1024;
        }
        return  ompi_coll_tuned_bcast_intra_bintree (buff, count, datatype, root, comm, segsize);
    } else if (message_size <= 35000) {
        if (communicator_size <= 12) {
            /* Binary_8K */
            segsize = 1024 << 3;
            return  ompi_coll_tuned_bcast_intra_bintree (buff, count, datatype, root, comm, segsize);
        } else {
            /* SplittedBinary_1K */
            segsize = 1024;
            return ompi_coll_tuned_bcast_intra_split_bintree(buff, count, datatype, root, comm, segsize);
        }

    } else if (communicator_size > (a1 * (message_size / 1024.0) + b1)) {
        /* SplittedBinary_8K */
        segsize = 1024 << 3;
        return ompi_coll_tuned_bcast_intra_split_bintree(buff, count, datatype, root, comm, segsize);
    }
    if (communicator_size > (a2 * (message_size / 1024.0) + b2)) {
        /* Pipeline_8K */
        segsize = 1024 << 3;
    } else {
        /* Pipeline_64K */
        segsize = 1024 << 6;
    }
    return ompi_coll_tuned_bcast_intra_pipeline (buff, count, datatype, root, comm, segsize);
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
