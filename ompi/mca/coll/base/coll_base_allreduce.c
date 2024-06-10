/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      University of Houston. All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All Rights
 *                         reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Siberian State University of Telecommunications
 *                         and Information Science. All rights reserved.
 * Copyright (c) 2022      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c)           Amazon.com, Inc. or its affiliates.
 *                         All rights reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "opal/util/bit_ops.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/op/op.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_base_topo.h"
#include "coll_base_util.h"

/*
 * ompi_coll_base_allreduce_intra_nonoverlapping
 *
 * This function just calls a reduce followed by a broadcast
 * both called functions are base but they complete sequentially,
 * i.e. no additional overlapping
 * meaning if the number of segments used is greater than the topo depth
 * then once the first segment of data is fully 'reduced' it is not broadcast
 * while the reduce continues (cost = cost-reduce + cost-bcast + decision x 3)
 *
 */
int
ompi_coll_base_allreduce_intra_nonoverlapping(const void *sbuf, void *rbuf, size_t count,
                                               struct ompi_datatype_t *dtype,
                                               struct ompi_op_t *op,
                                               struct ompi_communicator_t *comm,
                                               mca_coll_base_module_t *module)
{
    int err, rank;

    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:allreduce_intra_nonoverlapping rank %d", rank));

    /* Reduce to 0 and broadcast. */

    if (MPI_IN_PLACE == sbuf) {
        if (0 == rank) {
            err = comm->c_coll->coll_reduce (MPI_IN_PLACE, rbuf, count, dtype,
                                            op, 0, comm, comm->c_coll->coll_reduce_module);
        } else {
            err = comm->c_coll->coll_reduce (rbuf, NULL, count, dtype, op, 0,
                                            comm, comm->c_coll->coll_reduce_module);
        }
    } else {
        err = comm->c_coll->coll_reduce (sbuf, rbuf, count, dtype, op, 0,
                                        comm, comm->c_coll->coll_reduce_module);
    }
    if (MPI_SUCCESS != err) {
        return err;
    }

    return comm->c_coll->coll_bcast (rbuf, count, dtype, 0, comm,
                                    comm->c_coll->coll_bcast_module);
}

/*
 *   ompi_coll_base_allreduce_intra_recursivedoubling
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
ompi_coll_base_allreduce_intra_recursivedoubling(const void *sbuf, void *rbuf,
                                                  size_t count,
                                                  struct ompi_datatype_t *dtype,
                                                  struct ompi_op_t *op,
                                                  struct ompi_communicator_t *comm,
                                                  mca_coll_base_module_t *module)
{
    int ret, line, rank, size, adjsize, remote, distance;
    int newrank, newremote, extra_ranks;
    char *tmpsend = NULL, *tmprecv = NULL, *tmpswap = NULL, *inplacebuf_free = NULL, *inplacebuf;
    char *recvbuf = NULL;
    ptrdiff_t span, gap = 0;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:allreduce_intra_recursivedoubling rank %d", rank));

    /* Special case for size == 1 */
    if (1 == size) {
        if (MPI_IN_PLACE != sbuf) {
            ret = ompi_datatype_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
            if (ret < 0) { line = __LINE__; goto error_hndl; }
        }
        return MPI_SUCCESS;
    }

    /* get the device for sbuf and rbuf and where the op would like to execute */
    int sendbuf_dev, recvbuf_dev, op_dev;
    uint64_t sendbuf_flags, recvbuf_flags;
    ompi_coll_base_select_device(op, sbuf, rbuf, count, dtype, &sendbuf_dev, &recvbuf_dev,
                                 &sendbuf_flags, &recvbuf_flags, &op_dev);
    span = opal_datatype_span(&dtype->super, count, &gap);
    inplacebuf_free = ompi_coll_base_allocate_on_device(op_dev, span, module);
    if (NULL == inplacebuf_free) { ret = -1; line = __LINE__; goto error_hndl; }
    inplacebuf = inplacebuf_free - gap;
    //printf("allreduce ring count %d sbuf_dev %d rbuf_dev %d op_dev %d\n", count, sendbuf_dev, recvbuf_dev, op_dev);

    opal_accelerator_stream_t *stream = NULL;
    if (op_dev >= 0) {
        stream = MCA_ACCELERATOR_STREAM_DEFAULT;
    }

    tmpsend = (char*) sbuf;
    if (op_dev != recvbuf_dev) {
        /* copy data to where the op wants it to be */
        if (MPI_IN_PLACE == sbuf) {
            ret = ompi_datatype_copy_content_same_ddt_stream(dtype, count, inplacebuf, (char*)rbuf, stream);
            if (ret < 0) { line = __LINE__; goto error_hndl; }
        }
        /* only copy if op is on the device or we cannot access the sendbuf on the host */
        else if (op_dev != MCA_ACCELERATOR_NO_DEVICE_ID ||
                 0 == (sendbuf_flags & MCA_ACCELERATOR_FLAGS_UNIFIED_MEMORY)) {
            ret = ompi_datatype_copy_content_same_ddt_stream(dtype, count, inplacebuf, (char*)sbuf, stream);
            if (ret < 0) { line = __LINE__; goto error_hndl; }
        }
        tmpsend = (char*) inplacebuf;
    } else if (MPI_IN_PLACE == sbuf) {
        ret = ompi_datatype_copy_content_same_ddt_stream(dtype, count, inplacebuf, (char*)rbuf, stream);
        if (ret < 0) { line = __LINE__; goto error_hndl; }
        tmpsend = (char*) inplacebuf;
    }

    /* Handle MPI_IN_PLACE */
    bool use_sbuf = (MPI_IN_PLACE != sbuf);
    /* allocate temporary recv buffer if the tmpbuf above is on a different device than the rbuf
     * and the op is on the device or we cannot access the recv buffer on the host */
    recvbuf = rbuf;
    bool free_recvbuf = false;
    if (op_dev != recvbuf_dev &&
        (op_dev != MCA_ACCELERATOR_NO_DEVICE_ID ||
         0 == (recvbuf_flags & MCA_ACCELERATOR_FLAGS_UNIFIED_MEMORY))) {
        recvbuf = ompi_coll_base_allocate_on_device(op_dev, span, module);
        free_recvbuf = true;
        if (use_sbuf) {
            /* copy from rbuf */
            ompi_datatype_copy_content_same_ddt_stream(dtype, count, (char*)recvbuf, (char*)sbuf, stream);
        } else {
            /* copy from sbuf */
            ompi_datatype_copy_content_same_ddt_stream(dtype, count, (char*)recvbuf, (char*)rbuf, stream);
        }
        use_sbuf = false;
    }

    tmprecv = (char*) recvbuf;

    /* Determine nearest power of two less than or equal to size */
    adjsize = opal_next_poweroftwo (size);
    adjsize >>= 1;

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
            /* wait for above copies to complete */
            if (NULL != stream) {
                opal_accelerator.wait_stream(stream);
            }
            /* wait for tmpsend to be copied */
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
            if (tmpsend == sbuf) {
                tmpsend = inplacebuf;
                /* tmpsend = tmprecv (op) sbuf */
                ompi_3buff_op_reduce_stream(op, sbuf, tmprecv, tmpsend, count, dtype, op_dev, stream);
            } else {
                /* tmpsend = tmprecv (op) tmpsend */
                ompi_op_reduce_stream(op, tmprecv, tmpsend, count, dtype, op_dev, stream);
            }
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

        bool have_next_iter = ((distance << 1) < adjsize);

        /* wait for previous ops to complete to complete */
        if (NULL != stream) {
            opal_accelerator.wait_stream(stream);
        }
        /* Exchange the data */
        ret = ompi_coll_base_sendrecv_actual(tmpsend, count, dtype, remote,
                                             MCA_COLL_BASE_TAG_ALLREDUCE,
                                             tmprecv, count, dtype, remote,
                                             MCA_COLL_BASE_TAG_ALLREDUCE,
                                             comm, MPI_STATUS_IGNORE);
        if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

        /* Apply operation */
        if (rank < remote) {
            if (tmpsend == sbuf) {
                /* special case: 1st iteration takes one input from the sbuf */
                /* tmprecv = sbuf (op) tmprecv */
                ompi_op_reduce_stream(op, sbuf, tmprecv, count, dtype, op_dev, stream);
                /* send the current recv buffer, and use the tmp buffer to receive */
                tmpsend = tmprecv;
                tmprecv = inplacebuf;
            } else if (have_next_iter || tmprecv == recvbuf) {
                /* All iterations, and the last if tmprecv is the recv buffer */
                /* tmprecv = tmpsend (op) tmprecv */
                ompi_op_reduce_stream(op, tmpsend, tmprecv, count, dtype, op_dev, stream);
                /* swap send and receive buffers */
                tmpswap = tmprecv;
                tmprecv = tmpsend;
                tmpsend = tmpswap;
            } else {
                /* Last iteration if tmprecv is not the recv buffer, then tmpsend is */
                /* Make sure we reduce into the receive buffer
                 * tmpsend = tmprecv (op) tmpsend */
                ompi_op_reduce_stream(op, tmprecv, tmpsend, count, dtype, op_dev, stream);
            }
        } else {
            if (tmpsend == sbuf) {
                /* First iteration: use input from sbuf */
                /* tmpsend = tmprecv (op) sbuf */
                tmpsend = inplacebuf;
                if (have_next_iter || tmpsend == recvbuf) {
                    ompi_3buff_op_reduce_stream(op, tmprecv, sbuf, tmpsend, count, dtype, op_dev, stream);
                } else {
                    ompi_op_reduce_stream(op, sbuf, tmprecv, count, dtype, op_dev, stream);
                    tmpsend = tmprecv;
                }
            } else if (have_next_iter || tmpsend == recvbuf) {
                /* All other iterations: reduce into tmpsend for next iteration */
                /* tmpsend = tmprecv (op) tmpsend */
                ompi_op_reduce_stream(op, tmprecv, tmpsend, count, dtype, op_dev, stream);
            } else {
                /* Last iteration: reduce into rbuf and set tmpsend to rbuf (needed at the end) */
                ompi_op_reduce_stream(op, tmpsend, tmprecv, count, dtype, op_dev, stream);
                tmpsend = tmprecv;
            }
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
            /* wait for previous ops to complete */
            if (NULL != stream) {
                opal_accelerator.wait_stream(stream);
            }
            ret = MCA_PML_CALL(send(tmpsend, count, dtype, (rank - 1),
                                    MCA_COLL_BASE_TAG_ALLREDUCE,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
        }
    }

    /* Ensure that the final result is in rbuf */
    if (tmpsend != rbuf) {
        /* TODO: catch this case in the 3buf selection above. Maybe already caught? */
        ret = ompi_datatype_copy_content_same_ddt_stream(dtype, count, (char*)rbuf, tmpsend, stream);
        if (ret < 0) { line = __LINE__; goto error_hndl; }
    }

    /* wait for previous ops to complete */
    if (NULL != stream) {
        opal_accelerator.wait_stream(stream);
    }
    ompi_coll_base_free_tmpbuf(inplacebuf_free, op_dev, module);

    if (free_recvbuf) {
        ompi_coll_base_free_tmpbuf(recvbuf, op_dev, module);
    }
    return MPI_SUCCESS;

 error_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output, "%s:%4d\tRank %d Error occurred %d\n",
                 __FILE__, line, rank, ret));
    (void)line;  // silence compiler warning
    ompi_coll_base_free_tmpbuf(inplacebuf_free, op_dev, module);

    if (op_dev != recvbuf_dev) {
        ompi_coll_base_free_tmpbuf(recvbuf, op_dev, module);
    }
    return ret;
}

/*
 *   ompi_coll_base_allreduce_intra_ring
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
ompi_coll_base_allreduce_intra_ring(const void *sbuf, void *rbuf, size_t count,
                                     struct ompi_datatype_t *dtype,
                                     struct ompi_op_t *op,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module)
{
    int ret, line, rank, size, k, recv_from, send_to, block_count, inbi;
    int early_segcount, late_segcount, split_rank, max_segcount;
    size_t typelng;
    char *tmpsend = NULL, *tmprecv = NULL, *inbuf[2] = {NULL, NULL};
    void *recvbuf = NULL;
    ptrdiff_t true_lb, true_extent, lb, extent;
    ptrdiff_t block_offset, max_real_segsize;
    ompi_request_t *reqs[2] = {MPI_REQUEST_NULL, MPI_REQUEST_NULL};

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:allreduce_intra_ring rank %d, count %zu", rank, count));

    /* Special case for size == 1 */
    if (1 == size) {
        if (MPI_IN_PLACE != sbuf) {
            ret = ompi_datatype_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
            if (ret < 0) { line = __LINE__; goto error_hndl; }
        }
        return MPI_SUCCESS;
    }

    /* Special case for count less than size - use recursive doubling */
    if (count < (size_t) size) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output, "coll:base:allreduce_ring rank %d/%d, count %zu, switching to recursive doubling", rank, size, count));
        return (ompi_coll_base_allreduce_intra_recursivedoubling(sbuf, rbuf,
                                                                  count,
                                                                  dtype, op,
                                                                  comm, module));
    }

    /* Allocate and initialize temporary buffers */
    ret = ompi_datatype_get_extent(dtype, &lb, &extent);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
    ret = ompi_datatype_get_true_extent(dtype, &true_lb, &true_extent);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
    ret = ompi_datatype_type_size( dtype, &typelng);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

    /* Determine the number of elements per block and corresponding
       block sizes.
       The blocks are divided into "early" and "late" ones:
       blocks 0 .. (split_rank - 1) are "early" and
       blocks (split_rank) .. (size - 1) are "late".
       Early blocks are at most 1 element larger than the late ones.
    */
    COLL_BASE_COMPUTE_BLOCKCOUNT( count, size, split_rank,
                                   early_segcount, late_segcount );
    max_segcount = early_segcount;
    max_real_segsize = true_extent + (max_segcount - 1) * extent;

    /* get the device for sbuf and rbuf and where the op would like to execute */
    int sendbuf_dev, recvbuf_dev, op_dev;
    uint64_t sendbuf_flags, recvbuf_flags;
    ompi_coll_base_select_device(op, sbuf, rbuf, count, dtype, &sendbuf_dev, &recvbuf_dev,
                                 &sendbuf_flags, &recvbuf_flags, &op_dev);
    if (size > 2) {
        inbuf[0] = ompi_coll_base_allocate_on_device(op_dev, 2*max_real_segsize, module);
        if (NULL == inbuf[0]) { ret = -1; line = __LINE__; goto error_hndl; }
        inbuf[1] = inbuf[0] + max_real_segsize;
    } else {
        inbuf[0] = ompi_coll_base_allocate_on_device(op_dev, max_real_segsize, module);
        if (NULL == inbuf[0]) { ret = -1; line = __LINE__; goto error_hndl; }
    }

    /* Handle MPI_IN_PLACE */
    bool use_sbuf = (MPI_IN_PLACE != sbuf);
    /* allocate temporary recv buffer if the tmpbuf above is on a different device than the rbuf */
    recvbuf = rbuf;
    if (op_dev != recvbuf_dev &&
        /* only copy if op is on the device or the recvbuffer cannot be accessed on the host */
        (op_dev != MCA_ACCELERATOR_NO_DEVICE_ID || 0 == (MCA_ACCELERATOR_FLAGS_UNIFIED_MEMORY & recvbuf_flags))) {
        recvbuf = ompi_coll_base_allocate_on_device(op_dev, typelng*count, module);
        if (use_sbuf) {
            /* copy from rbuf */
            ompi_datatype_copy_content_same_ddt(dtype, count, (char*)recvbuf, (char*)sbuf);
        } else {
            /* copy from sbuf */
            ompi_datatype_copy_content_same_ddt(dtype, count, (char*)recvbuf, (char*)rbuf);
        }
        use_sbuf = false;
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
       Note that we must be careful when computing the beginning of buffers and
       for send operations and computation we must compute the exact block size.
    */
    send_to = (rank + 1) % size;
    recv_from = (rank + size - 1) % size;

    inbi = 0;
    /* Initialize first receive from the neighbor on the left */
    ret = MCA_PML_CALL(irecv(inbuf[inbi], max_segcount, dtype, recv_from,
                             MCA_COLL_BASE_TAG_ALLREDUCE, comm, &reqs[inbi]));
    if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
    /* Send first block (my block) to the neighbor on the right */
    block_offset = ((rank < split_rank)?
                    ((ptrdiff_t)rank * (ptrdiff_t)early_segcount) :
                    ((ptrdiff_t)rank * (ptrdiff_t)late_segcount + split_rank));
    block_count = ((rank < split_rank)? early_segcount : late_segcount);
    tmpsend = ((use_sbuf) ? ((char*)sbuf) : ((char*)recvbuf)) + block_offset * extent;
    ret = MCA_PML_CALL(send(tmpsend, block_count, dtype, send_to,
                            MCA_COLL_BASE_TAG_ALLREDUCE,
                            MCA_PML_BASE_SEND_STANDARD, comm));
    if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

    for (k = 2; k < size; k++) {
        const int prevblock = (rank + size - k + 1) % size;

        inbi = inbi ^ 0x1;

        /* Post irecv for the current block */
        ret = MCA_PML_CALL(irecv(inbuf[inbi], max_segcount, dtype, recv_from,
                                 MCA_COLL_BASE_TAG_ALLREDUCE, comm, &reqs[inbi]));
        if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

        /* Wait on previous block to arrive */
        ret = ompi_request_wait(&reqs[inbi ^ 0x1], MPI_STATUS_IGNORE);
        if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

        /* Apply operation on previous block: result goes to rbuf
           rbuf[prevblock] = inbuf[inbi ^ 0x1] (op) rbuf[prevblock]
        */
        block_offset = ((prevblock < split_rank)?
                        ((ptrdiff_t)prevblock * early_segcount) :
                        ((ptrdiff_t)prevblock * late_segcount + split_rank));
        block_count = ((prevblock < split_rank)? early_segcount : late_segcount);
        tmprecv = ((char*)recvbuf) + (ptrdiff_t)block_offset * extent;
        if (use_sbuf) {
            void *tmpsbuf = ((char*)sbuf) + (ptrdiff_t)block_offset * extent;
            /* tmprecv = inbuf[inbi ^ 0x1] (op) sbuf */
            ompi_3buff_op_reduce_stream(op, inbuf[inbi ^ 0x1], tmpsbuf, tmprecv, block_count,
                                        dtype, op_dev, NULL);
        } else {
            /* tmprecv = inbuf[inbi ^ 0x1] (op) tmprecv */
            ompi_op_reduce_stream(op, inbuf[inbi ^ 0x1], tmprecv, block_count,
                                  dtype, op_dev, NULL);
        }

        /* send previous block to send_to */
        ret = MCA_PML_CALL(send(tmprecv, block_count, dtype, send_to,
                                MCA_COLL_BASE_TAG_ALLREDUCE,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
    }

    /* Wait on the last block to arrive */
    ret = ompi_request_wait(&reqs[inbi], MPI_STATUS_IGNORE);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

    /* Apply operation on the last block (from neighbor (rank + 1)
       rbuf[rank+1] = inbuf[inbi] (op) rbuf[rank + 1] */
    recv_from = (rank + 1) % size;
    block_offset = ((recv_from < split_rank)?
                    ((ptrdiff_t)recv_from * early_segcount) :
                    ((ptrdiff_t)recv_from * late_segcount + split_rank));
    block_count = ((recv_from < split_rank)? early_segcount : late_segcount);
    tmprecv = ((char*)recvbuf) + (ptrdiff_t)block_offset * extent;
    ompi_op_reduce_stream(op, inbuf[inbi], tmprecv, block_count, dtype, op_dev, NULL);

    /* Distribution loop - variation of ring allgather */
    send_to = (rank + 1) % size;
    recv_from = (rank + size - 1) % size;
    for (k = 0; k < size - 1; k++) {
        const int recv_data_from = (rank + size - k) % size;
        const int send_data_from = (rank + 1 + size - k) % size;
        const int send_block_offset =
            ((send_data_from < split_rank)?
             ((ptrdiff_t)send_data_from * early_segcount) :
             ((ptrdiff_t)send_data_from * late_segcount + split_rank));
        const int recv_block_offset =
            ((recv_data_from < split_rank)?
             ((ptrdiff_t)recv_data_from * early_segcount) :
             ((ptrdiff_t)recv_data_from * late_segcount + split_rank));
        block_count = ((send_data_from < split_rank)?
                       early_segcount : late_segcount);

        tmprecv = (char*)recvbuf + (ptrdiff_t)recv_block_offset * extent;
        tmpsend = (char*)recvbuf + (ptrdiff_t)send_block_offset * extent;

        ret = ompi_coll_base_sendrecv(tmpsend, block_count, dtype, send_to,
                                       MCA_COLL_BASE_TAG_ALLREDUCE,
                                       tmprecv, max_segcount, dtype, recv_from,
                                       MCA_COLL_BASE_TAG_ALLREDUCE,
                                       comm, MPI_STATUS_IGNORE, rank);
        if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl;}
    }

    ompi_coll_base_free_tmpbuf(inbuf[0], op_dev, module);
    if (recvbuf != rbuf) {
        /* copy to final rbuf and release temporary recvbuf */
        ompi_datatype_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)recvbuf);
        ompi_coll_base_free_tmpbuf(recvbuf, op_dev, module);
    }

    return MPI_SUCCESS;

 error_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output, "%s:%4d\tRank %d Error occurred %d\n",
                 __FILE__, line, rank, ret));
    ompi_coll_base_free_reqs(reqs, 2);
    (void)line;  // silence compiler warning
    ompi_coll_base_free_tmpbuf(inbuf[0], op_dev, module);
    if (NULL != recvbuf && recvbuf != rbuf) {
        /* copy to final rbuf and release temporary recvbuf */
        ompi_datatype_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)recvbuf);
        ompi_coll_base_free_tmpbuf(recvbuf, op_dev, module);
    }
    return ret;
}

/*
 *   ompi_coll_base_allreduce_intra_ring_segmented
 *
 *   Function:       Pipelined ring algorithm for allreduce operation
 *   Accepts:        Same as MPI_Allreduce(), segment size
 *   Returns:        MPI_SUCCESS or error code
 *
 *   Description:    Implements pipelined ring algorithm for allreduce:
 *                   user supplies suggested segment size for the pipelining of
 *                   reduce operation.
 *                   The segment size determines the number of phases, np, for
 *                   the algorithm execution.
 *                   The message is automatically divided into blocks of
 *                   approximately  (count / (np * segcount)) elements.
 *                   At the end of reduction phase, allgather like step is
 *                   executed.
 *                   Algorithm requires (np + 1)*(N - 1) steps.
 *
 *   Limitations:    The algorithm DOES NOT preserve order of operations so it
 *                   can be used only for commutative operations.
 *                   In addition, algorithm cannot work if the total size is
 *                   less than size * segment size.
 *         Example on 3 nodes with 2 phases
 *         Initial state
 *   #      0              1             2
 *        [00a]          [10a]         [20a]
 *        [00b]          [10b]         [20b]
 *        [01a]          [11a]         [21a]
 *        [01b]          [11b]         [21b]
 *        [02a]          [12a]         [22a]
 *        [02b]          [12b]         [22b]
 *
 *        COMPUTATION PHASE 0 (a)
 *         Step 0: rank r sends block ra to rank (r+1) and receives bloc (r-1)a
 *                 from rank (r-1) [with wraparound].
 *    #     0              1             2
 *        [00a]        [00a+10a]       [20a]
 *        [00b]          [10b]         [20b]
 *        [01a]          [11a]       [11a+21a]
 *        [01b]          [11b]         [21b]
 *      [22a+02a]        [12a]         [22a]
 *        [02b]          [12b]         [22b]
 *
 *         Step 1: rank r sends block (r-1)a to rank (r+1) and receives bloc
 *                 (r-2)a from rank (r-1) [with wraparound].
 *    #     0              1             2
 *        [00a]        [00a+10a]   [00a+10a+20a]
 *        [00b]          [10b]         [20b]
 *    [11a+21a+01a]      [11a]       [11a+21a]
 *        [01b]          [11b]         [21b]
 *      [22a+02a]    [22a+02a+12a]     [22a]
 *        [02b]          [12b]         [22b]
 *
 *        COMPUTATION PHASE 1 (b)
 *         Step 0: rank r sends block rb to rank (r+1) and receives bloc (r-1)b
 *                 from rank (r-1) [with wraparound].
 *    #     0              1             2
 *        [00a]        [00a+10a]       [20a]
 *        [00b]        [00b+10b]       [20b]
 *        [01a]          [11a]       [11a+21a]
 *        [01b]          [11b]       [11b+21b]
 *      [22a+02a]        [12a]         [22a]
 *      [22b+02b]        [12b]         [22b]
 *
 *         Step 1: rank r sends block (r-1)b to rank (r+1) and receives bloc
 *                 (r-2)b from rank (r-1) [with wraparound].
 *    #     0              1             2
 *        [00a]        [00a+10a]   [00a+10a+20a]
 *        [00b]          [10b]     [0bb+10b+20b]
 *    [11a+21a+01a]      [11a]       [11a+21a]
 *    [11b+21b+01b]      [11b]         [21b]
 *      [22a+02a]    [22a+02a+12a]     [22a]
 *        [02b]      [22b+01b+12b]     [22b]
 *
 *
 *        DISTRIBUTION PHASE: ring ALLGATHER with ranks shifted by 1 (same as
 *         in regular ring algorithm.
 *
 */
int
ompi_coll_base_allreduce_intra_ring_segmented(const void *sbuf, void *rbuf, size_t count,
                                               struct ompi_datatype_t *dtype,
                                               struct ompi_op_t *op,
                                               struct ompi_communicator_t *comm,
                                               mca_coll_base_module_t *module,
                                               uint32_t segsize)
{
    int ret, line, rank, size, k, recv_from, send_to;
    int early_blockcount, late_blockcount, split_rank;
    int num_phases, phase, block_count, inbi;
    size_t typelng, segcount, max_segcount;
    char *tmpsend = NULL, *tmprecv = NULL, *inbuf[2] = {NULL, NULL};
    ptrdiff_t block_offset, max_real_segsize;
    ompi_request_t *reqs[2] = {MPI_REQUEST_NULL, MPI_REQUEST_NULL};
    ptrdiff_t lb, extent, gap;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:allreduce_intra_ring_segmented rank %d, count %zu", rank, count));

    /* Special case for size == 1 */
    if (1 == size) {
        if (MPI_IN_PLACE != sbuf) {
            ret = ompi_datatype_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
            if (ret < 0) { line = __LINE__; goto error_hndl; }
        }
        return MPI_SUCCESS;
    }

    /* Determine segment count based on the suggested segment size */
    ret = ompi_datatype_type_size( dtype, &typelng);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
    segcount = count;
    COLL_BASE_COMPUTED_SEGCOUNT(segsize, typelng, segcount)

        /* Special case for count less than size * segcount - use regular ring */
        if (count < (size_t) (size * segcount)) {
            OPAL_OUTPUT((ompi_coll_base_framework.framework_output, "coll:base:allreduce_ring_segmented rank %d/%d, count %zu, switching to regular ring", rank, size, count));
            return (ompi_coll_base_allreduce_intra_ring(sbuf, rbuf, count, dtype, op,
                                                         comm, module));
        }

    /* Determine the number of phases of the algorithm */
    num_phases = count / (size * segcount);
    if ((count % (size * segcount) >= (size_t) size) &&
        (count % (size * segcount) > (size_t) ((size * segcount) / 2))) {
        num_phases++;
    }

    /* Determine the number of elements per block and corresponding
       block sizes.
       The blocks are divided into "early" and "late" ones:
       blocks 0 .. (split_rank - 1) are "early" and
       blocks (split_rank) .. (size - 1) are "late".
       Early blocks are at most 1 element larger than the late ones.
       Note, these blocks will be split into num_phases segments,
       out of the largest one will have max_segcount elements.
    */
    COLL_BASE_COMPUTE_BLOCKCOUNT( count, size, split_rank,
                                   early_blockcount, late_blockcount );
    COLL_BASE_COMPUTE_BLOCKCOUNT( early_blockcount, num_phases, inbi,
                                   max_segcount, k);

    ret = ompi_datatype_get_extent(dtype, &lb, &extent);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
     max_real_segsize = opal_datatype_span(&dtype->super, max_segcount, &gap);

    int sendbuf_dev, recvbuf_dev, op_dev;
    uint64_t sendbuf_flags, recvbuf_flags;
    ompi_coll_base_select_device(op, sbuf, rbuf, count, dtype, &sendbuf_dev, &recvbuf_dev,
                                 &sendbuf_flags, &recvbuf_flags, &op_dev);
    /* Allocate and initialize temporary buffers */
    inbuf[0] = ompi_coll_base_allocate_on_device(op_dev, max_real_segsize, module);
    if (NULL == inbuf[0]) { ret = -1; line = __LINE__; goto error_hndl; }
    if (size > 2) {
        inbuf[1] = ompi_coll_base_allocate_on_device(op_dev, max_real_segsize, module);
        if (NULL == inbuf[1]) { ret = -1; line = __LINE__; goto error_hndl; }
    }

    /* Handle MPI_IN_PLACE */
    if (MPI_IN_PLACE != sbuf) {
        /* TODO: can we avoid this copy? */
        ret = ompi_datatype_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
        if (ret < 0) { line = __LINE__; goto error_hndl; }
    }

    /* Computation loop: for each phase, repeat ring allreduce computation loop */
    for (phase = 0; phase < num_phases; phase ++) {
        ptrdiff_t phase_offset;
        int early_phase_segcount, late_phase_segcount, split_phase, phase_count;

        /*
           For each of the remote nodes:
           - post irecv for block (r-1)
           - send block (r)
           To do this, first compute block offset and count, and use block offset
           to compute phase offset.
           - in loop for every step k = 2 .. n
           - post irecv for block (r + n - k) % n
           - wait on block (r + n - k + 1) % n to arrive
           - compute on block (r + n - k + 1) % n
           - send block (r + n - k + 1) % n
           - wait on block (r + 1)
           - compute on block (r + 1)
           - send block (r + 1) to rank (r + 1)
           Note that we must be careful when computing the beginning of buffers and
           for send operations and computation we must compute the exact block size.
        */
        send_to = (rank + 1) % size;
        recv_from = (rank + size - 1) % size;

        inbi = 0;
        /* Initialize first receive from the neighbor on the left */
        ret = MCA_PML_CALL(irecv(inbuf[inbi], max_segcount, dtype, recv_from,
                                 MCA_COLL_BASE_TAG_ALLREDUCE, comm, &reqs[inbi]));
        if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
        /* Send first block (my block) to the neighbor on the right:
           - compute my block and phase offset
           - send data */
        block_offset = ((rank < split_rank)?
                        ((ptrdiff_t)rank * (ptrdiff_t)early_blockcount) :
                        ((ptrdiff_t)rank * (ptrdiff_t)late_blockcount + split_rank));
        block_count = ((rank < split_rank)? early_blockcount : late_blockcount);
        COLL_BASE_COMPUTE_BLOCKCOUNT(block_count, num_phases, split_phase,
                                      early_phase_segcount, late_phase_segcount)
        phase_count = ((phase < split_phase)?
                       (early_phase_segcount) : (late_phase_segcount));
        phase_offset = ((phase < split_phase)?
                        ((ptrdiff_t)phase * (ptrdiff_t)early_phase_segcount) :
                        ((ptrdiff_t)phase * (ptrdiff_t)late_phase_segcount + split_phase));
        tmpsend = ((char*)rbuf) + (ptrdiff_t)(block_offset + phase_offset) * extent;
        ret = MCA_PML_CALL(send(tmpsend, phase_count, dtype, send_to,
                                MCA_COLL_BASE_TAG_ALLREDUCE,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

        for (k = 2; k < size; k++) {
            const int prevblock = (rank + size - k + 1) % size;

            inbi = inbi ^ 0x1;

            /* Post irecv for the current block */
            ret = MCA_PML_CALL(irecv(inbuf[inbi], max_segcount, dtype, recv_from,
                                     MCA_COLL_BASE_TAG_ALLREDUCE, comm,
                                     &reqs[inbi]));
            if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

            /* Wait on previous block to arrive */
            ret = ompi_request_wait(&reqs[inbi ^ 0x1], MPI_STATUS_IGNORE);
            if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

            /* Apply operation on previous block: result goes to rbuf
               rbuf[prevblock] = inbuf[inbi ^ 0x1] (op) rbuf[prevblock]
            */
            block_offset = ((prevblock < split_rank)?
                            ((ptrdiff_t)prevblock * (ptrdiff_t)early_blockcount) :
                            ((ptrdiff_t)prevblock * (ptrdiff_t)late_blockcount + split_rank));
            block_count = ((prevblock < split_rank)?
                           early_blockcount : late_blockcount);
            COLL_BASE_COMPUTE_BLOCKCOUNT(block_count, num_phases, split_phase,
                                          early_phase_segcount, late_phase_segcount)
                phase_count = ((phase < split_phase)?
                               (early_phase_segcount) : (late_phase_segcount));
            phase_offset = ((phase < split_phase)?
                            ((ptrdiff_t)phase * (ptrdiff_t)early_phase_segcount) :
                            ((ptrdiff_t)phase * (ptrdiff_t)late_phase_segcount + split_phase));
            tmprecv = ((char*)rbuf) + (ptrdiff_t)(block_offset + phase_offset) * extent;
            ompi_op_reduce_stream(op, inbuf[inbi ^ 0x1], tmprecv, phase_count,
                                  dtype, op_dev, NULL);

            /* send previous block to send_to */
            ret = MCA_PML_CALL(send(tmprecv, phase_count, dtype, send_to,
                                    MCA_COLL_BASE_TAG_ALLREDUCE,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }
        }

        /* Wait on the last block to arrive */
        ret = ompi_request_wait(&reqs[inbi], MPI_STATUS_IGNORE);
        if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl; }

        /* Apply operation on the last block (from neighbor (rank + 1)
           rbuf[rank+1] = inbuf[inbi] (op) rbuf[rank + 1] */
        recv_from = (rank + 1) % size;
        block_offset = ((recv_from < split_rank)?
                        ((ptrdiff_t)recv_from * (ptrdiff_t)early_blockcount) :
                        ((ptrdiff_t)recv_from * (ptrdiff_t)late_blockcount + split_rank));
        block_count = ((recv_from < split_rank)?
                       early_blockcount : late_blockcount);
        COLL_BASE_COMPUTE_BLOCKCOUNT(block_count, num_phases, split_phase,
                                      early_phase_segcount, late_phase_segcount)
            phase_count = ((phase < split_phase)?
                           (early_phase_segcount) : (late_phase_segcount));
        phase_offset = ((phase < split_phase)?
                        ((ptrdiff_t)phase * (ptrdiff_t)early_phase_segcount) :
                        ((ptrdiff_t)phase * (ptrdiff_t)late_phase_segcount + split_phase));
        tmprecv = ((char*)rbuf) + (ptrdiff_t)(block_offset + phase_offset) * extent;
        ompi_op_reduce_stream(op, inbuf[inbi], tmprecv, phase_count,
                              dtype, op_dev, NULL);
    }

    /* Distribution loop - variation of ring allgather */
    send_to = (rank + 1) % size;
    recv_from = (rank + size - 1) % size;
    for (k = 0; k < size - 1; k++) {
        const int recv_data_from = (rank + size - k) % size;
        const int send_data_from = (rank + 1 + size - k) % size;
        const int send_block_offset =
            ((send_data_from < split_rank)?
             ((ptrdiff_t)send_data_from * (ptrdiff_t)early_blockcount) :
             ((ptrdiff_t)send_data_from * (ptrdiff_t)late_blockcount + split_rank));
        const int recv_block_offset =
            ((recv_data_from < split_rank)?
             ((ptrdiff_t)recv_data_from * (ptrdiff_t)early_blockcount) :
             ((ptrdiff_t)recv_data_from * (ptrdiff_t)late_blockcount + split_rank));
        block_count = ((send_data_from < split_rank)?
                       early_blockcount : late_blockcount);

        tmprecv = (char*)rbuf + (ptrdiff_t)recv_block_offset * extent;
        tmpsend = (char*)rbuf + (ptrdiff_t)send_block_offset * extent;

        ret = ompi_coll_base_sendrecv(tmpsend, block_count, dtype, send_to,
                                       MCA_COLL_BASE_TAG_ALLREDUCE,
                                       tmprecv, early_blockcount, dtype, recv_from,
                                       MCA_COLL_BASE_TAG_ALLREDUCE,
                                       comm, MPI_STATUS_IGNORE, rank);
        if (MPI_SUCCESS != ret) { line = __LINE__; goto error_hndl;}

    }

    ompi_coll_base_free_tmpbuf(inbuf[0], op_dev, module);
    ompi_coll_base_free_tmpbuf(inbuf[1], op_dev, module);

    return MPI_SUCCESS;

 error_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output, "%s:%4d\tRank %d Error occurred %d\n",
                 __FILE__, line, rank, ret));
    ompi_coll_base_free_reqs(reqs, 2);
    (void)line;  // silence compiler warning
    ompi_coll_base_free_tmpbuf(inbuf[0], op_dev, module);
    ompi_coll_base_free_tmpbuf(inbuf[1], op_dev, module);
    return ret;
}

/*
 * Linear functions are copied from the BASIC coll module
 * they do not segment the message and are simple implementations
 * but for some small number of nodes and/or small data sizes they
 * are just as fast as base/tree based segmenting operations
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
ompi_coll_base_allreduce_intra_basic_linear(const void *sbuf, void *rbuf, size_t count,
                                             struct ompi_datatype_t *dtype,
                                             struct ompi_op_t *op,
                                             struct ompi_communicator_t *comm,
                                             mca_coll_base_module_t *module)
{
    int err, rank;

    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:allreduce_intra_basic_linear rank %d", rank));

    /* Reduce to 0 and broadcast. */

    if (MPI_IN_PLACE == sbuf) {
        if (0 == rank) {
            err = ompi_coll_base_reduce_intra_basic_linear (MPI_IN_PLACE, rbuf, count, dtype,
                                                             op, 0, comm, module);
        } else {
            err = ompi_coll_base_reduce_intra_basic_linear(rbuf, NULL, count, dtype,
                                                            op, 0, comm, module);
        }
    } else {
        err = ompi_coll_base_reduce_intra_basic_linear(sbuf, rbuf, count, dtype,
                                                        op, 0, comm, module);
    }
    if (MPI_SUCCESS != err) {
        return err;
    }

    return ompi_coll_base_bcast_intra_basic_linear(rbuf, count, dtype, 0, comm, module);
}

/*
 * ompi_coll_base_allreduce_intra_redscat_allgather
 *
 * Function:  Allreduce using Rabenseifner's algorithm.
 * Accepts:   Same arguments as MPI_Allreduce
 * Returns:   MPI_SUCCESS or error code
 *
 * Description: an implementation of Rabenseifner's allreduce algorithm [1, 2].
 *   [1] Rajeev Thakur, Rolf Rabenseifner and William Gropp.
 *       Optimization of Collective Communication Operations in MPICH //
 *       The Int. Journal of High Performance Computing Applications. Vol 19,
 *       Issue 1, pp. 49--66.
 *   [2] http://www.hlrs.de/mpi/myreduce.html.
 *
 * This algorithm is a combination of a reduce-scatter implemented with
 * recursive vector halving and recursive distance doubling, followed either
 * by an allgather implemented with recursive doubling [1].
 *
 * Step 1. If the number of processes is not a power of two, reduce it to
 * the nearest lower power of two (p' = 2^{\floor{\log_2 p}})
 * by removing r = p - p' extra processes as follows. In the first 2r processes
 * (ranks 0 to 2r - 1), all the even ranks send the second half of the input
 * vector to their right neighbor (rank + 1), and all the odd ranks send
 * the first half of the input vector to their left neighbor (rank - 1).
 * The even ranks compute the reduction on the first half of the vector and
 * the odd ranks compute the reduction on the second half. The odd ranks then
 * send the result to their left neighbors (the even ranks). As a result,
 * the even ranks among the first 2r processes now contain the reduction with
 * the input vector on their right neighbors (the odd ranks). These odd ranks
 * do not participate in the rest of the algorithm, which leaves behind
 * a power-of-two number of processes. The first r even-ranked processes and
 * the last p - 2r processes are now renumbered from 0 to p' - 1.
 *
 * Step 2. The remaining processes now perform a reduce-scatter by using
 * recursive vector halving and recursive distance doubling. The even-ranked
 * processes send the second half of their buffer to rank + 1 and the odd-ranked
 * processes send the first half of their buffer to rank - 1. All processes
 * then compute the reduction between the local buffer and the received buffer.
 * In the next log_2(p') - 1 steps, the buffers are recursively halved, and the
 * distance is doubled. At the end, each of the p' processes has 1 / p' of the
 * total reduction result.
 *
 * Step 3. An allgather is performed by using recursive vector doubling and
 * distance halving. All exchanges are executed in reverse order relative
 * to recursive doubling on previous step. If the number of processes is not
 * a power of two, the total result vector must be sent to the r processes
 * that were removed in the first step.
 *
 * Limitations:
 *   count >= 2^{\floor{\log_2 p}}
 *   commutative operations only
 *   intra-communicators only
 *
 * Memory requirements (per process):
 *   count * typesize + 4 * \log_2(p) * sizeof(int) = O(count)
 */
int ompi_coll_base_allreduce_intra_redscat_allgather(
    const void *sbuf, void *rbuf, size_t count, struct ompi_datatype_t *dtype,
    struct ompi_op_t *op, struct ompi_communicator_t *comm,
    mca_coll_base_module_t *module)
{
    int *rindex = NULL, *rcount = NULL, *sindex = NULL, *scount = NULL;

    int comm_size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:allreduce_intra_redscat_allgather: rank %d/%d",
                 rank, comm_size));

    /* Find nearest power-of-two less than or equal to comm_size */
    int nsteps = opal_hibit(comm_size, comm->c_cube_dim + 1);   /* ilog2(comm_size) */
    if (-1 == nsteps) {
        return MPI_ERR_ARG;
    }
    int nprocs_pof2 = 1 << nsteps;                              /* flp2(comm_size) */

    if (count < nprocs_pof2 || !ompi_op_is_commute(op)) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "coll:base:allreduce_intra_redscat_allgather: rank %d/%d "
                     "count %zu switching to basic linear allreduce",
                     rank, comm_size, count));
        return ompi_coll_base_allreduce_intra_basic_linear(sbuf, rbuf, count, dtype,
                                                           op, comm, module);
    }

    int err = MPI_SUCCESS;
    ptrdiff_t lb, extent, dsize, gap = 0;
    ompi_datatype_get_extent(dtype, &lb, &extent);
    dsize = opal_datatype_span(&dtype->super, count, &gap);

    /* get the device for sbuf and rbuf and where the op would like to execute */
    int sendbuf_dev, recvbuf_dev, op_dev;
    uint64_t sendbuf_flags, recvbuf_flags;
    ompi_coll_base_select_device(op, sbuf, rbuf, count, dtype, &sendbuf_dev, &recvbuf_dev,
                                 &sendbuf_flags, &recvbuf_flags, &op_dev);

    /* Temporary buffer for receiving messages */
    char *tmp_buf = NULL;
    char *tmp_buf_raw = ompi_coll_base_allocate_on_device(op_dev, dsize, module);
    if (NULL == tmp_buf_raw)
        return OMPI_ERR_OUT_OF_RESOURCE;
    tmp_buf = tmp_buf_raw - gap;

    char *recvbuf = rbuf;
    if (op_dev != recvbuf_dev && 0 == (MCA_ACCELERATOR_FLAGS_UNIFIED_MEMORY & recvbuf_flags)) {
        recvbuf = ompi_coll_base_allocate_on_device(op_dev, dsize, module);
    }
    if (op_dev != sendbuf_dev && 0 == (MCA_ACCELERATOR_FLAGS_UNIFIED_MEMORY & sendbuf_flags) && sbuf != MPI_IN_PLACE) {
        /* move the data into the recvbuf and set sbuf to MPI_IN_PLACE */
        ompi_datatype_copy_content_same_ddt(dtype, count, (char*)recvbuf, (char*)sbuf);
        sbuf = MPI_IN_PLACE;
    }

    /*
     * Step 1. Reduce the number of processes to the nearest lower power of two
     * p' = 2^{\floor{\log_2 p}} by removing r = p - p' processes.
     * 1. In the first 2r processes (ranks 0 to 2r - 1), all the even ranks send
     *    the second half of the input vector to their right neighbor (rank + 1)
     *    and all the odd ranks send the first half of the input vector to their
     *    left neighbor (rank - 1).
     * 2. All 2r processes compute the reduction on their half.
     * 3. The odd ranks then send the result to their left neighbors
     *    (the even ranks).
     *
     * The even ranks (0 to 2r - 1) now contain the reduction with the input
     * vector on their right neighbors (the odd ranks). The first r even
     * processes and the p - 2r last processes are renumbered from
     * 0 to 2^{\floor{\log_2 p}} - 1.
     */

    int vrank, step, wsize;
    int nprocs_rem = comm_size - nprocs_pof2;

    opal_accelerator_stream_t *stream = NULL;
    if (op_dev >= 0) {
        stream = MCA_ACCELERATOR_STREAM_DEFAULT;
    }

    if (rank < 2 * nprocs_rem) {
        int count_lhalf = count / 2;
        int count_rhalf = count - count_lhalf;
        const void *send_buf = sbuf;
        if (MPI_IN_PLACE == sbuf) {
            send_buf = recvbuf;
        }

        if (rank % 2 != 0) {
            /*
             * Odd process -- exchange with rank - 1
             * Send the left half of the input vector to the left neighbor,
             * Recv the right half of the input vector from the left neighbor
             */
            err = ompi_coll_base_sendrecv((void*)send_buf, count_lhalf, dtype, rank - 1,
                                          MCA_COLL_BASE_TAG_ALLREDUCE,
                                          (char *)tmp_buf + (ptrdiff_t)count_lhalf * extent,
                                          count_rhalf, dtype, rank - 1,
                                          MCA_COLL_BASE_TAG_ALLREDUCE, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            /* Reduce on the right half of the buffers (result in rbuf)
             * We're not using a stream here, the reduction will make sure that the result is available upon return */
            if (MPI_IN_PLACE != sbuf) {
                /* rbuf = sbuf (op) tmp_buf */
                ompi_3buff_op_reduce_stream(op,
                                            (char *)tmp_buf + (ptrdiff_t)count_lhalf * extent,
                                            (char *)sbuf + (ptrdiff_t)count_lhalf * extent,
                                            (char *)recvbuf + count_lhalf * extent,
                                            count_rhalf, dtype, op_dev, NULL);
            } else {
                /* rbuf = rbuf (op) tmp_buf */
                ompi_op_reduce_stream(op, (char *)tmp_buf + (ptrdiff_t)count_lhalf * extent,
                                      (char *)recvbuf + count_lhalf * extent, count_rhalf,
                                      dtype, op_dev, NULL);
            }

            /* Send the right half to the left neighbor */
            err = MCA_PML_CALL(send((char *)recvbuf + (ptrdiff_t)count_lhalf * extent,
                                    count_rhalf, dtype, rank - 1,
                                    MCA_COLL_BASE_TAG_ALLREDUCE,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            /* This process does not pariticipate in recursive doubling phase */
            vrank = -1;

        } else {
            /*
             * Even process -- exchange with rank + 1
             * Send the right half of the input vector to the right neighbor,
             * Recv the left half of the input vector from the right neighbor
             */
            err = ompi_coll_base_sendrecv((char *)send_buf + (ptrdiff_t)count_lhalf * extent,
                                          count_rhalf, dtype, rank + 1,
                                          MCA_COLL_BASE_TAG_ALLREDUCE,
                                          tmp_buf, count_lhalf, dtype, rank + 1,
                                          MCA_COLL_BASE_TAG_ALLREDUCE, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            /* Reduce on the right half of the buffers (result in rbuf) */
            if (MPI_IN_PLACE != sbuf) {
                /* rbuf = sbuf (op) tmp_buf */
                ompi_3buff_op_reduce_stream(op, sbuf, tmp_buf, recvbuf, count_lhalf, dtype, op_dev, stream);
            } else {
                /* rbuf = rbuf (op) tmp_buf */
                ompi_op_reduce_stream(op, tmp_buf, recvbuf, count_lhalf, dtype, op_dev, stream);
            }


            /* Recv the right half from the right neighbor */
            err = MCA_PML_CALL(recv((char *)recvbuf + (ptrdiff_t)count_lhalf * extent,
                                    count_rhalf, dtype, rank + 1,
                                    MCA_COLL_BASE_TAG_ALLREDUCE, comm,
                                    MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            /* wait for the op to complete */
            if (NULL != stream) {
                opal_accelerator.wait_stream(stream);
            }

            vrank = rank / 2;
        }
    } else { /* rank >= 2 * nprocs_rem */
        vrank = rank - nprocs_rem;
    }

    /* At this point the input data has been accumulated into the rbuf */

    /*
     * Step 2. Reduce-scatter implemented with recursive vector halving and
     * recursive distance doubling. We have p' = 2^{\floor{\log_2 p}}
     * power-of-two number of processes with new ranks (vrank) and result in rbuf.
     *
     * The even-ranked processes send the right half of their buffer to rank + 1
     * and the odd-ranked processes send the left half of their buffer to
     * rank - 1. All processes then compute the reduction between the local
     * buffer and the received buffer. In the next \log_2(p') - 1 steps, the
     * buffers are recursively halved, and the distance is doubled. At the end,
     * each of the p' processes has 1 / p' of the total reduction result.
     */
    rindex = malloc(sizeof(*rindex) * nsteps);
    sindex = malloc(sizeof(*sindex) * nsteps);
    rcount = malloc(sizeof(*rcount) * nsteps);
    scount = malloc(sizeof(*scount) * nsteps);
    if (NULL == rindex || NULL == sindex || NULL == rcount || NULL == scount) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup_and_return;
    }

    if (vrank != -1) {
        step = 0;
        wsize = count;
        sindex[0] = rindex[0] = 0;

        for (int mask = 1; mask < nprocs_pof2; mask <<= 1) {
            /*
             * On each iteration: rindex[step] = sindex[step] -- beginning of the
             * current window. Length of the current window is storded in wsize.
             */
            int vdest = vrank ^ mask;
            /* Translate vdest virtual rank to real rank */
            int dest = (vdest < nprocs_rem) ? vdest * 2 : vdest + nprocs_rem;

            if (rank < dest) {
                /*
                 * Recv into the left half of the current window, send the right
                 * half of the window to the peer (perform reduce on the left
                 * half of the current window)
                 */
                rcount[step] = wsize / 2;
                scount[step] = wsize - rcount[step];
                sindex[step] = rindex[step] + rcount[step];
            } else {
                /*
                 * Recv into the right half of the current window, send the left
                 * half of the window to the peer (perform reduce on the right
                 * half of the current window)
                 */
                scount[step] = wsize / 2;
                rcount[step] = wsize - scount[step];
                rindex[step] = sindex[step] + scount[step];
            }

            /* Send part of data from the rbuf, recv into the tmp_buf */
            err = ompi_coll_base_sendrecv((char *)recvbuf + (ptrdiff_t)sindex[step] * extent,
                                          scount[step], dtype, dest,
                                          MCA_COLL_BASE_TAG_ALLREDUCE,
                                          (char *)tmp_buf + (ptrdiff_t)rindex[step] * extent,
                                          rcount[step], dtype, dest,
                                          MCA_COLL_BASE_TAG_ALLREDUCE, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            /* Local reduce: rbuf[] = tmp_buf[] <op> rbuf[] */
            ompi_op_reduce_stream(op, (char *)tmp_buf + (ptrdiff_t)rindex[step] * extent,
                                  (char *)recvbuf + (ptrdiff_t)rindex[step] * extent,
                                  rcount[step], dtype, op_dev, NULL);

            /* Move the current window to the received message */
            if (step + 1 < nsteps) {
                rindex[step + 1] = rindex[step];
                sindex[step + 1] = rindex[step];
                wsize = rcount[step];
                step++;
            }
        }
        /*
         * Assertion: each process has 1 / p' of the total reduction result:
         * rcount[nsteps - 1] elements in the rbuf[rindex[nsteps - 1], ...].
         */

        /*
         * Step 3. Allgather by the recursive doubling algorithm.
         * Each process has 1 / p' of the total reduction result:
         * rcount[nsteps - 1] elements in the rbuf[rindex[nsteps - 1], ...].
         * All exchanges are executed in reverse order relative
         * to recursive doubling (previous step).
         */

        step = nsteps - 1;

        for (int mask = nprocs_pof2 >> 1; mask > 0; mask >>= 1) {
            int vdest = vrank ^ mask;
            /* Translate vdest virtual rank to real rank */
            int dest = (vdest < nprocs_rem) ? vdest * 2 : vdest + nprocs_rem;

            /*
             * Send rcount[step] elements from rbuf[rindex[step]...]
             * Recv scount[step] elements to rbuf[sindex[step]...]
             */
            err = ompi_coll_base_sendrecv((char *)recvbuf + (ptrdiff_t)rindex[step] * extent,
                                          rcount[step], dtype, dest,
                                          MCA_COLL_BASE_TAG_ALLREDUCE,
                                          (char *)recvbuf + (ptrdiff_t)sindex[step] * extent,
                                          scount[step], dtype, dest,
                                          MCA_COLL_BASE_TAG_ALLREDUCE, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }
            step--;
        }
    }

    /*
     * Step 4. Send total result to excluded odd ranks.
     */
    bool recvbuf_need_copy = true;
    if (rank < 2 * nprocs_rem) {
        if (rank % 2 != 0) {
            /* Odd process -- recv result from rank - 1 */
            err = MCA_PML_CALL(recv(rbuf, count, dtype, rank - 1,
                                    MCA_COLL_BASE_TAG_ALLREDUCE, comm,
                                    MPI_STATUS_IGNORE));
            if (OMPI_SUCCESS != err) { goto cleanup_and_return; }
            recvbuf_need_copy = false;

        } else {
            /* Even process -- send result to rank + 1 */
            err = MCA_PML_CALL(send(recvbuf, count, dtype, rank + 1,
                                    MCA_COLL_BASE_TAG_ALLREDUCE,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }
        }
    }

    if (recvbuf != rbuf) {
        /* copy into final rbuf */
        if (recvbuf_need_copy) {
            ompi_datatype_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)recvbuf);
        }
        ompi_coll_base_free_tmpbuf(recvbuf, op_dev, module);
    }

  cleanup_and_return:

    ompi_coll_base_free_tmpbuf(tmp_buf_raw, op_dev, module);
    if (NULL != rindex)
        free(rindex);
    if (NULL != sindex)
        free(sindex);
    if (NULL != rcount)
        free(rcount);
    if (NULL != scount)
        free(scount);
    return err;
}

/*
 *   ompi_coll_base_allreduce_intra_allgather_reduce
 *
 *   Function:       use allgather for allreduce operation
 *   Accepts:        Same as MPI_Allreduce()
 *   Returns:        MPI_SUCCESS or error code
 *
 *   Description:    Implements allgather based allreduce aimed to improve internode 
 *                   allreduce latency: this method takes advantage of the send and 
 *                   receive can happen at the same time; first step is allgather
 *                   operation to allow all ranks to obtain the full dataset; the second
 *                   step is to do reduction on all ranks to get allreduce result. 
 *
 *   Limitations:    This method is designed for small message sizes allreduce because it 
 *                   is not efficient in terms of network bandwidth comparing
 *                   to gather/reduce/bcast type of approach.
 */
int ompi_coll_base_allreduce_intra_allgather_reduce(const void *sbuf, void *rbuf, size_t count,
                                                    struct ompi_datatype_t *dtype,
                                                    struct ompi_op_t *op,
                                                    struct ompi_communicator_t *comm,
                                                    mca_coll_base_module_t *module)
{
    int line = -1;
    char *partial_buf = NULL;
    char *partial_buf_start = NULL;
    char *sendtmpbuf = NULL;
    char *tmpsend = NULL;
    char *tmpsend_start = NULL;
    int err = OMPI_SUCCESS;

    ptrdiff_t extent, lb;
    ompi_datatype_get_extent(dtype, &lb, &extent);

    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);

    sendtmpbuf = (char*) sbuf;
    if( sbuf == MPI_IN_PLACE ) {
        sendtmpbuf = (char *)rbuf;
    }
    ptrdiff_t buf_size, gap = 0;
    buf_size = opal_datatype_span(&dtype->super, (int64_t)count * size, &gap);
    partial_buf = (char *) malloc(buf_size);
    partial_buf_start = partial_buf - gap;
    buf_size = opal_datatype_span(&dtype->super, (int64_t)count, &gap);
    tmpsend = (char *) malloc(buf_size);
    tmpsend_start = tmpsend - gap;

    err = ompi_datatype_copy_content_same_ddt(dtype, count,
                                              (char*)tmpsend_start,
                                              (char*)sendtmpbuf);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    // apply allgather data so that each rank has a full copy to do reduce (trade bandwidth for better latency)
    err = comm->c_coll->coll_allgather(tmpsend_start, count, dtype,
                                       partial_buf_start, count, dtype,
                                       comm, comm->c_coll->coll_allgather_module);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    for (int target = 1; target < size; target++) {
        ompi_op_reduce(op,
                       partial_buf_start + (ptrdiff_t)target * count * extent,
                       partial_buf_start,
                       count,
                       dtype);
    }

    // move data to rbuf
    err = ompi_datatype_copy_content_same_ddt(dtype, count,
                                              (char*)rbuf,
                                              (char*)partial_buf_start);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    if (NULL != partial_buf) free(partial_buf);
    if (NULL != tmpsend) free(tmpsend);
    return MPI_SUCCESS;

err_hndl:
    if (NULL != partial_buf) {
        free(partial_buf);
        partial_buf = NULL;
        partial_buf_start = NULL;
    }
     if (NULL != tmpsend) {
        free(tmpsend);
        tmpsend = NULL;
        tmpsend_start = NULL;
    }
   OPAL_OUTPUT((ompi_coll_base_framework.framework_output,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void)line;  // silence compiler warning
    return err;

}
/* copied function (with appropriate renaming) ends here */
