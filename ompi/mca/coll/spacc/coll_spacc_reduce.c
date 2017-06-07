/*
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_spacc.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "opal/util/bit_ops.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_util.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/op/op.h"

/*
 * mca_coll_spacc_reduce_intra_redscat_gather
 *
 * Function:  Reduce using Rabenseifner's algorithm.
 * Accepts:   Same arguments as MPI_Reduce
 * Returns:   MPI_SUCCESS or error code
 *
 * Description: an implementation of Rabenseifner's reduce algorithm [1, 2].
 *   [1] Rajeev Thakur, Rolf Rabenseifner and William Gropp.
 *       Optimization of Collective Communication Operations in MPICH //
 *       The Int. Journal of High Performance Computing Applications. Vol 19,
 *       Issue 1, pp. 49--66.
 *   [2] http://www.hlrs.de/mpi/myreduce.html.
 *
 * This algorithm is a combination of a reduce-scatter implemented with
 * recursive vector halving and recursive distance doubling, followed either
 * by a binomial tree gather [1].
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
 * Step 3. A binomial tree gather is performed by using recursive vector
 * doubling and distance halving. In the non-power-of-two case, if the root
 * happens to be one of those odd-ranked processes that would normally
 * be removed in the first step, then the role of this process and process 0
 * are interchanged.
 * 
 * Limitations:
 *   count >= 2^{\floor{\log_2 p}}
 *   commutative operations only
 *   intra-communicators only
 *
 * Memory requirements (per process):
 *   rank != root: 2 * count * typesize + 4 * log_2(p) * sizeof(int) = O(count)
 *   rank == root: count * typesize + 4 * log_2(p) * sizeof(int) = O(count)
 *
 * Recommendations: root = 0, otherwise it is required additional steps
 *                  in the root process.
 */
int mca_coll_spacc_reduce_intra_redscat_gather(
    const void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
    struct ompi_op_t *op, int root, struct ompi_communicator_t *comm,
    mca_coll_base_module_t *module)
{
    int comm_size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);

    opal_output_verbose(30, mca_coll_spacc_stream,
                        "coll:spacc:reduce_intra_redscat_gather: rank %d/%d, root %d",
                        rank, comm_size, root);

    /* Find nearest power-of-two less than or equal to comm_size */
    int nsteps = opal_hibit(comm_size, comm->c_cube_dim + 1);   /* ilog2(comm_size) */
    assert(nsteps >= 0);
    int nprocs_pof2 = 1 << nsteps;                              /* flp2(comm_size) */

    if (count < nprocs_pof2 || !ompi_op_is_commute(op)) {
        opal_output_verbose(20, mca_coll_spacc_stream,
                            "coll:spacc:reduce_intra_redscat_gather: rank %d/%d count %d switching to base reduce",
                            rank, comm_size, count);
        return ompi_coll_base_reduce_intra_basic_linear(sbuf, rbuf, count, dtype,
                                                        op, root, comm, module);
    }

    int err = MPI_SUCCESS;
    int *rindex = NULL, *rcount = NULL, *sindex = NULL, *scount = NULL;

    ptrdiff_t lb, extent, dsize, gap;
    ompi_datatype_get_extent(dtype, &lb, &extent);
    dsize = opal_datatype_span(&dtype->super, count, &gap);

    /* Temporary buffer for receiving messages */
    char *tmp_buf = NULL;
    char *tmp_buf_raw = (char *)malloc(dsize);
    if (NULL == tmp_buf_raw)
        return OMPI_ERR_OUT_OF_RESOURCE;
    tmp_buf = tmp_buf_raw - gap;

    char *rbuf_raw = NULL;
    if (rank != root) {
        rbuf_raw = (char *)malloc(dsize);
        if (NULL == rbuf_raw) {
            err = OMPI_ERR_OUT_OF_RESOURCE;
            goto cleanup_and_return;
        }
        rbuf = rbuf_raw - gap;
    }

    if ((rank != root) || (sbuf != MPI_IN_PLACE)) {
        /* Copy sbuf to rbuf */
        err = ompi_datatype_copy_content_same_ddt(dtype, count, (char *)rbuf,
                                                  (char *)sbuf);
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
     * 0 to 2^{\floor{\log_2 p}} - 1. These odd ranks do not participate in the
     * rest of the algorithm.
     */

    int vrank, step, wsize;
    int nprocs_rem = comm_size - nprocs_pof2;

    if (rank < 2 * nprocs_rem) {
        int count_lhalf = count / 2;
        int count_rhalf = count - count_lhalf;

        if (rank % 2 != 0) {
            /*
             * Odd process -- exchange with rank - 1
             * Send the left half of the input vector to the left neighbor,
             * Recv the right half of the input vector from the left neighbor
             */
            err = ompi_coll_base_sendrecv(rbuf, count_lhalf, dtype, rank - 1,
                                          MCA_COLL_BASE_TAG_REDUCE,
                                          (char *)tmp_buf + (ptrdiff_t)count_lhalf * extent,
                                          count_rhalf, dtype, rank - 1,
                                          MCA_COLL_BASE_TAG_REDUCE, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            /* Reduce on the right half of the buffers (result in rbuf) */
            ompi_op_reduce(op, (char *)tmp_buf + (ptrdiff_t)count_lhalf * extent,
                           (char *)rbuf + count_lhalf * extent, count_rhalf, dtype);

            /* Send the right half to the left neighbor */
            err = MCA_PML_CALL(send((char *)rbuf + (ptrdiff_t)count_lhalf * extent,
                                    count_rhalf, dtype, rank - 1,
                                    MCA_COLL_BASE_TAG_REDUCE,
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
            err = ompi_coll_base_sendrecv((char *)rbuf + (ptrdiff_t)count_lhalf * extent,
                                          count_rhalf, dtype, rank + 1,
                                          MCA_COLL_BASE_TAG_REDUCE,
                                          tmp_buf, count_lhalf, dtype, rank + 1,
                                          MCA_COLL_BASE_TAG_REDUCE, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            /* Reduce on the right half of the buffers (result in rbuf) */
            ompi_op_reduce(op, tmp_buf, rbuf, count_lhalf, dtype);

            /* Recv the right half from the right neighbor */
            err = MCA_PML_CALL(recv((char *)rbuf + (ptrdiff_t)count_lhalf * extent,
                                    count_rhalf, dtype, rank + 1,
                                    MCA_COLL_BASE_TAG_REDUCE, comm,
                                    MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            vrank = rank / 2;
        }
    } else { /* rank >= 2 * nprocs_rem */
        vrank = rank - nprocs_rem;
    }

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

    rindex = malloc(sizeof(*rindex) * nsteps);    /* O(\log_2(p)) */
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
             * On each iteration: rindex[step] = sindex[step] -- begining of the
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
            err = ompi_coll_base_sendrecv((char *)rbuf + (ptrdiff_t)sindex[step] * extent,
                                          scount[step], dtype, dest,
                                          MCA_COLL_BASE_TAG_REDUCE,
                                          (char *)tmp_buf + (ptrdiff_t)rindex[step] * extent,
                                          rcount[step], dtype, dest,
                                          MCA_COLL_BASE_TAG_REDUCE, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            /* Local reduce: rbuf[] = tmp_buf[] <op> rbuf[] */
            ompi_op_reduce(op, (char *)tmp_buf + (ptrdiff_t)rindex[step] * extent,
                           (char *)rbuf + (ptrdiff_t)rindex[step] * extent,
                           rcount[step], dtype);

            /* Move the current window to the received message */
            if (step + 1 < nsteps) {
                rindex[step + 1] = rindex[step];
                sindex[step + 1] = rindex[step];
                wsize = rcount[step];
                step++;
            }
        }
    }
    /*
     * Assertion: each process has 1 / p' of the total reduction result:
     * rcount[nsteps - 1] elements in the rbuf[rindex[nsteps - 1], ...].
     */

    /*
     * Setup the root process for gather operation.
     * Case 1: root < 2r and root is odd -- root process was excluded on step 1
     *         Recv data from process 0, vroot = 0, vrank = 0
     * Case 2: root < 2r and root is even: vroot = root / 2
     * Case 3: root >= 2r: vroot = root - r
     */
    int vroot = 0;
    if (root < 2 * nprocs_rem) {
        if (root % 2 != 0) {
            vroot = 0;
            if (rank == root) {
                /*
                 * Case 1: root < 2r and root is odd -- root process was
                 * excluded on step 1 (newrank == -1).
                 * Recv a data from the process 0.
                 */
                rindex[0] = 0;
                step = 0, wsize = count;
                for (int mask = 1; mask < nprocs_pof2; mask *= 2) {
                    rcount[step] = wsize / 2;
                    scount[step] = wsize - rcount[step];
                    rindex[step] = 0;
                    sindex[step] = rcount[step];
                    step++;
                    wsize /= 2;
                }

                err = MCA_PML_CALL(recv(rbuf, rcount[nsteps - 1], dtype, 0,
                                        MCA_COLL_BASE_TAG_REDUCE, comm,
                                        MPI_STATUS_IGNORE));
                if (MPI_SUCCESS != err) { goto cleanup_and_return; }
                vrank = 0;

            } else if (vrank == 0) {
                /* Send a data to the root */
                err = MCA_PML_CALL(send(rbuf, rcount[nsteps - 1], dtype, root,
                                        MCA_COLL_BASE_TAG_REDUCE,
                                        MCA_PML_BASE_SEND_STANDARD, comm));
                if (MPI_SUCCESS != err) { goto cleanup_and_return; }
                vrank = -1;
            }
        } else {
            /* Case 2: root < 2r and a root is even: vroot = root / 2 */
            vroot = root / 2;
        }
    } else {
        /* Case 3: root >= 2r: newroot = root - r */
        vroot = root - nprocs_rem;
    }

    /*
     * Step 3. Gather result at the vroot by the binomial tree algorithm.
     * Each process has 1 / p' of the total reduction result:
     * rcount[nsteps - 1] elements in the rbuf[rindex[nsteps - 1], ...].
     * All exchanges are executed in reverse order relative
     * to recursive doubling (previous step).
     */

    if (vrank != -1) {
        int vdest_tree, vroot_tree;
        step = nsteps - 1; /* step = ilog2(p') - 1 */

        for (int mask = nprocs_pof2 >> 1; mask > 0; mask >>= 1) {
            int vdest = vrank ^ mask;
            /* Translate vdest virtual rank to real rank */
            int dest = (vdest < nprocs_rem) ? vdest * 2 : vdest + nprocs_rem;
            if ((vdest == 0) && (root < 2 * nprocs_rem) && (root % 2 != 0))
                dest = root;

            vdest_tree = vdest >> step;
            vdest_tree <<= step;
            vroot_tree = vroot >> step;
            vroot_tree <<= step;
            if (vdest_tree == vroot_tree) {
                /* Send data from rbuf and exit */
                err = MCA_PML_CALL(send((char *)rbuf + (ptrdiff_t)rindex[step] * extent,
                                        rcount[step], dtype, dest,
                                        MCA_COLL_BASE_TAG_REDUCE,
                                        MCA_PML_BASE_SEND_STANDARD, comm));
                if (MPI_SUCCESS != err) { goto cleanup_and_return; }
                break;
            } else {
                /* Recv and continue */
                err = MCA_PML_CALL(recv((char *)rbuf + (ptrdiff_t)sindex[step] * extent,
                                        scount[step], dtype, dest,
                                        MCA_COLL_BASE_TAG_REDUCE, comm,
                                        MPI_STATUS_IGNORE));
                if (MPI_SUCCESS != err) { goto cleanup_and_return; }
            }
            step--;
        }
    }

  cleanup_and_return:
    if (NULL != tmp_buf_raw)
        free(tmp_buf_raw);
    if (NULL != rbuf_raw)
        free(rbuf_raw);
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
