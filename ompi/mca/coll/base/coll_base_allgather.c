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
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2024      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "math.h"

#include "mpi.h"
#include "opal/util/bit_ops.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_base_topo.h"
#include "coll_base_util.h"

/*
 * ompi_coll_base_allgather_intra_recursivedoubling
 *
 * Function:     allgather using O(log(N)) steps.
 * Accepts:      Same arguments as MPI_Allgather
 * Returns:      MPI_SUCCESS or error code
 *
 * Description:  Recursive doubling algorithm for MPI_Allgather implementation.
 *               This algorithm is used in MPICH-2 for small- and medium-sized
 *               messages on power-of-two processes.
 *
 * Limitation:   Current implementation only works on power-of-two number of
 *               processes.
 *               In case this algorithm is invoked on non-power-of-two
 *               processes, Bruck algorithm will be invoked.
 *
 * Memory requirements:
 *               No additional memory requirements beyond user-supplied buffers.
 *
 * Example on 4 nodes:
 *   Initialization: everyone has its own buffer at location rank in rbuf
 *    #     0      1      2      3
 *         [0]    [ ]    [ ]    [ ]
 *         [ ]    [1]    [ ]    [ ]
 *         [ ]    [ ]    [2]    [ ]
 *         [ ]    [ ]    [ ]    [3]
 *   Step 0: exchange data with (rank ^ 2^0)
 *    #     0      1      2      3
 *         [0]    [0]    [ ]    [ ]
 *         [1]    [1]    [ ]    [ ]
 *         [ ]    [ ]    [2]    [2]
 *         [ ]    [ ]    [3]    [3]
 *   Step 1: exchange data with (rank ^ 2^1) (if you can)
 *    #     0      1      2      3
 *         [0]    [0]    [0]    [0]
 *         [1]    [1]    [1]    [1]
 *         [2]    [2]    [2]    [2]
 *         [3]    [3]    [3]    [3]
 *
 *  TODO: Modify the algorithm to work with any number of nodes.
 *        We can modify code to use identical implementation like MPICH-2:
 *        - using recursive-halving algorithm, at the end of each step,
 *          determine if there are nodes who did not exchange their data in that
 *          step, and send them appropriate messages.
 */
int
ompi_coll_base_allgather_intra_recursivedoubling(const void *sbuf, size_t scount,
                                                  struct ompi_datatype_t *sdtype,
                                                  void* rbuf, size_t rcount,
                                                  struct ompi_datatype_t *rdtype,
                                                  struct ompi_communicator_t *comm,
                                                  mca_coll_base_module_t *module)
{
    int line = -1, rank, size, pow2size, err;
    int remote, distance, sendblocklocation;
    ptrdiff_t rlb, rext;
    char *tmpsend = NULL, *tmprecv = NULL;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    pow2size = opal_next_poweroftwo (size);
    pow2size >>=1;

    /* Current implementation only handles power-of-two number of processes.
       If the function was called on non-power-of-two number of processes,
       print warning and call bruck allgather algorithm with same parameters.
    */
    if (pow2size != size) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "coll:base:allgather_intra_recursivedoubling WARNING: non-pow-2 size %d, switching to bruck algorithm",
                     size));

        // fall back to the bruck method with radix 2
        int k = 2;
        return ompi_coll_base_allgather_intra_k_bruck(sbuf, scount, sdtype,
                                                      rbuf, rcount, rdtype,
                                                      comm, module, k);
    }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:allgather_intra_recursivedoubling rank %d, size %d",
                 rank, size));

    err = ompi_datatype_get_extent (rdtype, &rlb, &rext);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    /* Initialization step:
       - if send buffer is not MPI_IN_PLACE, copy send buffer to block 0 of
       receive buffer
    */
    if (MPI_IN_PLACE != sbuf) {
        tmpsend = (char*) sbuf;
        tmprecv = (char*) rbuf + (ptrdiff_t)rank * (ptrdiff_t)rcount * rext;
        err = ompi_datatype_sndrcv(tmpsend, scount, sdtype, tmprecv, rcount, rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }

    }

    /* Communication step:
       At every step i, rank r:
       - exchanges message with rank remote = (r ^ 2^i).

    */
    sendblocklocation = rank;
    for (distance = 0x1; distance < size; distance<<=1) {
        remote = rank ^ distance;

        if (rank < remote) {
            tmpsend = (char*)rbuf + (ptrdiff_t)sendblocklocation * (ptrdiff_t)rcount * rext;
            tmprecv = (char*)rbuf + (ptrdiff_t)(sendblocklocation + distance) * (ptrdiff_t)rcount * rext;
        } else {
            tmpsend = (char*)rbuf + (ptrdiff_t)sendblocklocation * (ptrdiff_t)rcount * rext;
            tmprecv = (char*)rbuf + (ptrdiff_t)(sendblocklocation - distance) * (ptrdiff_t)rcount * rext;
            sendblocklocation -= distance;
        }

        /* Sendreceive */
        err = ompi_coll_base_sendrecv(tmpsend, (ptrdiff_t)distance * (ptrdiff_t)rcount, rdtype,
                                       remote, MCA_COLL_BASE_TAG_ALLGATHER,
                                       tmprecv, (ptrdiff_t)distance * (ptrdiff_t)rcount, rdtype,
                                       remote, MCA_COLL_BASE_TAG_ALLGATHER,
                                       comm, MPI_STATUS_IGNORE, rank);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    }

    return OMPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void)line;  // silence compiler warning
    return err;
}

/*
 * ompi_coll_base_allgather_intra_sparbit
 *
 * Function:     allgather using O(log(N)) steps.
 * Accepts:      Same arguments as MPI_Allgather
 * Returns:      MPI_SUCCESS or error code
 *
 * Description: Proposal of an allgather algorithm similar to Bruck but with inverted distances
 *              and non-decreasing exchanged data sizes. Described in "Sparbit: a new
 *              logarithmic-cost and data locality-aware MPI Allgather algorithm".
 *
 * Memory requirements:  
 *              Additional memory for N requests. 
 *
 * Example on 6 nodes, with l representing the highest power of two smaller than N, in this case l =
 * 4 (more details can be found on the paper):
 *  Initial state
 *    #     0      1      2      3      4      5
 *         [0]    [ ]    [ ]    [ ]    [ ]    [ ]
 *         [ ]    [1]    [ ]    [ ]    [ ]    [ ]
 *         [ ]    [ ]    [2]    [ ]    [ ]    [ ]
 *         [ ]    [ ]    [ ]    [3]    [ ]    [ ]
 *         [ ]    [ ]    [ ]    [ ]    [4]    [ ]
 *         [ ]    [ ]    [ ]    [ ]    [ ]    [5]
 *   Step 0: Each process sends its own block to process r + l and receives another from r - l.
 *    #     0      1      2      3      4      5
 *         [0]    [ ]    [ ]    [ ]    [0]    [ ]
 *         [ ]    [1]    [ ]    [ ]    [ ]    [1]
 *         [2]    [ ]    [2]    [ ]    [ ]    [ ]
 *         [ ]    [3]    [ ]    [3]    [ ]    [ ]
 *         [ ]    [ ]    [4]    [ ]    [4]    [ ]
 *         [ ]    [ ]    [ ]    [5]    [ ]    [5]
 *   Step 1: Each process sends its own block to process r + l/2 and receives another from r - l/2.
 *   The block received on the previous step is ignored to avoid a future double-write.  
 *    #     0      1      2      3      4      5
 *         [0]    [ ]    [0]    [ ]    [0]    [ ]
 *         [ ]    [1]    [ ]    [1]    [ ]    [1]
 *         [2]    [ ]    [2]    [ ]    [2]    [ ]
 *         [ ]    [3]    [ ]    [3]    [ ]    [3]
 *         [4]    [ ]    [4]    [ ]    [4]    [ ]
 *         [ ]    [5]    [ ]    [5]    [ ]    [5]
 *   Step 1: Each process sends all the data it has (3 blocks) to process r + l/4 and similarly
 *   receives all the data from process r - l/4. 
 *    #     0      1      2      3      4      5
 *         [0]    [0]    [0]    [0]    [0]    [0]
 *         [1]    [1]    [1]    [1]    [1]    [1]
 *         [2]    [2]    [2]    [2]    [2]    [2]
 *         [3]    [3]    [3]    [3]    [3]    [3]
 *         [4]    [4]    [4]    [4]    [4]    [4]
 *         [5]    [5]    [5]    [5]    [5]    [5]
 */

int ompi_coll_base_allgather_intra_sparbit(const void *sbuf, size_t scount,
                                                  struct ompi_datatype_t *sdtype,
                                                  void* rbuf, size_t rcount,
                                                  struct ompi_datatype_t *rdtype,
                                                  struct ompi_communicator_t *comm,
                                                  mca_coll_base_module_t *module)
{
    /* ################# VARIABLE DECLARATION, BUFFER CREATION AND PREPARATION FOR THE ALGORITHM ######################## */

    /* list of variable declaration */
    int rank = 0, comm_size = 0, comm_log = 0, exclusion = 0, data_expected = 1, transfer_count = 0;
    int sendto, recvfrom, send_disp, recv_disp;
    uint32_t last_ignore, ignore_steps, distance = 1;

    int err = 0;
    int line = -1;

    ptrdiff_t rlb, rext;

    char *tmpsend = NULL, *tmprecv = NULL;

    MPI_Request *requests = NULL;

    /* algorithm choice information printing */
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output, 
                 "coll:base:allgather_intra_sparbit rank %d", rank));

    comm_size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    err = ompi_datatype_get_extent(rdtype, &rlb, &rext);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    /* if the MPI_IN_PLACE condition is not set, copy the send buffer to the receive buffer to perform the sends (all the data is extracted and forwarded from the recv buffer)*/
    /* tmprecv and tmpsend are used as abstract pointers to simplify send and receive buffer choice */
    tmprecv = (char *) rbuf;
    if(MPI_IN_PLACE != sbuf){
        tmpsend = (char *) sbuf; 
        err = ompi_datatype_sndrcv(tmpsend, scount, sdtype, tmprecv + (ptrdiff_t) rank * rcount * rext, rcount, rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }
    }
    tmpsend = tmprecv;

    requests = (MPI_Request *) malloc(comm_size * sizeof(MPI_Request));
    
    /* ################# ALGORITHM LOGIC ######################## */

    /* calculate log2 of the total process count */
    comm_log = ceil(log(comm_size)/log(2));
    distance <<= comm_log - 1;

    last_ignore = __builtin_ctz(comm_size);
    ignore_steps = (~((uint32_t) comm_size >> last_ignore) | 1) << last_ignore;

    /* perform the parallel binomial tree distribution steps */
    for (int i = 0; i < comm_log; ++i) {
       sendto = (rank + distance) % comm_size;  
       recvfrom = (rank - distance + comm_size) % comm_size;  
       exclusion = (distance & ignore_steps) == distance;

       for (transfer_count = 0; transfer_count < data_expected - exclusion; transfer_count++) {
           send_disp = (rank - 2 * transfer_count * distance + comm_size) % comm_size;
           recv_disp = (rank - (2 * transfer_count + 1) * distance + comm_size) % comm_size;

           /* Since each process sends several non-contiguos blocks of data, each block sent (and therefore each send and recv call) needs a different tag. */
           /* As base OpenMPI only provides one tag for allgather, we are forced to use a tag space from other components in the send and recv calls */
           MCA_PML_CALL(isend(tmpsend + (ptrdiff_t) send_disp * scount * rext, scount, rdtype, sendto, MCA_COLL_BASE_TAG_HCOLL_BASE - send_disp, MCA_PML_BASE_SEND_STANDARD, comm, requests + transfer_count));
           MCA_PML_CALL(irecv(tmprecv + (ptrdiff_t) recv_disp * rcount * rext, rcount, rdtype, recvfrom, MCA_COLL_BASE_TAG_HCOLL_BASE - recv_disp, comm, requests + data_expected - exclusion + transfer_count));
       }
       ompi_request_wait_all(transfer_count * 2, requests, MPI_STATUSES_IGNORE);

       distance >>= 1; 
       /* calculates the data expected for the next step, based on the current number of blocks and eventual exclusions */
       data_expected = (data_expected << 1) - exclusion;
       exclusion = 0;
    }
    
    free(requests);

    return OMPI_SUCCESS;

err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void)line;  // silence compiler warning
    return err;
}

/*
 * ompi_coll_base_allgather_intra_ring
 *
 * Function:     allgather using O(N) steps.
 * Accepts:      Same arguments as MPI_Allgather
 * Returns:      MPI_SUCCESS or error code
 *
 * Description:  Ring algorithm for all gather.
 *               At every step i, rank r receives message from rank (r - 1)
 *               containing data from rank (r - i - 1) and sends message to rank
 *               (r + 1) containing data from rank (r - i), with wrap arounds.
 * Memory requirements:
 *               No additional memory requirements.
 *
 */
int ompi_coll_base_allgather_intra_ring(const void *sbuf, size_t scount,
                                         struct ompi_datatype_t *sdtype,
                                         void* rbuf, size_t rcount,
                                         struct ompi_datatype_t *rdtype,
                                         struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module)
{
    int line = -1, rank, size, err, sendto, recvfrom, i, recvdatafrom, senddatafrom;
    ptrdiff_t rlb, rext;
    char *tmpsend = NULL, *tmprecv = NULL;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:allgather_intra_ring rank %d", rank));

    err = ompi_datatype_get_extent (rdtype, &rlb, &rext);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    /* Initialization step:
       - if send buffer is not MPI_IN_PLACE, copy send buffer to appropriate block
       of receive buffer
    */
    tmprecv = (char*) rbuf + (ptrdiff_t)rank * (ptrdiff_t)rcount * rext;
    if (MPI_IN_PLACE != sbuf) {
        tmpsend = (char*) sbuf;
        err = ompi_datatype_sndrcv(tmpsend, scount, sdtype, tmprecv, rcount, rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }
    }

    /* Communication step:
       At every step i: 0 .. (P-1), rank r:
       - receives message from [(r - 1 + size) % size] containing data from rank
       [(r - i - 1 + size) % size]
       - sends message to rank [(r + 1) % size] containing data from rank
       [(r - i + size) % size]
       - sends message which starts at beginning of rbuf and has size
    */
    sendto = (rank + 1) % size;
    recvfrom  = (rank - 1 + size) % size;

    for (i = 0; i < size - 1; i++) {
        recvdatafrom = (rank - i - 1 + size) % size;
        senddatafrom = (rank - i + size) % size;

        tmprecv = (char*)rbuf + (ptrdiff_t)recvdatafrom * (ptrdiff_t)rcount * rext;
        tmpsend = (char*)rbuf + (ptrdiff_t)senddatafrom * (ptrdiff_t)rcount * rext;

        /* Sendreceive */
        err = ompi_coll_base_sendrecv(tmpsend, rcount, rdtype, sendto,
                                       MCA_COLL_BASE_TAG_ALLGATHER,
                                       tmprecv, rcount, rdtype, recvfrom,
                                       MCA_COLL_BASE_TAG_ALLGATHER,
                                       comm, MPI_STATUS_IGNORE, rank);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    }

    return OMPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void)line;  // silence compiler warning
    return err;
}

/*
 * ompi_coll_base_allgather_intra_neighborexchange
 *
 * Function:     allgather using N/2 steps (O(N))
 * Accepts:      Same arguments as MPI_Allgather
 * Returns:      MPI_SUCCESS or error code
 *
 * Description:  Neighbor Exchange algorithm for allgather.
 *               Described by Chen et.al. in
 *               "Performance Evaluation of Allgather Algorithms on
 *                Terascale Linux Cluster with Fast Ethernet",
 *               Proceedings of the Eighth International Conference on
 *               High-Performance Computing inn Asia-Pacific Region
 *               (HPCASIA'05), 2005
 *
 *               Rank r exchanges message with one of its neighbors and
 *               forwards the data further in the next step.
 *
 *               No additional memory requirements.
 *
 * Limitations:  Algorithm works only on even number of processes.
 *               For odd number of processes we switch to ring algorithm.
 *
 * Example on 6 nodes:
 *  Initial state
 *    #     0      1      2      3      4      5
 *         [0]    [ ]    [ ]    [ ]    [ ]    [ ]
 *         [ ]    [1]    [ ]    [ ]    [ ]    [ ]
 *         [ ]    [ ]    [2]    [ ]    [ ]    [ ]
 *         [ ]    [ ]    [ ]    [3]    [ ]    [ ]
 *         [ ]    [ ]    [ ]    [ ]    [4]    [ ]
 *         [ ]    [ ]    [ ]    [ ]    [ ]    [5]
 *   Step 0:
 *    #     0      1      2      3      4      5
 *         [0]    [0]    [ ]    [ ]    [ ]    [ ]
 *         [1]    [1]    [ ]    [ ]    [ ]    [ ]
 *         [ ]    [ ]    [2]    [2]    [ ]    [ ]
 *         [ ]    [ ]    [3]    [3]    [ ]    [ ]
 *         [ ]    [ ]    [ ]    [ ]    [4]    [4]
 *         [ ]    [ ]    [ ]    [ ]    [5]    [5]
 *   Step 1:
 *    #     0      1      2      3      4      5
 *         [0]    [0]    [0]    [ ]    [ ]    [0]
 *         [1]    [1]    [1]    [ ]    [ ]    [1]
 *         [ ]    [2]    [2]    [2]    [2]    [ ]
 *         [ ]    [3]    [3]    [3]    [3]    [ ]
 *         [4]    [ ]    [ ]    [4]    [4]    [4]
 *         [5]    [ ]    [ ]    [5]    [5]    [5]
 *   Step 2:
 *    #     0      1      2      3      4      5
 *         [0]    [0]    [0]    [0]    [0]    [0]
 *         [1]    [1]    [1]    [1]    [1]    [1]
 *         [2]    [2]    [2]    [2]    [2]    [2]
 *         [3]    [3]    [3]    [3]    [3]    [3]
 *         [4]    [4]    [4]    [4]    [4]    [4]
 *         [5]    [5]    [5]    [5]    [5]    [5]
 */
int
ompi_coll_base_allgather_intra_neighborexchange(const void *sbuf, size_t scount,
                                                 struct ompi_datatype_t *sdtype,
                                                 void* rbuf, size_t rcount,
                                                 struct ompi_datatype_t *rdtype,
                                                 struct ompi_communicator_t *comm,
                                                 mca_coll_base_module_t *module)
{
    int line = -1, rank, size, i, even_rank, err;
    int neighbor[2], offset_at_step[2], recv_data_from[2], send_data_from;
    ptrdiff_t rlb, rext;
    char *tmpsend = NULL, *tmprecv = NULL;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    if (size % 2) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "coll:base:allgather_intra_neighborexchange WARNING: odd size %d, switching to ring algorithm",
                     size));
        return ompi_coll_base_allgather_intra_ring(sbuf, scount, sdtype,
                                                    rbuf, rcount, rdtype,
                                                    comm, module);
    }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:allgather_intra_neighborexchange rank %d", rank));

    err = ompi_datatype_get_extent (rdtype, &rlb, &rext);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    /* Initialization step:
       - if send buffer is not MPI_IN_PLACE, copy send buffer to appropriate block
       of receive buffer
    */
    tmprecv = (char*) rbuf + (ptrdiff_t)rank *(ptrdiff_t) rcount * rext;
    if (MPI_IN_PLACE != sbuf) {
        tmpsend = (char*) sbuf;
        err = ompi_datatype_sndrcv(tmpsend, scount, sdtype, tmprecv, rcount, rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }
    }

    /* Determine neighbors, order in which blocks will arrive, etc. */
    even_rank = !(rank % 2);
    if (even_rank) {
        neighbor[0] = (rank + 1) % size;
        neighbor[1] = (rank - 1 + size) % size;
        recv_data_from[0] = rank;
        recv_data_from[1] = rank;
        offset_at_step[0] = (+2);
        offset_at_step[1] = (-2);
    } else {
        neighbor[0] = (rank - 1 + size) % size;
        neighbor[1] = (rank + 1) % size;
        recv_data_from[0] = neighbor[0];
        recv_data_from[1] = neighbor[0];
        offset_at_step[0] = (-2);
        offset_at_step[1] = (+2);
    }

    /* Communication loop:
       - First step is special: exchange a single block with neighbor[0].
       - Rest of the steps:
       update recv_data_from according to offset, and
       exchange two blocks with appropriate neighbor.
       the send location becomes previous receive location.
    */
    tmprecv = (char*)rbuf + (ptrdiff_t)neighbor[0] * (ptrdiff_t)rcount * rext;
    tmpsend = (char*)rbuf + (ptrdiff_t)rank * (ptrdiff_t)rcount * rext;
    /* Sendreceive */
    err = ompi_coll_base_sendrecv(tmpsend, rcount, rdtype, neighbor[0],
                                   MCA_COLL_BASE_TAG_ALLGATHER,
                                   tmprecv, rcount, rdtype, neighbor[0],
                                   MCA_COLL_BASE_TAG_ALLGATHER,
                                   comm, MPI_STATUS_IGNORE, rank);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    /* Determine initial sending location */
    if (even_rank) {
        send_data_from = rank;
    } else {
        send_data_from = recv_data_from[0];
    }

    for (i = 1; i < (size / 2); i++) {
        const int i_parity = i % 2;
        recv_data_from[i_parity] =
            (recv_data_from[i_parity] + offset_at_step[i_parity] + size) % size;

        tmprecv = (char*)rbuf + (ptrdiff_t)recv_data_from[i_parity] * (ptrdiff_t)rcount * rext;
        tmpsend = (char*)rbuf + (ptrdiff_t)send_data_from * rcount * rext;

        /* Sendreceive */
        err = ompi_coll_base_sendrecv(tmpsend, (ptrdiff_t)2 * (ptrdiff_t)rcount, rdtype,
                                       neighbor[i_parity],
                                       MCA_COLL_BASE_TAG_ALLGATHER,
                                       tmprecv, (ptrdiff_t)2 * (ptrdiff_t)rcount, rdtype,
                                       neighbor[i_parity],
                                       MCA_COLL_BASE_TAG_ALLGATHER,
                                       comm, MPI_STATUS_IGNORE, rank);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

        send_data_from = recv_data_from[i_parity];
    }

    return OMPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void)line;  // silence compiler warning
    return err;
}


int ompi_coll_base_allgather_intra_two_procs(const void *sbuf, size_t scount,
                                              struct ompi_datatype_t *sdtype,
                                              void* rbuf, size_t rcount,
                                              struct ompi_datatype_t *rdtype,
                                              struct ompi_communicator_t *comm,
                                              mca_coll_base_module_t *module)
{
    int line = -1, err, rank, remote;
    char *tmpsend = NULL, *tmprecv = NULL;
    ptrdiff_t rext, lb;

    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_allgather_intra_two_procs rank %d", rank));

    if (2 != ompi_comm_size(comm)) {
        return MPI_ERR_UNSUPPORTED_OPERATION;
    }

    err = ompi_datatype_get_extent (rdtype, &lb, &rext);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    /* Exchange data:
       - compute source and destinations
       - send receive data
    */
    remote  = rank ^ 0x1;

    tmpsend = (char*)sbuf;
    if (MPI_IN_PLACE == sbuf) {
        tmpsend = (char*)rbuf + (ptrdiff_t)rank * (ptrdiff_t)rcount * rext;
        scount = rcount;
        sdtype = rdtype;
    }
    tmprecv = (char*)rbuf + (ptrdiff_t)remote * (ptrdiff_t)rcount * rext;

    err = ompi_coll_base_sendrecv(tmpsend, scount, sdtype, remote,
                                   MCA_COLL_BASE_TAG_ALLGATHER,
                                   tmprecv, rcount, rdtype, remote,
                                   MCA_COLL_BASE_TAG_ALLGATHER,
                                   comm, MPI_STATUS_IGNORE, rank);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    /* Place your data in correct location if necessary */
    if (MPI_IN_PLACE != sbuf) {
        err = ompi_datatype_sndrcv((char*)sbuf, scount, sdtype,
                                   (char*)rbuf + (ptrdiff_t)rank * (ptrdiff_t)rcount * rext, rcount, rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }
    }

    return MPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output, "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void)line;  // silence compiler warning
    return err;
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
 * JPG following the examples from other coll_base implementations. Dec06.
 */

/* copied function (with appropriate renaming) starts here */

/*
 *    allgather_intra_basic_linear
 *
 *    Function:    - allgather using other MPI collections
 *    Accepts:    - same as MPI_Allgather()
 *    Returns:    - MPI_SUCCESS or error code
 */
int
ompi_coll_base_allgather_intra_basic_linear(const void *sbuf, size_t scount,
                                             struct ompi_datatype_t *sdtype,
                                             void *rbuf,
                                             size_t rcount,
                                             struct ompi_datatype_t *rdtype,
                                             struct ompi_communicator_t *comm,
                                             mca_coll_base_module_t *module)
{
    int err;
    ptrdiff_t lb, extent;

    /* Handle MPI_IN_PLACE (see explanantion in reduce.c for how to
       allocate temp buffer) -- note that rank 0 can use IN_PLACE
       natively, and we can just alias the right position in rbuf
       as sbuf and avoid using a temporary buffer if gather is
       implemented correctly */
    if (MPI_IN_PLACE == sbuf && 0 != ompi_comm_rank(comm)) {
        ompi_datatype_get_extent(rdtype, &lb, &extent);
        sbuf = ((char*) rbuf) + (ompi_comm_rank(comm) * extent * rcount);
        sdtype = rdtype;
        scount = rcount;
    }

    /* Gather and broadcast. */

    err = comm->c_coll->coll_gather(sbuf, scount, sdtype,
                                   rbuf, rcount, rdtype,
                                   0, comm, comm->c_coll->coll_gather_module);
    if (MPI_SUCCESS == err) {
        size_t length = (ptrdiff_t)rcount * ompi_comm_size(comm);
        if( length < (size_t)INT_MAX ) {
            err = comm->c_coll->coll_bcast(rbuf, (ptrdiff_t)rcount * ompi_comm_size(comm), rdtype,
                                          0, comm, comm->c_coll->coll_bcast_module);
        } else {
            ompi_datatype_t* temptype;
            ompi_datatype_create_contiguous(ompi_comm_size(comm), rdtype, &temptype);
            ompi_datatype_commit(&temptype);
            err = comm->c_coll->coll_bcast(rbuf, rcount, temptype,
                                          0, comm, comm->c_coll->coll_bcast_module);
            ompi_datatype_destroy(&temptype);
        }
    }

    /* All done */

    return err;
}

/*
 * ompi_coll_base_allgather_intra_k_bruck
 *
 * Function:     allgather using O(logk(N)) steps.
 * Accepts:      Same arguments as MPI_Allgather
 * Returns:      MPI_SUCCESS or error code
 *
 * Description:  This method extend ompi_coll_base_allgather_intra_bruck to handle any
 *               radix k; use non-blocking communication to take advantage of multiple ports
 *               The algorithm detail is described in Bruck et al. (1997),
 *               "Efficient Algorithms for All-to-all Communications
 *                in Multiport Message-Passing Systems"
 * Memory requirements:  The data placed in rbuf after communication phase has a shift due to
 *                       data wrap around. This buffer with a size of rcount * (size - rank)
 *                       is used to shift the received data to the proper order
 *
 * Example on 10 nodes with k=3:
 *   Initialization: everyone has its own buffer at location 0 in rbuf
 *                   This means if user specified MPI_IN_PLACE for sendbuf
 *                   we must copy our block from recvbuf to beginning!
 *    #     0      1      2      3      4      5      6      7      8      9
 *         [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]
 *   Step 0: send message to (rank - k^0 * i), receive message from (rank + k^0 * i)
 *           message size is k^0 * block size and i is between [1, k-1]
 *    #     0      1      2      3      4      5      6      7      8      9
 *         [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]
 *         [1]    [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]    [0]
 *         [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]    [0]    [1]
 *   Step 1: send message to (rank - k^1 * i), receive message from (rank + k^1 * i)
 *           message size is k^1 * block size
 *    #     0      1      2      3      4      5      6      7      8      9
 *         [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]
 *         [1]    [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]    [0]
 *         [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]    [0]    [1]
 *         [3]    [4]    [5]    [6]    [7]    [8]    [9]    [0]    [1]    [2]
 *         [4]    [5]    [6]    [7]    [8]    [9]    [0]    [1]    [2]    [3]
 *         [5]    [6]    [7]    [8]    [9]    [0]    [1]    [2]    [3]    [4]
 *         [6]    [7]    [8]    [9]    [0]    [1]    [2]    [3]    [4]    [5]
 *         [7]    [8]    [9]    [0]    [1]    [2]    [3]    [4]    [5]    [6]
 *         [8]    [9]    [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]
 *   Step 2: send message to (rank - k^2 * i), receive message from (rank + k^2 * i)
 *           message size is k^2 * block size or "all remaining blocks" for each exchange
 *    #     0      1      2      3      4      5      6      7      8      9
 *         [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]
 *         [1]    [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]    [0]
 *         [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]    [0]    [1]
 *         [3]    [4]    [5]    [6]    [7]    [8]    [9]    [0]    [1]    [2]
 *         [4]    [5]    [6]    [7]    [8]    [9]    [0]    [1]    [2]    [3]
 *         [5]    [6]    [7]    [8]    [9]    [0]    [1]    [2]    [3]    [4]
 *         [6]    [7]    [8]    [9]    [0]    [1]    [2]    [3]    [4]    [5]
 *         [7]    [8]    [9]    [0]    [1]    [2]    [3]    [4]    [5]    [6]
 *         [8]    [9]    [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]
 *         [9]    [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]    [8]
 *    Finalization: Do a local shift (except rank 0) to get data in correct place
 *    #     0      1      2      3      4      5      6      7      8      9
 *         [0]    [0]    [0]    [0]    [0]    [0]    [0]    [0]    [0]    [0]
 *         [1]    [1]    [1]    [1]    [1]    [1]    [1]    [1]    [1]    [1]
 *         [2]    [2]    [2]    [2]    [2]    [2]    [2]    [2]    [2]    [2]
 *         [3]    [3]    [3]    [3]    [3]    [3]    [3]    [3]    [3]    [3]
 *         [4]    [4]    [4]    [4]    [4]    [4]    [4]    [4]    [4]    [4]
 *         [5]    [5]    [5]    [5]    [5]    [5]    [5]    [5]    [5]    [5]
 *         [6]    [6]    [6]    [6]    [6]    [6]    [6]    [6]    [6]    [6]
 *         [7]    [7]    [7]    [7]    [7]    [7]    [7]    [7]    [7]    [7]
 *         [8]    [8]    [8]    [8]    [8]    [8]    [8]    [8]    [8]    [8]
 *         [9]    [9]    [9]    [9]    [9]    [9]    [9]    [9]    [9]    [9]
 */
int ompi_coll_base_allgather_intra_k_bruck(const void *sbuf, size_t scount,
                                          struct ompi_datatype_t *sdtype,
                                          void* rbuf, size_t rcount,
                                          struct ompi_datatype_t *rdtype,
                                          struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module,
                                          int radix)
{
    int line = -1, rank, size, dst, src, err = MPI_SUCCESS;
    int recvcount, distance;
    ptrdiff_t rlb, rextent;
    ptrdiff_t rsize, rgap = 0;
    ompi_request_t **reqs = NULL;
    int num_reqs, max_reqs = 0;

    char *tmpsend = NULL;
    char *tmprecv = NULL;
    char *tmp_buf = NULL;
    char *tmp_buf_start = NULL;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:allgather_intra_k_bruck radix %d rank %d", radix, rank));
    err = ompi_datatype_get_extent (rdtype, &rlb, &rextent);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    if (0 != rank) {
        /* Compute the temporary buffer size, including datatypes empty gaps */
        rsize = opal_datatype_span(&rdtype->super, (size_t)rcount * (size - rank), &rgap);
        tmp_buf = (char *) malloc(rsize);
        tmp_buf_start = tmp_buf - rgap;
    }

    // tmprecv points to the data initially on this rank, handle mpi_in_place case
    tmprecv = (char*) rbuf;
    if (MPI_IN_PLACE != sbuf) {
        tmpsend = (char*) sbuf;
        err = ompi_datatype_sndrcv(tmpsend, scount, sdtype, tmprecv, rcount, rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    } else if (0 != rank) {
        // root data placement is at the correct poistion
        tmpsend = ((char*)rbuf) + (ptrdiff_t)rank * (ptrdiff_t)rcount * rextent;
        err = ompi_datatype_copy_content_same_ddt(rdtype, rcount, tmprecv, tmpsend);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }
    /*
       Maximum number of communication phases logk(n)
       For each phase i, rank r:
       - increase the distance and recvcount by k times
       - sends (k - 1) messages which starts at beginning of rbuf and has size
       (recvcount) to rank (r - distance * j)
       - receives (k - 1) messages of size recvcount from rank (r + distance * j)
       at location (rbuf + distance * j * rcount * rext)
       - calculate the remaining data for each of the (k - 1) messages in the last
       phase to complete all transactions
    */
    max_reqs = 2 * (radix - 1);
    reqs = ompi_coll_base_comm_get_reqs(module->base_data, max_reqs);
    recvcount = 1;
    tmpsend = (char*) rbuf;
    for (distance = 1; distance < size; distance *= radix) {
        num_reqs = 0;
        for (int j = 1; j < radix; j++)
        {
            if (distance * j >= size) {
                break;
            }
            src = (rank + distance * j) % size;
            dst = (rank - distance * j + size) % size;

            tmprecv = tmpsend + (ptrdiff_t)distance * j * rcount * rextent;

            if (distance <= (size / radix)) {
                recvcount = distance;
            } else {
                recvcount = (distance < (size - distance * j)?
                            distance:(size - distance * j));
            }

            err = MCA_PML_CALL(irecv(tmprecv,
                                     recvcount * rcount,
                                     rdtype,
                                     src,
                                     MCA_COLL_BASE_TAG_ALLGATHER,
                                     comm,
                                     &reqs[num_reqs++]));
            if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
            err = MCA_PML_CALL(isend(tmpsend,
                                     recvcount * rcount,
                                     rdtype,
                                     dst,
                                     MCA_COLL_BASE_TAG_ALLGATHER,
                                     MCA_PML_BASE_SEND_STANDARD,
                                     comm,
                                     &reqs[num_reqs++]));
            if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
        }
        err = ompi_request_wait_all(num_reqs, reqs, MPI_STATUSES_IGNORE);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    // Finalization step:        On all ranks except 0, data needs to be shifted locally
    if (0 != rank) {
        err = ompi_datatype_copy_content_same_ddt(rdtype,
                                                  ((ptrdiff_t)(size - rank) * rcount),
                                                  tmp_buf_start,
                                                  rbuf);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

        tmpsend = (char*) rbuf + (ptrdiff_t)(size - rank) * rcount * rextent;
        err = ompi_datatype_copy_content_same_ddt(rdtype,
                                                  (ptrdiff_t)rank * rcount,
                                                  rbuf,
                                                  tmpsend);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

        tmprecv = (char*) rbuf + (ptrdiff_t)rank * rcount * rextent;
        err = ompi_datatype_copy_content_same_ddt(rdtype,
                                                  (ptrdiff_t)(size - rank) * rcount,
                                                  tmprecv,
                                                  tmp_buf_start);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    if(tmp_buf != NULL) free(tmp_buf);
    return MPI_SUCCESS;

err_hndl:
    if( NULL != reqs ) {
        if (MPI_ERR_IN_STATUS == err) {
            for( num_reqs = 0; num_reqs < max_reqs; num_reqs++ ) {
                if (MPI_REQUEST_NULL == reqs[num_reqs]) continue;
                if (MPI_ERR_PENDING == reqs[num_reqs]->req_status.MPI_ERROR) continue;
                if (reqs[num_reqs]->req_status.MPI_ERROR != MPI_SUCCESS) {
                    err = reqs[num_reqs]->req_status.MPI_ERROR;
                    break;
                }
            }
        }
        ompi_coll_base_free_reqs(reqs, max_reqs);
    }
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    if(tmp_buf != NULL) {
        free(tmp_buf);
        tmp_buf = NULL;
        tmp_buf_start = NULL;
    }
    (void)line;  // silence compiler warning
    return err;
}

/**
 * A greedy algorithm to exchange data among processes in the communicator via
 * non-blocking direct messaging for allgather. This avoids the round trip in
 * a rooted communication pattern, e.g. gather on the root and then broadcast to peers.
 *
 * This algorithm benefits inter-node allgather latency for small messages.
 * Caution is needed on larger communicators(n) and data sizes(m), which will
 * result in m*n^2 total traffic and potential network congestion.
 */
int ompi_coll_base_allgather_direct_messaging(const void *sbuf, size_t scount,
                                              struct ompi_datatype_t *sdtype,
                                              void* rbuf, size_t rcount,
                                              struct ompi_datatype_t *rdtype,
                                              struct ompi_communicator_t *comm,
                                              mca_coll_base_module_t *module)
{
    int line = -1, rank, comm_size, err = MPI_SUCCESS;
    ptrdiff_t rlb, rextent;
    ptrdiff_t incr;
    ompi_request_t **reqs = NULL;
    int max_reqs = 0, reqs_needed = 0;
    int peer_rank = 0;

    char *tmpsend = NULL;
    char *tmprecv = NULL;

    rank = ompi_comm_rank(comm);
    comm_size = ompi_comm_size(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:ompi_coll_base_allgather_direct_messaging rank %d", rank));

    err = ompi_datatype_get_extent (rdtype, &rlb, &rextent);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    if (MPI_IN_PLACE != sbuf) {
        tmpsend = (char*) sbuf;
        tmprecv = (char*) rbuf + (ptrdiff_t)rank * (ptrdiff_t)rcount * rextent;
        err = ompi_datatype_sndrcv(tmpsend, scount, sdtype, tmprecv, rcount, rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }
    }
    else {
        tmpsend = (char*) rbuf + (ptrdiff_t)rank * (ptrdiff_t)rcount * rextent;
    }

    /* Requests for send to AND receive from everyone else */
    reqs_needed = (comm_size - 1) * 2;
    reqs = ompi_coll_base_comm_get_reqs(module->base_data, reqs_needed);

    incr = rextent * rcount;

    /* Exchange data with peer processes, excluding self */
    for (int i = 1; i < comm_size; ++i) {
        /* Start at the next rank */
        peer_rank = (rank + i) % comm_size;

        /* Prepare for the next receive buffer */
        tmprecv = (char*)rbuf + peer_rank * incr;

        err = MCA_PML_CALL(irecv(tmprecv, rcount, rdtype, peer_rank, MCA_COLL_BASE_TAG_ALLGATHER,
                                 comm, &reqs[max_reqs++]));
        if (MPI_SUCCESS != err) {
            goto err_hndl;
        }

        err = MCA_PML_CALL(isend(tmpsend, scount, sdtype, peer_rank, MCA_COLL_BASE_TAG_ALLGATHER,
                                 MCA_PML_BASE_SEND_STANDARD, comm, &reqs[max_reqs++]));
        if (MPI_SUCCESS != err) {
            goto err_hndl;
        }
    }

    err = ompi_request_wait_all(max_reqs, reqs, MPI_STATUSES_IGNORE);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    return MPI_SUCCESS;

err_hndl:
    if (NULL != reqs) {
        if (MPI_ERR_IN_STATUS == err) {
            for (int i = 0; i < reqs_needed; i++) {
                if (MPI_REQUEST_NULL == reqs[i])
                    continue;
                if (MPI_ERR_PENDING == reqs[i]->req_status.MPI_ERROR)
                    continue;
                if (MPI_SUCCESS != reqs[i]->req_status.MPI_ERROR) {
                    err = reqs[i]->req_status.MPI_ERROR;
                    break;
                }
            }
        }
        ompi_coll_base_free_reqs(reqs, reqs_needed);
    }
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void)line;  // silence compiler warning
    return err;
}
/* copied function (with appropriate renaming) ends here */
