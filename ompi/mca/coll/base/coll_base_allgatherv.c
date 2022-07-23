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
 * Copyright (c) 2013-2017 Los Alamos National Security, LLC. All Rights
 *                         reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "math.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_base_topo.h"
#include "coll_base_util.h"

/*
 * ompi_coll_base_allgatherv_intra_bruck
 *
 * Function:     allgather using O(log(N)) steps.
 * Accepts:      Same arguments as MPI_Allgather
 * Returns:      MPI_SUCCESS or error code
 *
 * Description:  Variation to All-to-all algorithm described by Bruck et al.in
 *               "Efficient Algorithms for All-to-all Communications
 *                in Multiport Message-Passing Systems"
 * Note:         Unlike in case of allgather implementation, we relay on
 *               indexed datatype to select buffers appropriately.
 *               The only additional memory requirement is for creation of
 *               temporary datatypes.
 * Example on 7 nodes (memory lay out need not be in-order)
 *   Initial set up:
 *    #     0      1      2      3      4      5      6
 *         [0]    [ ]    [ ]    [ ]    [ ]    [ ]    [ ]
 *         [ ]    [1]    [ ]    [ ]    [ ]    [ ]    [ ]
 *         [ ]    [ ]    [2]    [ ]    [ ]    [ ]    [ ]
 *         [ ]    [ ]    [ ]    [3]    [ ]    [ ]    [ ]
 *         [ ]    [ ]    [ ]    [ ]    [4]    [ ]    [ ]
 *         [ ]    [ ]    [ ]    [ ]    [ ]    [5]    [ ]
 *         [ ]    [ ]    [ ]    [ ]    [ ]    [ ]    [6]
 *   Step 0: send message to (rank - 2^0), receive message from (rank + 2^0)
 *    #     0      1      2      3      4      5      6
 *         [0]    [ ]    [ ]    [ ]    [ ]    [ ]    [0]
 *         [1]    [1]    [ ]    [ ]    [ ]    [ ]    [ ]
 *         [ ]    [2]    [2]    [ ]    [ ]    [ ]    [ ]
 *         [ ]    [ ]    [3]    [3]    [ ]    [ ]    [ ]
 *         [ ]    [ ]    [ ]    [4]    [4]    [ ]    [ ]
 *         [ ]    [ ]    [ ]    [ ]    [5]    [5]    [ ]
 *         [ ]    [ ]    [ ]    [ ]    [ ]    [6]    [6]
 *   Step 1: send message to (rank - 2^1), receive message from (rank + 2^1).
 *           message contains all blocks from (rank) .. (rank + 2^2) with
 *           wrap around.
 *    #     0      1      2      3      4      5      6
 *         [0]    [ ]    [ ]    [ ]    [0]    [0]    [0]
 *         [1]    [1]    [ ]    [ ]    [ ]    [1]    [1]
 *         [2]    [2]    [2]    [ ]    [ ]    [ ]    [2]
 *         [3]    [3]    [3]    [3]    [ ]    [ ]    [ ]
 *         [ ]    [4]    [4]    [4]    [4]    [ ]    [ ]
 *         [ ]    [ ]    [5]    [5]    [5]    [5]    [ ]
 *         [ ]    [ ]    [ ]    [6]    [6]    [6]    [6]
 *   Step 2: send message to (rank - 2^2), receive message from (rank + 2^2).
 *           message size is "all remaining blocks"
 *    #     0      1      2      3      4      5      6
 *         [0]    [0]    [0]    [0]    [0]    [0]    [0]
 *         [1]    [1]    [1]    [1]    [1]    [1]    [1]
 *         [2]    [2]    [2]    [2]    [2]    [2]    [2]
 *         [3]    [3]    [3]    [3]    [3]    [3]    [3]
 *         [4]    [4]    [4]    [4]    [4]    [4]    [4]
 *         [5]    [5]    [5]    [5]    [5]    [5]    [5]
 *         [6]    [6]    [6]    [6]    [6]    [6]    [6]
 */
int ompi_coll_base_allgatherv_intra_bruck(const void *sbuf, int scount,
                                           struct ompi_datatype_t *sdtype,
                                           void *rbuf, const int *rcounts,
                                           const int *rdispls,
                                           struct ompi_datatype_t *rdtype,
                                           struct ompi_communicator_t *comm,
                                           mca_coll_base_module_t *module)
{
    int line = -1, err = 0, rank, size, sendto, recvfrom, distance, blockcount, i;
    int *new_rcounts = NULL, *new_rdispls = NULL, *new_scounts = NULL, *new_sdispls = NULL;
    ptrdiff_t rlb, rext;
    char *tmpsend = NULL, *tmprecv = NULL;
    struct ompi_datatype_t *new_rdtype, *new_sdtype;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:allgather_intra_bruck rank %d", rank));

    err = ompi_datatype_get_extent (rdtype, &rlb, &rext);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    /* Initialization step:
       - if send buffer is not MPI_IN_PLACE, copy send buffer to block rank of
       the receive buffer.
    */
    tmprecv = (char*) rbuf + (ptrdiff_t)rdispls[rank] * rext;
    if (MPI_IN_PLACE != sbuf) {
        tmpsend = (char*) sbuf;
        err = ompi_datatype_sndrcv(tmpsend, scount, sdtype,
                                   tmprecv, rcounts[rank], rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }

    }

    /* Communication step:
       At every step i, rank r:
       - doubles the distance
       - sends message with blockcount blocks, (rbuf[rank] .. rbuf[rank + 2^i])
       to rank (r - distance)
       - receives message of blockcount blocks,
       (rbuf[r + distance] ... rbuf[(r+distance) + 2^i]) from
       rank (r + distance)
       - blockcount doubles until the last step when only the remaining data is
       exchanged.
    */
    blockcount = 1;
    tmpsend = (char*) rbuf;

    new_rcounts = (int*) calloc(4*size, sizeof(int));
    if (NULL == new_rcounts) { err = -1; line = __LINE__; goto err_hndl; }
    new_rdispls = new_rcounts + size;
    new_scounts = new_rdispls + size;
    new_sdispls = new_scounts + size;

    for (distance = 1; distance < size; distance<<=1) {

        recvfrom = (rank + distance) % size;
        sendto = (rank - distance + size) % size;

        if (distance <= (size >> 1)) {
            blockcount = distance;
        } else {
            blockcount = size - distance;
        }

        /* create send and receive datatypes */
        for (i = 0; i < blockcount; i++) {
            const int tmp_srank = (rank + i) % size;
            const int tmp_rrank = (recvfrom + i) % size;
            new_scounts[i] = rcounts[tmp_srank];
            new_sdispls[i] = rdispls[tmp_srank];
            new_rcounts[i] = rcounts[tmp_rrank];
            new_rdispls[i] = rdispls[tmp_rrank];
        }
        err = ompi_datatype_create_indexed(blockcount, new_scounts, new_sdispls,
                                           rdtype, &new_sdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
        err = ompi_datatype_create_indexed(blockcount, new_rcounts, new_rdispls,
                                           rdtype, &new_rdtype);

        err = ompi_datatype_commit(&new_sdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
        err = ompi_datatype_commit(&new_rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

        /* Sendreceive */
        err = ompi_coll_base_sendrecv(rbuf, 1, new_sdtype, sendto,
                                       MCA_COLL_BASE_TAG_ALLGATHERV,
                                       rbuf, 1, new_rdtype, recvfrom,
                                       MCA_COLL_BASE_TAG_ALLGATHERV,
                                       comm, MPI_STATUS_IGNORE, rank);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

        ompi_datatype_destroy(&new_sdtype);
        ompi_datatype_destroy(&new_rdtype);
    }

    free(new_rcounts);

    return OMPI_SUCCESS;

 err_hndl:
    if( NULL != new_rcounts ) free(new_rcounts);

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

int ompi_coll_base_allgatherv_intra_sparbit(const void *sbuf, int scount,
                                           struct ompi_datatype_t *sdtype,
                                           void* rbuf, const int *rcounts,
                                           const int *rdispls,
                                           struct ompi_datatype_t *rdtype,
                                           struct ompi_communicator_t *comm,
                                           mca_coll_base_module_t *module)
{
    /* ################# VARIABLE DECLARATION, BUFFER CREATION AND PREPARATION FOR THE ALGORITHM ######################## */

    /* list of variable declaration */
    int rank = 0, comm_size = 0, comm_log = 0, exclusion = 0; 
    int data_expected = 1, transfer_count = 0, step_requests = 0;
    int sendto, recvfrom, send_disp, recv_disp;
    uint32_t last_ignore, ignore_steps, distance = 1;

    int err = 0;
    int line = -1;

    ptrdiff_t rlb, rext;

    char *tmpsend = NULL, *tmprecv = NULL;

    MPI_Request *requests = NULL;

    /* printf("utilizando o allgatherv novo!!\n"); */

    /* algorithm choice information printing */
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output, 
                 "coll:sparbit:allgather_sync_intra rank %d", rank));

    comm_size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    err = ompi_datatype_get_extent(rdtype, &rlb, &rext);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    /* if the MPI_IN_PLACE condition is not set, copy the send buffer to the receive buffer to perform the sends (all the data is extracted and forwarded from the recv buffer)*/
    /* tmprecv and tmpsend are used as abstract pointers to simplify send and receive buffer choice */
    tmprecv = (char *) rbuf;
    if(MPI_IN_PLACE != sbuf){
        tmpsend = (char *) sbuf; 
        err = ompi_datatype_sndrcv(tmpsend, scount, sdtype, tmprecv + (ptrdiff_t) rdispls[rank] * rext, scount, rdtype);
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

           /* Since each process sends several non-contiguos blocks of data to the same destination,
            * each block sent (and therefore each send and recv call) needs a different tag. */
           /* As base OpenMPI only provides one tag for allgather, we are forced to use a tag space
            * from other components in the send and recv calls */
           if(rcounts[send_disp] > 0)
               MCA_PML_CALL(isend(tmpsend + (ptrdiff_t) rdispls[send_disp] * rext, rcounts[send_disp], rdtype, sendto, MCA_COLL_BASE_TAG_HCOLL_BASE - send_disp, MCA_PML_BASE_SEND_STANDARD, comm, requests + step_requests++));
           if(rcounts[recv_disp] > 0)
               MCA_PML_CALL(irecv(tmprecv + (ptrdiff_t) rdispls[recv_disp] * rext, rcounts[recv_disp], rdtype, recvfrom, MCA_COLL_BASE_TAG_HCOLL_BASE - recv_disp, comm, requests + step_requests++));
       }
       ompi_request_wait_all(step_requests, requests, MPI_STATUSES_IGNORE);

       distance >>= 1; 
       /* calculates the data expected for the next step, based on the current number of blocks and eventual exclusions */
       data_expected = (data_expected << 1) - exclusion;
       exclusion = step_requests = 0;
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
 * ompi_coll_base_allgatherv_intra_ring
 *
 * Function:     allgatherv using O(N) steps.
 * Accepts:      Same arguments as MPI_Allgatherv
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
int ompi_coll_base_allgatherv_intra_ring(const void *sbuf, int scount,
                                          struct ompi_datatype_t *sdtype,
                                          void* rbuf, const int *rcounts, const int *rdisps,
                                          struct ompi_datatype_t *rdtype,
                                          struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module)
{
    int line = -1, rank, size, sendto, recvfrom, i, recvdatafrom, senddatafrom, err = 0;
    ptrdiff_t rlb, rext;
    char *tmpsend = NULL, *tmprecv = NULL;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:allgatherv_intra_ring rank %d", rank));

    err = ompi_datatype_get_extent (rdtype, &rlb, &rext);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    /* Initialization step:
       - if send buffer is not MPI_IN_PLACE, copy send buffer to
       the appropriate block of receive buffer
    */
    tmprecv = (char*) rbuf + (ptrdiff_t)rdisps[rank] * rext;
    if (MPI_IN_PLACE != sbuf) {
        tmpsend = (char*) sbuf;
        err = ompi_datatype_sndrcv(tmpsend, scount, sdtype,
                                   tmprecv, rcounts[rank], rdtype);
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

        tmprecv = (char*)rbuf + rdisps[recvdatafrom] * rext;
        tmpsend = (char*)rbuf + rdisps[senddatafrom] * rext;

        /* Sendreceive */
        err = ompi_coll_base_sendrecv(tmpsend, rcounts[senddatafrom], rdtype,
                                       sendto, MCA_COLL_BASE_TAG_ALLGATHERV,
                                       tmprecv, rcounts[recvdatafrom], rdtype,
                                       recvfrom, MCA_COLL_BASE_TAG_ALLGATHERV,
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
 * ompi_coll_base_allgatherv_intra_neighborexchange
 *
 * Function:     allgatherv using N/2 steps (O(N))
 * Accepts:      Same arguments as MPI_Allgatherv
 * Returns:      MPI_SUCCESS or error code
 *
 * Description:  Neighbor Exchange algorithm for allgather adapted for
 *               allgatherv.
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
ompi_coll_base_allgatherv_intra_neighborexchange(const void *sbuf, int scount,
                                                  struct ompi_datatype_t *sdtype,
                                                  void* rbuf, const int *rcounts, const int *rdispls,
                                                  struct ompi_datatype_t *rdtype,
                                                  struct ompi_communicator_t *comm,
                                                  mca_coll_base_module_t *module)
{
    int line = -1, rank, size, i, even_rank, err = 0;
    int neighbor[2], offset_at_step[2], recv_data_from[2], send_data_from;
    int new_scounts[2], new_sdispls[2], new_rcounts[2], new_rdispls[2];
    ptrdiff_t rlb, rext;
    char *tmpsend = NULL, *tmprecv = NULL;
    struct ompi_datatype_t  *new_rdtype, *new_sdtype;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    if (size % 2) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "coll:base:allgatherv_intra_neighborexchange WARNING: odd size %d, switching to ring algorithm",
                     size));
        return ompi_coll_base_allgatherv_intra_ring(sbuf, scount, sdtype,
                                                     rbuf, rcounts,
                                                     rdispls, rdtype,
                                                     comm, module);
    }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:allgatherv_intra_neighborexchange rank %d", rank));

    err = ompi_datatype_get_extent (rdtype, &rlb, &rext);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    /* Initialization step:
       - if send buffer is not MPI_IN_PLACE, copy send buffer to
       the appropriate block of receive buffer
    */
    tmprecv = (char*) rbuf + (ptrdiff_t)rdispls[rank] * rext;
    if (MPI_IN_PLACE != sbuf) {
        tmpsend = (char*) sbuf;
        err = ompi_datatype_sndrcv(tmpsend, scount, sdtype,
                                   tmprecv, rcounts[rank], rdtype);
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
       Note, we need to create indexed datatype to send and receive these
       blocks properly.
    */
    tmprecv = (char*)rbuf + (ptrdiff_t)rdispls[neighbor[0]] * rext;
    tmpsend = (char*)rbuf + (ptrdiff_t)rdispls[rank] * rext;
    err = ompi_coll_base_sendrecv(tmpsend, rcounts[rank], rdtype,
                                   neighbor[0], MCA_COLL_BASE_TAG_ALLGATHERV,
                                   tmprecv, rcounts[neighbor[0]], rdtype,
                                   neighbor[0], MCA_COLL_BASE_TAG_ALLGATHERV,
                                   comm, MPI_STATUS_IGNORE, rank);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    /* Determine initial sending counts and displacements*/
    if (even_rank) {
        send_data_from = rank;
    } else {
        send_data_from = recv_data_from[0];
    }

    for (i = 1; i < (size / 2); i++) {
        const int i_parity = i % 2;
        recv_data_from[i_parity] =
            (recv_data_from[i_parity] + offset_at_step[i_parity] + size) % size;

        /* Create new indexed types for sending and receiving.
           We are sending data from ranks (send_data_from) and (send_data_from+1)
           We are receiving data from ranks (recv_data_from[i_parity]) and
           (recv_data_from[i_parity]+1).
        */
        new_scounts[0] = rcounts[send_data_from];
        new_scounts[1] = rcounts[(send_data_from + 1)];
        new_sdispls[0] = rdispls[send_data_from];
        new_sdispls[1] = rdispls[(send_data_from + 1)];
        err = ompi_datatype_create_indexed(2, new_scounts, new_sdispls, rdtype,
                                           &new_sdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
        err = ompi_datatype_commit(&new_sdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

        new_rcounts[0] = rcounts[recv_data_from[i_parity]];
        new_rcounts[1] = rcounts[(recv_data_from[i_parity] + 1)];
        new_rdispls[0] = rdispls[recv_data_from[i_parity]];
        new_rdispls[1] = rdispls[(recv_data_from[i_parity] + 1)];
        err = ompi_datatype_create_indexed(2, new_rcounts, new_rdispls, rdtype,
                                           &new_rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
        err = ompi_datatype_commit(&new_rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

        tmprecv = (char*)rbuf;
        tmpsend = (char*)rbuf;

        /* Sendreceive */
        err = ompi_coll_base_sendrecv(tmpsend, 1, new_sdtype, neighbor[i_parity],
                                       MCA_COLL_BASE_TAG_ALLGATHERV,
                                       tmprecv, 1, new_rdtype, neighbor[i_parity],
                                       MCA_COLL_BASE_TAG_ALLGATHERV,
                                       comm, MPI_STATUS_IGNORE, rank);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

        send_data_from = recv_data_from[i_parity];

        ompi_datatype_destroy(&new_sdtype);
        ompi_datatype_destroy(&new_rdtype);
    }

    return OMPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void)line;  // silence compiler warning
    return err;
}


int ompi_coll_base_allgatherv_intra_two_procs(const void *sbuf, int scount,
                                               struct ompi_datatype_t *sdtype,
                                               void* rbuf, const int *rcounts,
                                               const int *rdispls,
                                               struct ompi_datatype_t *rdtype,
                                               struct ompi_communicator_t *comm,
                                               mca_coll_base_module_t *module)
{
    int line = -1, err = 0, rank, remote;
    char *tmpsend = NULL, *tmprecv = NULL;
    ptrdiff_t rext, lb;

    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_allgatherv_intra_two_procs rank %d", rank));

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
        tmpsend = (char*)rbuf + (ptrdiff_t)rdispls[rank] * rext;
        scount = rcounts[rank];
        sdtype = rdtype;
    }
    tmprecv = (char*)rbuf + (ptrdiff_t)rdispls[remote] * rext;

    err = ompi_coll_base_sendrecv(tmpsend, scount, sdtype, remote,
                                   MCA_COLL_BASE_TAG_ALLGATHERV,
                                   tmprecv, rcounts[remote], rdtype, remote,
                                   MCA_COLL_BASE_TAG_ALLGATHERV,
                                   comm, MPI_STATUS_IGNORE, rank);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    /* Place your data in correct location if necessary */
    if (MPI_IN_PLACE != sbuf) {
        err = ompi_datatype_sndrcv((char*)sbuf, scount, sdtype,
                                   (char*)rbuf + (ptrdiff_t)rdispls[rank] * rext,
                                   rcounts[rank], rdtype);
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
 *	allgatherv_intra_basic
 *
 *	Function:	- allgatherv using other MPI collectives:
 *                        gatherv + bcast (from basic module).
 *	Accepts:	- same as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
ompi_coll_base_allgatherv_intra_basic_default(const void *sbuf, int scount,
                                              struct ompi_datatype_t *sdtype,
                                              void *rbuf, const int *rcounts,
                                              const int *disps,
                                              struct ompi_datatype_t *rdtype,
                                              struct ompi_communicator_t *comm,
                                              mca_coll_base_module_t *module)
{
    int size, rank, err;
    MPI_Aint extent, lb;
    char *send_buf = NULL;
    struct ompi_datatype_t *newtype, *send_type;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    /*
     * We don't have a root process defined. Arbitrarily assign root
     * to process with rank 0 (OMPI convention)
     */

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_allgatherv_intra_basic_default rank %d",
                 rank));

    if (MPI_IN_PLACE == sbuf) {
        ompi_datatype_get_extent(rdtype, &lb, &extent);
        send_type = rdtype;
        send_buf = (char*)rbuf;
        send_buf += ((ptrdiff_t)disps[rank] * extent);
        scount = rcounts[rank];
    } else {
        send_buf = (char*)sbuf;
        send_type = sdtype;
    }

    err = comm->c_coll->coll_gatherv(send_buf,
                                    scount, send_type,rbuf,
                                    rcounts, disps, rdtype, 0,
                                    comm, comm->c_coll->coll_gatherv_module);
    if (MPI_SUCCESS != err) {
        return err;
    }
    /*
     * we now have all the data in the root's rbuf. Need to
     * broadcast the data out to the other processes
     *
     * Need to define a datatype that captures the different vectors
     * from each process. MPI_TYPE_INDEXED with params
     *                    size,rcount,displs,rdtype,newtype
     * should do the trick.
     * Use underlying ddt functions to create, and commit the
     * new datatype on each process, then broadcast and destroy the
     * datatype.
     */

    err = ompi_datatype_create_indexed(size,rcounts,disps,rdtype,&newtype);
    if (MPI_SUCCESS != err) {
        return err;
    }

    err = ompi_datatype_commit(&newtype);
    if(MPI_SUCCESS != err) {
        return err;
    }

    comm->c_coll->coll_bcast(rbuf, 1, newtype, 0, comm,
                            comm->c_coll->coll_bcast_module);

    ompi_datatype_destroy (&newtype);

    return MPI_SUCCESS;
}

/* copied function (with appropriate renaming) ends here */
