/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All Rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_base_topo.h"
#include "coll_base_util.h"

/**
 * A quick version of the MPI_Sendreceive implemented for the barrier.
 * No actual data is moved across the wire, we use 0-byte messages to
 * signal a two peer synchronization.
 */
static inline int
ompi_coll_base_sendrecv_zero(int dest, int stag,
                              int source, int rtag,
                              MPI_Comm comm)

{
    int err, line = 0;
    ompi_request_t* reqs[2];
    ompi_status_public_t statuses[2];

    /* post new irecv */
    err = MCA_PML_CALL(irecv( NULL, 0, MPI_BYTE, source, rtag,
                              comm, &reqs[0]));
    if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

    /* send data to children */
    err = MCA_PML_CALL(isend( NULL, 0, MPI_BYTE, dest, stag,
                              MCA_PML_BASE_SEND_STANDARD, comm, &reqs[1]));
    if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

    err = ompi_request_wait_all( 2, reqs, statuses );
    if( MPI_ERR_IN_STATUS == err ) {
        /* As we use wait_all we will get MPI_ERR_IN_STATUS which is not an error
         * code that we can propagate up the stack. Instead, look for the real
         * error code from the MPI_ERROR in the status.
         */
        int err_index = 0;
        if( MPI_SUCCESS == statuses[0].MPI_ERROR ) {
            err_index = 1;
        }
        err = statuses[err_index].MPI_ERROR;
        OPAL_OUTPUT ((ompi_coll_base_framework.framework_output, "%s:%d: Error %d occurred in the %s"
                                              " stage of ompi_coll_base_sendrecv_zero\n",
                      __FILE__, line, err, (0 == err_index ? "receive" : "send")));
        return err;
    }
    if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

    return (MPI_SUCCESS);

 error_handler:
    /* Error discovered during the posting of the irecv or isend,
     * and no status is available.
     */
    OPAL_OUTPUT ((ompi_coll_base_framework.framework_output, "%s:%d: Error %d occurred\n",
                  __FILE__, line, err));
    return err;
}

/*
 * Barrier is ment to be a synchronous operation, as some BTLs can mark
 * a request done before its passed to the NIC and progress might not be made
 * elsewhere we cannot allow a process to exit the barrier until its last
 * [round of] sends are completed.
 *
 * It is last round of sends rather than 'last' individual send as each pair of
 * peers can use different channels/devices/btls and the receiver of one of
 * these sends might be forced to wait as the sender
 * leaves the collective and does not make progress until the next mpi call
 *
 */

/*
 * Simple double ring version of barrier
 *
 * synchronous gurantee made by last ring of sends are synchronous
 *
 */
int ompi_coll_base_barrier_intra_doublering(struct ompi_communicator_t *comm,
                                             mca_coll_base_module_t *module)
{
    int rank, size, err = 0, line = 0, left, right;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"ompi_coll_base_barrier_intra_doublering rank %d", rank));

    left = ((rank-1)%size);
    right = ((rank+1)%size);

    if (rank > 0) { /* receive message from the left */
        err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, left,
                                MCA_COLL_BASE_TAG_BARRIER, comm,
                                MPI_STATUS_IGNORE));
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
    }

    /* Send message to the right */
    err = MCA_PML_CALL(send((void*)NULL, 0, MPI_BYTE, right,
                            MCA_COLL_BASE_TAG_BARRIER,
                            MCA_PML_BASE_SEND_STANDARD, comm));
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

    /* root needs to receive from the last node */
    if (rank == 0) {
        err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, left,
                                MCA_COLL_BASE_TAG_BARRIER, comm,
                                MPI_STATUS_IGNORE));
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
    }

    /* Allow nodes to exit */
    if (rank > 0) { /* post Receive from left */
        err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, left,
                                MCA_COLL_BASE_TAG_BARRIER, comm,
                                MPI_STATUS_IGNORE));
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
    }

    /* send message to the right one */
    err = MCA_PML_CALL(send((void*)NULL, 0, MPI_BYTE, right,
                            MCA_COLL_BASE_TAG_BARRIER,
                            MCA_PML_BASE_SEND_SYNCHRONOUS, comm));
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

    /* rank 0 post receive from the last node */
    if (rank == 0) {
        err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, left,
                                MCA_COLL_BASE_TAG_BARRIER, comm,
                                MPI_STATUS_IGNORE));
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
    }

    return MPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    return err;
}


/*
 * To make synchronous, uses sync sends and sync sendrecvs
 */

int ompi_coll_base_barrier_intra_recursivedoubling(struct ompi_communicator_t *comm,
                                                    mca_coll_base_module_t *module)
{
    int rank, size, adjsize, err, line, mask, remote;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_barrier_intra_recursivedoubling rank %d",
                 rank));

    /* do nearest power of 2 less than size calc */
    adjsize = opal_next_poweroftwo(size);
    adjsize >>= 1;

    /* if size is not exact power of two, perform an extra step */
    if (adjsize != size) {
        if (rank >= adjsize) {
            /* send message to lower ranked node */
            remote = rank - adjsize;
            err = ompi_coll_base_sendrecv_zero(remote, MCA_COLL_BASE_TAG_BARRIER,
                                                remote, MCA_COLL_BASE_TAG_BARRIER,
                                                comm);
            if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}

        } else if (rank < (size - adjsize)) {

            /* receive message from high level rank */
            err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, rank+adjsize,
                                    MCA_COLL_BASE_TAG_BARRIER, comm,
                                    MPI_STATUS_IGNORE));

            if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}
        }
    }

    /* exchange messages */
    if ( rank < adjsize ) {
        mask = 0x1;
        while ( mask < adjsize ) {
            remote = rank ^ mask;
            mask <<= 1;
            if (remote >= adjsize) continue;

            /* post receive from the remote node */
            err = ompi_coll_base_sendrecv_zero(remote, MCA_COLL_BASE_TAG_BARRIER,
                                                remote, MCA_COLL_BASE_TAG_BARRIER,
                                                comm);
            if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}
        }
    }

    /* non-power of 2 case */
    if (adjsize != size) {
        if (rank < (size - adjsize)) {
            /* send enter message to higher ranked node */
            remote = rank + adjsize;
            err = MCA_PML_CALL(send((void*)NULL, 0, MPI_BYTE, remote,
                                    MCA_COLL_BASE_TAG_BARRIER,
                                    MCA_PML_BASE_SEND_SYNCHRONOUS, comm));

            if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}
        }
    }

    return MPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    return err;
}


/*
 * To make synchronous, uses sync sends and sync sendrecvs
 */

int ompi_coll_base_barrier_intra_bruck(struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module)
{
    int rank, size, distance, to, from, err, line = 0;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_barrier_intra_bruck rank %d", rank));

    /* exchange data with rank-2^k and rank+2^k */
    for (distance = 1; distance < size; distance <<= 1) {
        from = (rank + size - distance) % size;
        to   = (rank + distance) % size;

        /* send message to lower ranked node */
        err = ompi_coll_base_sendrecv_zero(to, MCA_COLL_BASE_TAG_BARRIER,
                                            from, MCA_COLL_BASE_TAG_BARRIER,
                                            comm);
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}
    }

    return MPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    return err;
}


/*
 * To make synchronous, uses sync sends and sync sendrecvs
 */
/* special case for two processes */
int ompi_coll_base_barrier_intra_two_procs(struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module)
{
    int remote, err;

    remote = ompi_comm_rank(comm);
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_barrier_intra_two_procs rank %d", remote));
    remote = (remote + 1) & 0x1;

    err = ompi_coll_base_sendrecv_zero(remote, MCA_COLL_BASE_TAG_BARRIER,
                                        remote, MCA_COLL_BASE_TAG_BARRIER,
                                        comm);
    return (err);
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

int ompi_coll_base_barrier_intra_basic_linear(struct ompi_communicator_t *comm,
                                              mca_coll_base_module_t *module)
{
    int i, err, rank, size;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* All non-root send & receive zero-length message. */
    if (rank > 0) {
        err = MCA_PML_CALL(send (NULL, 0, MPI_BYTE, 0,
                                 MCA_COLL_BASE_TAG_BARRIER,
                                 MCA_PML_BASE_SEND_STANDARD, comm));
        if (MPI_SUCCESS != err) {
            return err;
        }

        err = MCA_PML_CALL(recv (NULL, 0, MPI_BYTE, 0,
                                 MCA_COLL_BASE_TAG_BARRIER,
                                 comm, MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* The root collects and broadcasts the messages. */

    else {
        ompi_request_t** requests;

        requests = (ompi_request_t**)malloc( size * sizeof(ompi_request_t*) );
        for (i = 1; i < size; ++i) {
            err = MCA_PML_CALL(irecv(NULL, 0, MPI_BYTE, MPI_ANY_SOURCE,
                                     MCA_COLL_BASE_TAG_BARRIER, comm,
                                     &(requests[i])));
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
        ompi_request_wait_all( size-1, requests+1, MPI_STATUSES_IGNORE );

        for (i = 1; i < size; ++i) {
            err = MCA_PML_CALL(send(NULL, 0, MPI_BYTE, i,
                                    MCA_COLL_BASE_TAG_BARRIER,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (MPI_SUCCESS != err) {
                return err;
            }
        }

        free( requests );
    }

    /* All done */

    return MPI_SUCCESS;

}
/* copied function (with appropriate renaming) ends here */

/*
 * Another recursive doubling type algorithm, but in this case
 * we go up the tree and back down the tree.
 */
int ompi_coll_base_barrier_intra_tree(struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module)
{
    int rank, size, depth, err, jump, partner;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_barrier_intra_tree %d",
                 rank));

    /* Find the nearest power of 2 of the communicator size. */
    depth = opal_next_poweroftwo_inclusive(size);

    for (jump=1; jump<depth; jump<<=1) {
        partner = rank ^ jump;
        if (!(partner & (jump-1)) && partner < size) {
            if (partner > rank) {
                err = MCA_PML_CALL(recv (NULL, 0, MPI_BYTE, partner,
                                         MCA_COLL_BASE_TAG_BARRIER, comm,
                                         MPI_STATUS_IGNORE));
                if (MPI_SUCCESS != err)
                    return err;
            } else if (partner < rank) {
                err = MCA_PML_CALL(send (NULL, 0, MPI_BYTE, partner,
                                         MCA_COLL_BASE_TAG_BARRIER,
                                         MCA_PML_BASE_SEND_STANDARD, comm));
                if (MPI_SUCCESS != err)
                    return err;
            }
        }
    }

    depth >>= 1;
    for (jump = depth; jump>0; jump>>=1) {
        partner = rank ^ jump;
        if (!(partner & (jump-1)) && partner < size) {
            if (partner > rank) {
                err = MCA_PML_CALL(send (NULL, 0, MPI_BYTE, partner,
                                         MCA_COLL_BASE_TAG_BARRIER,
                                         MCA_PML_BASE_SEND_STANDARD, comm));
                if (MPI_SUCCESS != err)
                    return err;
            } else if (partner < rank) {
                err = MCA_PML_CALL(recv (NULL, 0, MPI_BYTE, partner,
                                         MCA_COLL_BASE_TAG_BARRIER, comm,
                                         MPI_STATUS_IGNORE));
                if (MPI_SUCCESS != err)
                    return err;
            }
        }
    }

    return MPI_SUCCESS;
}
