/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
 * Barrier is ment to be a synchronous operation, as some BTLs can mark a request done
 * before its passed to the NIC and progress might not be made elsewhere we cannot
 * allow a process to exit the barrier until its last [round of] sends are completed.
 *
 * It is last round of sends rather than 'last' individual send as each pair of peers can use different
 * channels/devices/btls and the receiver of one of these sends might be forced to wait as the sender
 * leaves the collective and does not make progress until the next mpi call 
 *
 */


/*
 * Simple double ring version of barrier
 *
 * synchronous gurantee made by last ring of sends are synchronous
 *
 */
int ompi_coll_tuned_barrier_intra_doublering(struct ompi_communicator_t *comm)
{
    int rank, size;
    int err=0, line=0;
    int left, right;


    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_barrier_intra_doublering rank %d", rank));
  
    left = ((rank-1)%size);
    right = ((rank+1)%size);

    if (rank > 0) { /* receive message from the left */
        err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, left, 
                              MCA_COLL_BASE_TAG_BARRIER, comm, MPI_STATUS_IGNORE));
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
    }

    /* Send message to the right */
    err = MCA_PML_CALL(send((void*)NULL, 0, MPI_BYTE, right, MCA_COLL_BASE_TAG_BARRIER, 
                            MCA_PML_BASE_SEND_STANDARD, comm));
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

    /* root needs to receive from the last node */
    if (rank == 0) {
        err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, left, 
                              MCA_COLL_BASE_TAG_BARRIER, comm, MPI_STATUS_IGNORE));
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
    }

    /* Allow nodes to exit */
    if (rank > 0) { /* post Receive from left */
        err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, left, 
                              MCA_COLL_BASE_TAG_BARRIER, comm, MPI_STATUS_IGNORE));
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
    }

    /* send message to the right one */
    err = MCA_PML_CALL(send((void*)NULL, 0, MPI_BYTE, right, MCA_COLL_BASE_TAG_BARRIER, 
                            MCA_PML_BASE_SEND_SYNCHRONOUS, comm));
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
 
    /* rank 0 post receive from the last node */
    if (rank == 0) {
        err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, left, 
                              MCA_COLL_BASE_TAG_BARRIER, comm, MPI_STATUS_IGNORE));
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
    }

    return MPI_SUCCESS;

 err_hndl:
     OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,line,err,rank));
   return err;
}


/*
 * To make synchronous, uses sync sends and sync sendrecvs
 */

int ompi_coll_tuned_barrier_intra_recursivedoubling(struct ompi_communicator_t *comm)
{
    int rank, size, adjsize;
    int i, err, line;
    int mask, remote;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_barrier_intra_recursivedoubling rank %d", rank));

    /* do nearest power of 2 less than size calc */
    adjsize = 1;
    for(i=0;adjsize*2<size;adjsize*=2) { }

    /* if size is not exact power of two, perform an extra step */
    if (adjsize != size) {
        if (rank >= adjsize) {
            /* send message to lower ranked node */
            err = MCA_PML_CALL(send((void*)NULL, 0, MPI_BYTE, rank-adjsize, 
                            MCA_COLL_BASE_TAG_BARRIER, MCA_PML_BASE_SEND_STANDARD, comm));

            if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}

            /* post receive from lower ranked node */
            err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, rank-adjsize,
                              MCA_COLL_BASE_TAG_BARRIER, comm, MPI_STATUS_IGNORE));

            if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}

        } else if (rank < (size - adjsize)) {

            /* receive message from high level rank */
            err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, rank+adjsize,
                              MCA_COLL_BASE_TAG_BARRIER, comm, MPI_STATUS_IGNORE));

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

            err = ompi_coll_tuned_sendrecv_localcompleted (NULL, 0, MPI_BYTE, remote, MCA_COLL_BASE_TAG_BARRIER, 
                                   NULL, 0, MPI_BYTE, remote, MCA_COLL_BASE_TAG_BARRIER,
                                   comm, MPI_STATUS_IGNORE, rank);

            if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}
        }
    }

    /* non-power of 2 case */
    if (adjsize != size) {
        if (rank < (size - adjsize)) {

            /* send enter message to higher ranked node */
            err = MCA_PML_CALL(send((void*)NULL, 0, MPI_BYTE, rank+adjsize, 
                            MCA_COLL_BASE_TAG_BARRIER, MCA_PML_BASE_SEND_SYNCHRONOUS, comm));

            if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}
        }
    }

    return MPI_SUCCESS;

    err_hndl:
     OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,line,err,rank));
        return err;
}


/*
 * To make synchronous, uses sync sends and sync sendrecvs
 */

int ompi_coll_tuned_barrier_intra_bruck(struct ompi_communicator_t *comm)
{
    int rank, size;
    int distance, to, from;
    int err, line = 0;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_barrier_intra_bruck rank %d", rank));

    /* exchange data with rank-2^k and rank+2^k */
    for (distance = 1; distance < size; distance <<= 1) { 
        from = (rank + size - distance)%size;
        to   = (rank + distance)%size;
        err = ompi_coll_tuned_sendrecv_localcompleted (NULL, 0, MPI_BYTE, to, MCA_COLL_BASE_TAG_BARRIER,
                                    NULL, 0, MPI_BYTE, from, MCA_COLL_BASE_TAG_BARRIER,
                                    comm, MPI_STATUS_IGNORE, rank);
       if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}
    }

    return MPI_SUCCESS;

    err_hndl:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,line,err,rank));
        return err;
}


/*
 * To make synchronous, uses sync sends and sync sendrecvs
 */
/* special case for two processes */
int ompi_coll_tuned_barrier_intra_two_procs(struct ompi_communicator_t *comm)
{
    int rank;
    int err=0;

    rank = ompi_comm_rank(comm);
    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_barrier_intra_two_procs rank %d", rank));

    if (0==rank) {
        err = ompi_coll_tuned_sendrecv_localcompleted (NULL, 0, MPI_BYTE, 1, MCA_COLL_BASE_TAG_BARRIER, 
                                   NULL, 0, MPI_BYTE, 1, MCA_COLL_BASE_TAG_BARRIER,
                                   comm, MPI_STATUS_IGNORE, rank);
    }
    else {
        err = ompi_coll_tuned_sendrecv_localcompleted (NULL, 0, MPI_BYTE, 0, MCA_COLL_BASE_TAG_BARRIER,
                                   NULL, 0, MPI_BYTE, 0, MCA_COLL_BASE_TAG_BARRIER,
                                   comm, MPI_STATUS_IGNORE, rank);
    }

    return (err);
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

static int ompi_coll_tuned_barrier_intra_basic_linear(struct ompi_communicator_t *comm)
{
    int i;
    int err;
    int size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);

    /* All non-root send & receive zero-length message. */

    if (rank > 0) {
        err =
            MCA_PML_CALL(send
                         (NULL, 0, MPI_BYTE, 0, MCA_COLL_BASE_TAG_BARRIER,
                          MCA_PML_BASE_SEND_STANDARD, comm));
        if (MPI_SUCCESS != err) {
            return err;
        }

        err =
            MCA_PML_CALL(recv
                         (NULL, 0, MPI_BYTE, 0, MCA_COLL_BASE_TAG_BARRIER,
                          comm, MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* The root collects and broadcasts the messages. */

    else {
        for (i = 1; i < size; ++i) {
            err = MCA_PML_CALL(recv(NULL, 0, MPI_BYTE, MPI_ANY_SOURCE,
                                    MCA_COLL_BASE_TAG_BARRIER,
                                    comm, MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != err) {
                return err;
            }
        }


        for (i = 1; i < size; ++i) {
            err =
                MCA_PML_CALL(send
                             (NULL, 0, MPI_BYTE, i,
                              MCA_COLL_BASE_TAG_BARRIER,
                              MCA_PML_BASE_SEND_STANDARD, comm));
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
    }

    /* All done */

    return MPI_SUCCESS;

}
/* copied function (with appropriate renaming) ends here */


/* The following are used by dynamic and forced rules */

/* publish details of each algorithm and if its forced/fixed/locked in */
/* as you add methods/algorithms you must update this and the query/map routines */

int ompi_coll_tuned_barrier_intra_check_forced ( )
{

mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "barrier_algorithm",
                           "Which barrier algorithm is used. Can be locked down to choice of: 0 ignore, 1 linear, 2 double ring, 3: recursive doubling 4: bruck, 5: two proc only, 6: step based bmtree",
                           false, false, ompi_coll_tuned_barrier_forced_choice,
                           &ompi_coll_tuned_barrier_forced_choice);

return (MPI_SUCCESS);
}



int ompi_coll_tuned_barrier_intra_query ( )
{
    return (5); /* 4 algorithms available */ 
                /* 2 to do */
}


int ompi_coll_tuned_barrier_intra_do_forced(struct ompi_communicator_t *comm)
{
   OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:barrier_intra_do_forced selected algorithm %d", ompi_coll_tuned_barrier_forced_choice));

switch (ompi_coll_tuned_barrier_forced_choice) {
    case (0):   return ompi_coll_tuned_barrier_intra_dec_fixed (comm);
    case (1):   return ompi_coll_tuned_barrier_intra_basic_linear (comm); 
    case (2):   return ompi_coll_tuned_barrier_intra_doublering (comm);
    case (3):   return ompi_coll_tuned_barrier_intra_recursivedoubling (comm);
    case (4):   return ompi_coll_tuned_barrier_intra_bruck (comm);
    case (5):   return ompi_coll_tuned_barrier_intra_two_procs (comm);
/*     case (6):   return ompi_coll_tuned_barrier_intra_bmtree_step (comm); */
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:barrier_intra_do_forced attempt to select algorithm %d when only 0-%d is valid?",
                    ompi_coll_tuned_barrier_forced_choice, ompi_coll_tuned_barrier_intra_query()));
        return (MPI_ERR_ARG);
    } /* switch */

}


int ompi_coll_tuned_barrier_intra_do_this (struct ompi_communicator_t *comm, int choice, int faninout, int segsize)
{
   OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:barrier_intra_do_this selected algorithm %d topo fanin/out%d", choice, faninout));

switch (choice) {
    case (0):   return ompi_coll_tuned_barrier_intra_dec_fixed (comm);
    case (1):   return ompi_coll_tuned_barrier_intra_basic_linear (comm); 
    case (2):   return ompi_coll_tuned_barrier_intra_doublering (comm);
    case (3):   return ompi_coll_tuned_barrier_intra_recursivedoubling (comm);
    case (4):   return ompi_coll_tuned_barrier_intra_bruck (comm);
    case (5):   return ompi_coll_tuned_barrier_intra_two_procs (comm);
/*     case (6):   return ompi_coll_tuned_barrier_intra_bmtree_step (comm); */
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:barrier_intra_do_this attempt to select algorithm %d when only 0-%d is valid?",
                    choice, ompi_coll_tuned_barrier_intra_query()));
        return (MPI_ERR_ARG);
    } /* switch */

}

