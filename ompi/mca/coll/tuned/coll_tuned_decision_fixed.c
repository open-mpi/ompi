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
    int comsize, rank, err;
    size_t dsize, total_dsize;

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_alltoall_intra_dec_fixed"));

    comsize = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* special case */
    if (comsize==2) {
        return ompi_coll_tuned_alltoall_intra_two_procs (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    }

    /* else we need data size for decision function */
    err = ompi_ddt_get_size (sdtype, &dsize);
    if (err != MPI_SUCCESS) { 
        OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,__LINE__,err,rank));
        return (err);
    }

    total_dsize = dsize * scount * comsize;   /* needed for decision */

    if (comsize >= 12 && total_dsize <= 768) {
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
    int comsize;

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_barrier_intra_dec_fixed"));

    comsize = ompi_comm_size(comm);

    if (2==comsize)
        return ompi_coll_tuned_barrier_intra_two_procs(comm);
    /*         return ompi_coll_tuned_barrier_intra_doublering(comm); */
    return ompi_coll_tuned_barrier_intra_recursivedoubling(comm);
    /*     return ompi_coll_tuned_barrier_intra_bruck(comm); */
    /*     return ompi_coll_tuned_barrier_intra_linear(comm); */

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
    int comsize, rank, err;
    int segsize = 0;
    size_t msgsize, dsize;

    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_bcast_intra_dec_fixed"));

    comsize = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* else we need data size for decision function */
    err = ompi_ddt_get_size (datatype, &dsize);
    if (err != MPI_SUCCESS) {
        OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,__LINE__,err,rank));
        return (err);
    }

    msgsize = dsize * (unsigned long)count;   /* needed for decision */

    /* this is based on gige measurements */

    if (comsize  < 4) {
        return ompi_coll_tuned_bcast_intra_basic_linear (buff, count, datatype, root, comm);
    }
    if (comsize == 4) {
        if (msgsize < 524288) segsize = 0;
        else segsize = 16384;
        return ompi_coll_tuned_bcast_intra_bintree (buff, count, datatype, root, comm, segsize);
    }
    if (comsize <= 8 && msgsize < 4096) {
        return ompi_coll_tuned_bcast_intra_basic_linear (buff, count, datatype, root, comm);
    }
    if (comsize > 8 && msgsize >= 32768 && msgsize < 524288) {
        segsize = 16384;
        return  ompi_coll_tuned_bcast_intra_bintree (buff, count, datatype, root, comm, segsize);
    }
    if (msgsize >= 524288) {
        segsize = 16384;
        return ompi_coll_tuned_bcast_intra_pipeline (buff, count, datatype, root, comm, segsize);
    }
    segsize = 0;
    /* once tested can swap this back in */
    /* return ompi_coll_tuned_bcast_intra_bmtree (buff, count, datatype, root, comm, segsize); */
    return ompi_coll_tuned_bcast_intra_bintree (buff, count, datatype, root, comm, segsize);
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
    int comsize, rank, err, segsize = 0, fanout = 0;
    size_t msgsize, dsize;

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_reduce_intra_dec_fixed"));

    comsize = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* need data size for decision function */
    err = ompi_ddt_get_size (datatype, &dsize);
    if (err != MPI_SUCCESS) {
        OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,__LINE__,err,rank));
        return (err);
    }

    msgsize = dsize * count;   /* needed for decision */

    /* for small messages use linear algorithm */
    if (msgsize <= 4096) {
        segsize = 0;
        fanout = comsize - 1;
        /* when linear implemented or taken from basic put here, right now using chain as a linear system */
        /* it is implemented and I shouldn't be calling a chain with a fanout bigger than MAXTREEFANOUT from topo.h! */
        return ompi_coll_tuned_reduce_intra_basic_linear (sendbuf, recvbuf, count, datatype, op, root, comm); 
        /*        return ompi_coll_tuned_reduce_intra_chain (sendbuf, recvbuf, count, datatype, op, root, comm, segsize, fanout); */
    }
    if (msgsize < 524288) {
        if (msgsize <= 65536 ) {
            segsize = 32768;
            fanout = 8;
        } else {
            segsize = 1024;
            fanout = comsize/2;
        }
        /* later swap this for a binary tree */
        /*         fanout = 2; */
        return ompi_coll_tuned_reduce_intra_chain (sendbuf, recvbuf, count, datatype, op, root, comm, segsize, fanout);
    }
    segsize = 1024;
    return ompi_coll_tuned_reduce_intra_pipeline (sendbuf, recvbuf, count, datatype, op, root, comm, segsize);
}
