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
#include "include/constants.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "coll_tuned.h"
#include "mca/pml/pml.h"
#include "opal/util/bit_ops.h"

#include "coll_tuned.h"

/*
 *  allreduce_intra
 *
 *  Function:   - allreduce using other MPI collectives
 *  Accepts:    - same as MPI_Allreduce()
 *  Returns:    - MPI_SUCCESS or error code
 */
int
mca_coll_tuned_allreduce_intra_dec_dynamic (void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               struct ompi_communicator_t *comm)
{
    int i;
    int size;
    int rank;
    int err;
    int contig;
    int dsize;

    OPAL_OUTPUT((mca_coll_tuned_stream, "mca_coll_tuned_allreduce_intra_dec_dynamic"));

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    if (mca_coll_tuned_allreduce_forced_choice) {
        return mca_coll_tuned_allreduce_intra_do_forced (sbuf, rbuf, count, dtype, op, comm);
    }
    else {
        return mca_coll_tuned_allreduce_intra_dec_fixed (sbuf, rbuf, count, dtype, op, comm);
    }
}

/*
 *	alltoall_intra_dec 
 *
 *	Function:	- seletects alltoall algorithm to use
 *	Accepts:	- same arguments as MPI_Alltoall()
 *	Returns:	- MPI_SUCCESS or error code (passed from the bcast implementation)
 */

int mca_coll_tuned_alltoall_intra_dec_dynamic(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype,
                                    void* rbuf, int rcount, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm)
{
    int i;
    int size;
    int rank;
    int err;
    int contig;
    int dsize;

    OPAL_OUTPUT((mca_coll_tuned_stream, "mca_coll_tuned_alltoall_intra_dec_dynamic"));

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    if (mca_coll_tuned_alltoall_forced_choice) {
        return mca_coll_tuned_alltoall_intra_do_forced (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    }
    else {
        return mca_coll_tuned_alltoall_intra_dec_fixed (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    }
}

/*
 *	barrier_intra_dec 
 *
 *	Function:	- seletects barrier algorithm to use
 *	Accepts:	- same arguments as MPI_Barrier()
 *	Returns:	- MPI_SUCCESS or error code (passed from the barrier implementation)
 */
int mca_coll_tuned_barrier_intra_dec_dynamic(struct ompi_communicator_t *comm)
{
    int i;
    int size;
    int rank;
    int err;
    int contig;
    int dsize;

    OPAL_OUTPUT((mca_coll_tuned_stream,"mca_coll_tuned_barrier_intra_dec_dynamic"));

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    if (mca_coll_tuned_barrier_forced_choice) {
       return mca_coll_tuned_barrier_intra_do_forced (comm);
    }
    else {
       return mca_coll_tuned_barrier_intra_dec_fixed (comm);
    }

}

/*
 *   bcast_intra_dec 
 *
 *   Function:   - seletects broadcast algorithm to use
 *   Accepts:   - same arguments as MPI_Bcast()
 *   Returns:   - MPI_SUCCESS or error code (passed from the bcast implementation)
 */
int mca_coll_tuned_bcast_intra_dec_dynamic(void *buff, int count,
                                   struct ompi_datatype_t *datatype, int root,
                                   struct ompi_communicator_t *comm)
{
    int i;
    int size;
    int rank;
    int err;
    int contig;
    int dsize;

    OPAL_OUTPUT((mca_coll_tuned_stream, "coll:tuned:bcast_intra_dec_dynamic"));

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    if (mca_coll_tuned_bcast_forced_choice) {
       return mca_coll_tuned_bcast_intra_do_forced (buff, count, datatype, root, comm);
    }
    else {
       return mca_coll_tuned_bcast_intra_dec_fixed (buff, count, datatype, root, comm);
    }

}

/*
 *	reduce_intra_dec 
 *
 *	Function:	- seletects reduce algorithm to use
 *	Accepts:	- same arguments as MPI_reduce()
 *	Returns:	- MPI_SUCCESS or error code (passed from the reduce implementation)
 *                                        
 */
int mca_coll_tuned_reduce_intra_dec_dynamic( void *sendbuf, void *recvbuf,
                                          int count, struct ompi_datatype_t* datatype,
                                          struct ompi_op_t* op, int root,
                                          struct ompi_communicator_t* comm)
{
    int i;
    int size;
    int rank;
    int err;
    int contig;
    int dsize;

    OPAL_OUTPUT((mca_coll_tuned_stream, "coll:tuned:reduce_intra_dec_dynamic"));

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    if (mca_coll_tuned_reduce_forced_choice) {
       return mca_coll_tuned_reduce_intra_do_forced (sendbuf, recvbuf, count, datatype, op, root, comm);
    }
    else {
       return mca_coll_tuned_reduce_intra_dec_fixed (sendbuf, recvbuf, count, datatype, op, root, comm);
    }

}

