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
#include "mca/pml/pml.h"
#include "opal/util/bit_ops.h"
#include "op/op.h"
#include "coll_tuned.h"


/*
 *	reduce_intra_dec 
 *
 *	Function:	- seletects reduce algorithm to use
 *	Accepts:	- same arguments as MPI_reduce()
 *	Returns:	- MPI_SUCCESS or error code (passed from the reduce implementation)
 *                                        
 */
int mca_coll_tuned_reduce_intra_dec_fixed( void *sendbuf, void *recvbuf,
                                          int count, ompi_datatype_t* datatype,
                                          ompi_op_t* op, int root,
                                          ompi_communicator_t* comm)
{
    int i;
    int size;
    int rank;
    int err;
    int contig;
    int msgsize;
    MPI_Aint ext;
    long lb;
    int segsize = 0;
    int fanout = 0;


    OPAL_OUTPUT((mca_coll_tuned_stream, "mca_coll_tuned_reduce_intra_dec_fixed"));

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* need data size for decision function */
    err = ompi_ddt_get_extent (datatype, &lb, &ext);
    if (err != MPI_SUCCESS) {
            OPAL_OUTPUT((mca_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,__LINE__,err,rank));
        return (err);
    }

    msgsize = ext * count;   /* needed for decision */
     /* for small messages use linear algorithm */
     if (msgsize <= 4096) {
        segsize = 0;
        fanout = size-1;
/* when linear implemented or taken from basic put here, right now using chain as a linear system */
/*         return mca_coll_tuned_reduce_intra_linear (sendbuf, recvbuf, count, datatype, op, root, comm); */
        return mca_coll_tuned_reduce_intra_chain (sendbuf, recvbuf, count, datatype, op, root, comm, segsize, fanout);
     } else if (msgsize <= 65536 ) {
        segsize = 32768;
        fanout = 8;
        return mca_coll_tuned_reduce_intra_chain (sendbuf, recvbuf, count, datatype, op, root, comm, segsize, fanout);
     } else if (msgsize < 524288) {
        segsize = 1024;
        fanout = size/2;
/* later swap this for a binary tree */
/*         fanout = 2; */
        return mca_coll_tuned_reduce_intra_chain (sendbuf, recvbuf, count, datatype, op, root, comm, segsize, fanout);
     } else {
        segsize = 1024;
        return mca_coll_tuned_reduce_intra_pipeline (sendbuf, recvbuf, count, datatype, op, root, comm, segsize);
     }

}

