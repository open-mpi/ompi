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
#include "op/op.h"
#include "mca/pml/pml.h"
#include "opal/util/bit_ops.h"

#include "coll_tuned.h"


/*
 *	reduce_intra_dec 
 *
 *	Function:	- seletects reduce algorithm to use
 *	Accepts:	- same arguments as MPI_reduce()
 *	Returns:	- MPI_SUCCESS or error code (passed from the reduce implementation)
 *                                        
 */
int mca_coll_tuned_reduce_intra_dec_dynamic( void *sendbuf, void *recvbuf,
                                          int count, ompi_datatype_t* datatype,
                                          ompi_op_t* op, int root,
                                          ompi_communicator_t* comm)
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

