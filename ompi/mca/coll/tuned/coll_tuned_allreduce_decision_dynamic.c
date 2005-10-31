/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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


