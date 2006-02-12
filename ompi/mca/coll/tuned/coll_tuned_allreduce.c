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

/*
 * ompi_coll_tuned_allreduce_intra_nonoverlapping
 *
 * This function just calls a reduce followed by a broadcast
 * both called functions are tuned but they complete sequentially,
 * i.e. no additional overlapping
 * meaning if the number of segments used is greater than the topo depth 
 * then once the first segment of data is fully 'reduced' it is not broadcast
 * while the reduce continues (cost = cost-reduce + cost-bcast + decision x 3)
 *
 */
int
ompi_coll_tuned_allreduce_intra_nonoverlapping(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               struct ompi_communicator_t *comm)
{
    int err;
    int rank;

    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:allreduce_intra_nonoverlapping rank %d", rank));

    /* Reduce to 0 and broadcast. */

    if (MPI_IN_PLACE == sbuf) {
        if (0 == ompi_comm_rank(comm)) {
            err = comm->c_coll.coll_reduce (MPI_IN_PLACE, rbuf, count, dtype, op, 0, comm);
        } else {
            err = comm->c_coll.coll_reduce (rbuf, NULL, count, dtype, op, 0, comm);
        }
    } else {
        err = comm->c_coll.coll_reduce (sbuf, rbuf, count, dtype, op, 0, comm);
    }
    if (MPI_SUCCESS != err) {
        return err;
    }

    return comm->c_coll.coll_bcast (rbuf, count, dtype, 0, comm);
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


/*
 *	allreduce_intra
 *
 *	Function:	- allreduce using other MPI collectives
 *	Accepts:	- same as MPI_Allreduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
ompi_coll_tuned_allreduce_intra_basic_linear(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               struct ompi_communicator_t *comm)
{
    int err;
    int rank;

    rank = ompi_comm_rank(comm);


    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:allreduce_intra_basic_linear rank %d", rank));

    /* Reduce to 0 and broadcast. */

    if (MPI_IN_PLACE == sbuf) {
        if (0 == ompi_comm_rank(comm)) {
            err = ompi_coll_tuned_reduce_intra_basic_linear (MPI_IN_PLACE, rbuf, count, dtype, op, 0, comm);
        } else {
            err = ompi_coll_tuned_reduce_intra_basic_linear(rbuf, NULL, count, dtype, op, 0, comm);
        }
    } else {
        err = ompi_coll_tuned_reduce_intra_basic_linear(sbuf, rbuf, count, dtype, op, 0, comm);
    }
    if (MPI_SUCCESS != err) {
        return err;
    }

    return ompi_coll_tuned_bcast_intra_basic_linear(rbuf, count, dtype, 0, comm);
}


/* copied function (with appropriate renaming) ends here */

/* The following are used by dynamic and forced rules */

/* publish details of each algorithm and if its forced/fixed/locked in */
/* as you add methods/algorithms you must update this and the query/map routines */

int ompi_coll_tuned_allreduce_intra_check_forced ( )
{

mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "allreduce_algorithm",
                           "Which allreduce algorithm is used. Can be locked down to choice of: 0 ignore, 1 basic linear, 2 nonoverlapping (tuned reduce + tuned bcast)",
                           false, false, ompi_coll_tuned_allreduce_forced_choice,
                           &ompi_coll_tuned_allreduce_forced_choice);

mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "allreduce_algorithm_segmentsize",
                           "Segment size in bytes used by default for allreduce algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation.",
                           false, false, ompi_coll_tuned_allreduce_forced_segsize,
                           &ompi_coll_tuned_allreduce_forced_segsize);

mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "allreduce_algorithm_tree_fanout",
                           "Fanout for n-tree used for allreduce algorithms. Only has meaning if algorithm is forced and supports n-tree topo based operation.",
                           false, false,
                           ompi_coll_tuned_init_tree_fanout, /* get system wide default */
                           &ompi_coll_tuned_allreduce_forced_tree_fanout);

mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "allreduce_algorithm_chain_fanout",
                           "Fanout for chains used for allreduce algorithms. Only has meaning if algorithm is forced and supports chain topo based operation.",
                           false, false,
                           ompi_coll_tuned_init_chain_fanout, /* get system wide default */
                           &ompi_coll_tuned_allreduce_forced_chain_fanout);

return (MPI_SUCCESS);
}

int ompi_coll_tuned_allreduce_intra_query ( )
{
    return (2); /* 2 algorithms available */
}


int ompi_coll_tuned_allreduce_intra_do_forced(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               struct ompi_communicator_t *comm)
{
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:allreduce_intra_do_forced selected algorithm %d", 
                               ompi_coll_tuned_allreduce_forced_choice));

switch (ompi_coll_tuned_allreduce_forced_choice) {
    case (0):   return ompi_coll_tuned_allreduce_intra_dec_fixed (sbuf, rbuf, count, dtype, op, comm);
    case (1):   return ompi_coll_tuned_allreduce_intra_basic_linear (sbuf, rbuf, count, dtype, op, comm);
    case (2):   return ompi_coll_tuned_allreduce_intra_nonoverlapping (sbuf, rbuf, count, dtype, op, comm);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:allreduce_intra_do_forced attempt to select algorithm %d when only 0-%d is valid?",
                    ompi_coll_tuned_allreduce_forced_choice, ompi_coll_tuned_allreduce_intra_query()));
        return (MPI_ERR_ARG);
    } /* switch */

}


int ompi_coll_tuned_allreduce_intra_do_this(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               struct ompi_communicator_t *comm,
                               int choice, int faninout, int segsize)
{
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:allreduce_intra_do_this algorithm %d topo fan in/out %d segsize %d", 
                            choice, faninout, segsize));

switch (choice) {
    case (0):   return ompi_coll_tuned_allreduce_intra_dec_fixed (sbuf, rbuf, count, dtype, op, comm);
    case (1):   return ompi_coll_tuned_allreduce_intra_basic_linear (sbuf, rbuf, count, dtype, op, comm);
    case (2):   return ompi_coll_tuned_allreduce_intra_nonoverlapping (sbuf, rbuf, count, dtype, op, comm);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:allreduce_intra_do_this attempt to select algorithm %d when only 0-%d is valid?",
                    choice, ompi_coll_tuned_allreduce_intra_query()));
        return (MPI_ERR_ARG);
    } /* switch */

}



