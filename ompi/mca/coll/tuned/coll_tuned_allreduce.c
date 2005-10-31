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
#include "ompi/include/constants.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "mca/pml/pml.h"
#include "op/op.h"
#include "coll_tuned.h"
#include "coll_tuned_topo.h"

#include <sys/types.h>
#include <unistd.h>

/* temp debug routines */
static int dump_buf_int (char* ptr, int count, char *comment, int rank);

static int dump_buf_int (char* ptr, int count, char *comment, int rank) {
int i=0;
int *tptr;
int c=0;
tptr=(int*)ptr;
printf("%1d ", rank);
if (comment) printf("%s ", comment);
if (count <0) {
    printf("cnt %d?\n", count);
    return (0);
}

if (count>5) c = 5;
else c = count;
printf("Cnt %1d  ", count);
for(i=0;i<c;i++) {
    printf("%1d [%1d] ", i, *tptr++);
    }
if (c!=count) {
    tptr=(int*)ptr;
    printf(" ... %1d [%1d]", count-1, tptr[count-1]);
}
printf("\n");
return (0);
}

/*
 * mca_coll_tuned_allreduce_intra_nonoverlapping
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
mca_coll_tuned_allreduce_intra_nonoverlapping(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               struct ompi_communicator_t *comm)
{
    int err;
    int rank;

    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:allreduce_intra_nonoverlapping rank %d", rank));

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
mca_coll_tuned_allreduce_intra_basic_linear(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               struct ompi_communicator_t *comm)
{
    int err;
    int rank;

    rank = ompi_comm_rank(comm);


    OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:allreduce_intra_basic_linear rank %d", rank));

    /* Reduce to 0 and broadcast. */

    if (MPI_IN_PLACE == sbuf) {
        if (0 == ompi_comm_rank(comm)) {
            err = mca_coll_tuned_reduce_intra_basic_linear (MPI_IN_PLACE, rbuf, count, dtype, op, 0, comm);
        } else {
            err = mca_coll_tuned_reduce_intra_basic_linear(rbuf, NULL, count, dtype, op, 0, comm);
        }
    } else {
        err = mca_coll_tuned_reduce_intra_basic_linear(sbuf, rbuf, count, dtype, op, 0, comm);
    }
    if (MPI_SUCCESS != err) {
        return err;
    }

    return mca_coll_tuned_bcast_intra_basic_linear(rbuf, count, dtype, 0, comm);
}


/* copied function (with appropriate renaming) ends here */

/* The following are used by dynamic and forced rules */

/* publish details of each algorithm and if its forced/fixed/locked in */
/* as you add methods/algorithms you must update this and the query/map routines */

int mca_coll_tuned_allreduce_intra_check_forced ( )
{

mca_base_param_reg_int(&mca_coll_tuned_component.collm_version,
                           "allreduce_algorithm",
                           "Which allreduce algorithm is used. Can be locked down to choice of: 0 ignore, 1 basic linear, 2 nonoverlapping (tuned reduce + tuned bcast)",
                           false, false, mca_coll_tuned_allreduce_forced_choice,
                           &mca_coll_tuned_allreduce_forced_choice);

mca_base_param_reg_int(&mca_coll_tuned_component.collm_version,
                           "allreduce_algorithm_segmentsize",
                           "Segment size in bytes used by default for allreduce algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation.",
                           false, false, mca_coll_tuned_allreduce_forced_segsize,
                           &mca_coll_tuned_allreduce_forced_segsize);

mca_base_param_reg_int(&mca_coll_tuned_component.collm_version,
                           "allreduce_algorithm_tree_fanout",
                           "Fanout for n-tree used for allreduce algorithms. Only has meaning if algorithm is forced and supports n-tree topo based operation.",
                           false, false,
                           mca_coll_tuned_init_tree_fanout, /* get system wide default */
                           &mca_coll_tuned_allreduce_forced_tree_fanout);

mca_base_param_reg_int(&mca_coll_tuned_component.collm_version,
                           "allreduce_algorithm_chain_fanout",
                           "Fanout for chains used for allreduce algorithms. Only has meaning if algorithm is forced and supports chain topo based operation.",
                           false, false,
                           mca_coll_tuned_init_chain_fanout, /* get system wide default */
                           &mca_coll_tuned_allreduce_forced_chain_fanout);

return (MPI_SUCCESS);
}

int mca_coll_tuned_allreduce_intra_query ( )
{
    return (2); /* 2 algorithms available */
}


int mca_coll_tuned_allreduce_intra_do_forced(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               struct ompi_communicator_t *comm)
{
        OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:allreduce_intra_do_forced selected algorithm %d",
                    mca_coll_tuned_allreduce_forced_choice));

switch (mca_coll_tuned_allreduce_forced_choice) {
    case (0):   return mca_coll_tuned_allreduce_intra_dec_fixed (sbuf, rbuf, count, dtype, op, comm);
    case (1):   return mca_coll_tuned_allreduce_intra_basic_linear (sbuf, rbuf, count, dtype, op, comm);
    case (2):   return mca_coll_tuned_allreduce_intra_nonoverlapping (sbuf, rbuf, count, dtype, op, comm);
    default:
        OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:allreduce_intra_do_forced attempt to select algorithm %d when only 0-%d is valid?",
                    mca_coll_tuned_allreduce_forced_choice, mca_coll_tuned_allreduce_intra_query()));
        return (MPI_ERR_ARG);
    } /* switch */

}



