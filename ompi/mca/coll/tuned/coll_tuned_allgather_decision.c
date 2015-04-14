/*
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_topo.h"
#include "ompi/mca/coll/base/coll_base_util.h"
#include "coll_tuned.h"

/* allgather algorithm variables */
static int coll_tuned_allgather_forced_algorithm = 0;
static int coll_tuned_allgather_segment_size = 0;
static int coll_tuned_allgather_tree_fanout;
static int coll_tuned_allgather_chain_fanout;

/* valid values for coll_tuned_allgather_forced_algorithm */
static mca_base_var_enum_value_t allgather_algorithms[] = {
    {0, "ignore"},
    {1, "linear"},
    {2, "bruck"},
    {3, "recursive_doubling"},
    {4, "ring"},
    {5, "neighbor"},
    {6, "two_proc"},
    {0, NULL}
};

/* The following are used by dynamic and forced rules */

/* publish details of each algorithm and if its forced/fixed/locked in */
/* as you add methods/algorithms you must update this and the query/map
   routines */

/* this routine is called by the component only */
/* this makes sure that the mca parameters are set to their initial values
   and perms */
/* module does not call this they call the forced_getvalues routine instead */

int
ompi_coll_tuned_allgather_intra_check_forced_init(coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices)
{
    mca_base_var_enum_t *new_enum;
    int cnt;

    for( cnt = 0; NULL != allgather_algorithms[cnt].string; cnt++ );
    ompi_coll_tuned_forced_max_algorithms[ALLGATHER] = cnt;

    (void) mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                           "allgather_algorithm_count",
                                           "Number of allgather algorithms available",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_CONSTANT,
                                           &cnt);

    /* MPI_T: This variable should eventually be bound to a communicator */
    coll_tuned_allgather_forced_algorithm = 0;
    (void) mca_base_var_enum_create("coll_tuned_allgather_algorithms", allgather_algorithms, &new_enum);
    mca_param_indices->algorithm_param_index =
        mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                        "allgather_algorithm",
                                        "Which allallgather algorithm is used. Can be locked down to choice of: 0 ignore, 1 basic linear, 2 bruck, 3 recursive doubling, 4 ring, 5 neighbor exchange, 6: two proc only.",
                                        MCA_BASE_VAR_TYPE_INT, new_enum, 0, 0,
                                        OPAL_INFO_LVL_5,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &coll_tuned_allgather_forced_algorithm);
    OBJ_RELEASE(new_enum);
    if (mca_param_indices->algorithm_param_index < 0) {
        return mca_param_indices->algorithm_param_index;
    }

    coll_tuned_allgather_segment_size = 0;
    mca_param_indices->segsize_param_index =
        mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                        "allgather_algorithm_segmentsize",
                                        "Segment size in bytes used by default for allgather algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation. Currently, available algorithms do not support segmentation.",
                                        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                        OPAL_INFO_LVL_5,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &coll_tuned_allgather_segment_size);

    coll_tuned_allgather_tree_fanout = ompi_coll_tuned_init_tree_fanout; /* get system wide default */
    mca_param_indices->tree_fanout_param_index =
        mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                        "allgather_algorithm_tree_fanout",
                                        "Fanout for n-tree used for allgather algorithms. Only has meaning if algorithm is forced and supports n-tree topo based operation. Currently, available algorithms do not support n-tree topologies.",
                                        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                        OPAL_INFO_LVL_5,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &coll_tuned_allgather_tree_fanout);

    coll_tuned_allgather_chain_fanout = ompi_coll_tuned_init_chain_fanout; /* get system wide default */
    mca_param_indices->chain_fanout_param_index =
      mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                      "allgather_algorithm_chain_fanout",
                                      "Fanout for chains used for allgather algorithms. Only has meaning if algorithm is forced and supports chain topo based operation. Currently, available algorithms do not support chain topologies.",
                                      MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                      OPAL_INFO_LVL_5,
                                      MCA_BASE_VAR_SCOPE_READONLY,
                                      &coll_tuned_allgather_chain_fanout);

    return (MPI_SUCCESS);
}

int ompi_coll_tuned_allgather_intra_do_forced(void *sbuf, int scount,
                                              struct ompi_datatype_t *sdtype,
                                              void* rbuf, int rcount,
                                              struct ompi_datatype_t *rdtype,
                                              struct ompi_communicator_t *comm,
                                              mca_coll_base_module_t *module)
{
    mca_coll_tuned_module_t *tuned_module = (mca_coll_tuned_module_t*) module;

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:allgather_intra_do_forced selected algorithm %d",
                 tuned_module->user_forced[ALLGATHER].algorithm));

    switch (tuned_module->user_forced[ALLGATHER].algorithm) {
    case (0):
        return ompi_coll_tuned_allgather_intra_dec_fixed(sbuf, scount, sdtype,
                                                         rbuf, rcount, rdtype,
                                                         comm, module);
    case (1):
        return ompi_coll_base_allgather_intra_basic_linear(sbuf, scount, sdtype,
                                                           rbuf, rcount, rdtype,
                                                           comm, module);
    case (2):
        return ompi_coll_base_allgather_intra_bruck(sbuf, scount, sdtype,
                                                    rbuf, rcount, rdtype,
                                                    comm, module);
    case (3):
        return ompi_coll_base_allgather_intra_recursivedoubling(sbuf, scount, sdtype,
                                                                rbuf, rcount, rdtype,
                                                                comm, module);
    case (4):
        return ompi_coll_base_allgather_intra_ring(sbuf, scount, sdtype,
                                                   rbuf, rcount, rdtype,
                                                   comm, module);
    case (5):
        return ompi_coll_base_allgather_intra_neighborexchange(sbuf, scount, sdtype,
                                                               rbuf, rcount, rdtype,
                                                               comm, module);
    case (6):
        return ompi_coll_base_allgather_intra_two_procs(sbuf, scount, sdtype,
                                                        rbuf, rcount, rdtype,
                                                        comm, module);
    } /* switch */
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:allgather_intra_do_forced attempt to select algorithm %d when only 0-%d is valid?",
                 tuned_module->user_forced[ALLGATHER].algorithm,
                 ompi_coll_tuned_forced_max_algorithms[ALLGATHER]));
    return (MPI_ERR_ARG);
}


int ompi_coll_tuned_allgather_intra_do_this(void *sbuf, int scount,
                                            struct ompi_datatype_t *sdtype,
                                            void* rbuf, int rcount,
                                            struct ompi_datatype_t *rdtype,
                                            struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module,
                                            int algorithm, int faninout, int segsize)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:allgather_intra_do_this selected algorithm %d topo faninout %d segsize %d",
                 algorithm, faninout, segsize));

    switch (algorithm) {
    case (0):
        return ompi_coll_tuned_allgather_intra_dec_fixed(sbuf, scount, sdtype,
                                                         rbuf, rcount, rdtype,
                                                         comm, module);
    case (1):
        return ompi_coll_base_allgather_intra_basic_linear(sbuf, scount, sdtype,
                                                           rbuf, rcount, rdtype,
                                                           comm, module);
    case (2):
        return ompi_coll_base_allgather_intra_bruck(sbuf, scount, sdtype,
                                                    rbuf, rcount, rdtype,
                                                    comm, module);
    case (3):
        return ompi_coll_base_allgather_intra_recursivedoubling(sbuf, scount, sdtype,
                                                                rbuf, rcount, rdtype,
                                                                comm, module);
    case (4):
        return ompi_coll_base_allgather_intra_ring(sbuf, scount, sdtype,
                                                   rbuf, rcount, rdtype,
                                                   comm, module);
    case (5):
        return ompi_coll_base_allgather_intra_neighborexchange(sbuf, scount, sdtype,
                                                               rbuf, rcount, rdtype,
                                                               comm, module);
    case (6):
        return ompi_coll_base_allgather_intra_two_procs(sbuf, scount, sdtype,
                                                        rbuf, rcount, rdtype,
                                                        comm, module);
    } /* switch */
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:allgather_intra_do_this attempt to select algorithm %d when only 0-%d is valid?",
                 algorithm, ompi_coll_tuned_forced_max_algorithms[ALLGATHER]));
    return (MPI_ERR_ARG);
}
