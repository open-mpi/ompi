/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * This files contains all the hierarchical implementations of barrier
 */

#include "coll_han.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"


/**
 * Short implementation of barrier that only does hierarchical
 * communications without tasks.
 */
int
mca_coll_han_barrier_intra_simple(struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;
    ompi_communicator_t *low_comm, *up_comm;

    /* create the subcommunicators */
    if( OMPI_SUCCESS != mca_coll_han_comm_create_new(comm, han_module) ) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle barrier with this communicator. Fall back on another component\n"));
        /* Put back the fallback collective support and call it once. All
         * future calls will then be automatically redirected.
         */
        HAN_LOAD_FALLBACK_COLLECTIVES(han_module, comm);
        return comm->c_coll->coll_barrier(comm, comm->c_coll->coll_bcast_module);
    }

    low_comm = han_module->sub_comm[INTRA_NODE];
    up_comm = han_module->sub_comm[INTER_NODE];

    int low_rank = ompi_comm_rank(low_comm);
    int root_low_rank = 0; /* rank leader will be 0 on each node */

    /* TODO: extend coll interface with half barrier */
    low_comm->c_coll->coll_barrier(low_comm,low_comm->c_coll->coll_barrier_module);

    if (low_rank == root_low_rank) {
        up_comm->c_coll->coll_barrier(up_comm, up_comm->c_coll->coll_barrier_module);
    }

    low_comm->c_coll->coll_barrier(low_comm,low_comm->c_coll->coll_barrier_module);

    return OMPI_SUCCESS;
}
