/*
 * Copyright (c) 2019      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* TODO: change ctrl_buf to cache line size*/
#include "coll_shared.h"
int mac_coll_shared_barrier_intra(struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t * module)
{
    mca_coll_shared_module_t *shared_module =
        (mca_coll_shared_module_t *) module;

    /* Enable shared module if necessary */
    if (!shared_module->enabled) {
        mca_coll_shared_lazy_enable(module, comm);
    }

    int rank = ompi_comm_rank(comm);
    /* Atomic add to current ctrl_buf */
    opal_atomic_add_fetch_32((opal_atomic_int32_t *)(shared_module->ctrl_buf[0] + shared_module->barrier_tag * opal_cache_line_size), 1);
    while (*((int *)(shared_module->ctrl_buf[0] + shared_module->barrier_tag * opal_cache_line_size)) != ompi_comm_size(comm)) {
        opal_progress();
    }

    /* Set previous used ctrl_buf to 0 */
    if (rank == 0) {
        *((int *)(shared_module->ctrl_buf[0] + ((shared_module->barrier_tag + 2) % 3) * opal_cache_line_size)) = 0;
    }
    /* Set barrier_tag to next ctrl_buf */
    shared_module->barrier_tag = (shared_module->barrier_tag + 1) % 3;
    return OMPI_SUCCESS;
}