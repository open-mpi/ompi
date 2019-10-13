/**
 * Copyright (c) 2019      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "coll_solo.h"
int mac_coll_solo_barrier_intra(struct ompi_communicator_t *comm, mca_coll_base_module_t * module)
{
    mca_coll_solo_module_t *solo_module = (mca_coll_solo_module_t *) module;

    /* Enable solo module if necessary */
    if (!solo_module->enabled) {
        mca_coll_solo_lazy_enable(module, comm);
    }

    int rank = ompi_comm_rank(comm);
    /* Atomic add to current ctrl_buf */
    char *barrier_ctrl_bufs = solo_module->ctrl_bufs[0] + opal_cache_line_size;
    opal_atomic_add_fetch_32((opal_atomic_int32_t *) (barrier_ctrl_bufs + solo_module->barrier_tag * opal_cache_line_size), 1);
    while (*((int32_t *) (barrier_ctrl_bufs + (solo_module->barrier_tag) * opal_cache_line_size)) != ompi_comm_size(comm)) {
        opal_progress();
    }

    /* Set previous used ctrl_buf to 0 */
    if (rank == 0) {
        *((int32_t *) (barrier_ctrl_bufs + ((solo_module->barrier_tag + 2) % 3) * opal_cache_line_size)) = 0;
    }
    /* Set barrier_tag to next ctrl_buf */
    solo_module->barrier_tag = (solo_module->barrier_tag + 1) % 3;
    return OMPI_SUCCESS;
}
