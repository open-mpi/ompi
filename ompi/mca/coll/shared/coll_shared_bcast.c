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

#include "coll_shared.h"

int mca_coll_shared_bcast_intra(void *buff, int count,
                                struct ompi_datatype_t *dtype, int root,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t * module)
{
    ptrdiff_t extent, lower_bound;
    ompi_datatype_get_extent(dtype, &lower_bound, &extent);
    if (count * extent <= 2048) {
        ompi_coll_base_bcast_intra_binomial(buff, count, dtype, root, comm,
                                            module, 2048);
    } else {
        mca_coll_shared_bcast_linear_intra(buff, count, dtype, root, comm,
                                           module);
    }
    return OMPI_SUCCESS;
}

int mca_coll_shared_bcast_linear_intra(void *buff, int count,
                                       struct ompi_datatype_t *dtype,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t * module)
{
    mca_coll_shared_module_t *shared_module =
        (mca_coll_shared_module_t *) module;
    ptrdiff_t extent, lower_bound;
    ompi_datatype_get_extent(dtype, &lower_bound, &extent);
    /* Enable shared module if necessary */
    if (!shared_module->enabled) {
        mca_coll_shared_lazy_enable(module, comm, count * extent);
    } else if ((size_t) count * extent > shared_module->data_buf_size) {
        mca_coll_shared_attach_data_buf(shared_module, comm,
                                        count * extent);
    }

    int rank = ompi_comm_rank(comm);
    /* Root copy to shared memory */
    shared_module->sm_data_win->w_osc_module->osc_fence(0,
                                                        shared_module->sm_data_win);
    if (rank == root) {
        shared_module->sm_data_win->w_osc_module->osc_put(buff, count,
                                                          dtype, root,
                                                          (ptrdiff_t)
                                                          shared_module->data_buf
                                                          [root], count,
                                                          dtype,
                                                          shared_module->sm_data_win);
    }
    shared_module->sm_data_win->w_osc_module->osc_fence(0,
                                                        shared_module->sm_data_win);

    /* Other processes copy data from shared memory */
    if (rank != root) {
        shared_module->sm_data_win->w_osc_module->osc_get(buff, count,
                                                          dtype, root,
                                                          (ptrdiff_t)
                                                          shared_module->data_buf
                                                          [root], count,
                                                          dtype,
                                                          shared_module->sm_data_win);
    }
    shared_module->sm_data_win->w_osc_module->osc_fence(0,
                                                        shared_module->sm_data_win);
    return OMPI_SUCCESS;
}
