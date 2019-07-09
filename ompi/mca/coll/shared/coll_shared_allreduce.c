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

int mca_coll_shared_allreduce_intra(const void *sbuf, void *rbuf,
                                    int count,
                                    struct ompi_datatype_t *dtype,
                                    struct ompi_op_t *op,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t * module)
{
    ptrdiff_t extent, lower_bound;
    ompi_datatype_get_extent(dtype, &lower_bound, &extent);
    if (count * extent <= 8 * 1024) {
        ompi_coll_base_allreduce_intra_recursivedoubling(sbuf, rbuf, count,
                                                         dtype, op, comm,
                                                         module);
    } else if (count * extent <= 128 * 1024) {
        ompi_coll_base_allreduce_intra_ring(sbuf, rbuf, count, dtype, op,
                                            comm, module);
    } else {
        mca_coll_shared_allreduce_ring_intra(sbuf, rbuf, count,
                                             dtype, op, comm, module);
    }

    return OMPI_SUCCESS;

}

/**
 * Each process operates a part of the shared data buffer in turn.
 * Suppose the number of processes is 4.
 * Step 1:
 * |  P0  |  P1  |  P2  |  P3  |
 * Step 2:
 * |  P1  |  P2  |  P3  |  P0  |
 * Step 3:
 * |  P2  |  P3  |  P0  |  P1  |
 * Step 4:
 * |  P3  |  P0  |  P1  |  P2  |
 * At last, every process copies data back from the shared data buffer.
 */
int mca_coll_shared_allreduce_ring_intra(const void *sbuf,
                                         void *rbuf, int count,
                                         struct ompi_datatype_t
                                         *dtype, struct ompi_op_t *op, struct ompi_communicator_t
                                         *comm,
                                         mca_coll_base_module_t * module)
{
    mca_coll_shared_module_t *shared_module =
        (mca_coll_shared_module_t *) module;
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);
    int i;
    ptrdiff_t extent, lower_bound;
    ompi_datatype_get_extent(dtype, &lower_bound, &extent);

    /* Enable shared module if necessary */
    if (!shared_module->enabled) {
        mca_coll_shared_lazy_enable(module, comm, count * extent);
    } else if ((size_t) count * extent > shared_module->data_buf_size) {
        mca_coll_shared_attach_data_buf(shared_module, comm,
                                        count * extent);
    }
    /* Set up segment count */
    int seg_count, l_seg_count;
    seg_count = count / size;
    l_seg_count = seg_count;
    if (rank == size - 1) {
        seg_count = count - rank * l_seg_count;
    }
    shared_module->ctrl_buf[rank][0] = rank;
    shared_module->sm_ctrl_win->w_osc_module->osc_fence(0,
                                                        shared_module->
                                                        sm_ctrl_win);
    int cur = rank;
    for (i = 0; i < size; i++) {
        if (cur != size - 1) {
            seg_count = l_seg_count;
        } else {
            seg_count = count - cur * l_seg_count;
        }
        /* Manually call opal_progress to prevent hanging */
        while (rank != shared_module->ctrl_buf[cur][0]) {
            opal_progress();
        }
        /* At first iteration, copy local data to the shared data buffer */
        if (cur == rank) {
            shared_module->sm_data_win->w_osc_module->osc_fence(0,
                                                                shared_module->sm_data_win);
            shared_module->sm_data_win->
                w_osc_module->osc_put((char *) sbuf +
                                      cur * l_seg_count * extent,
                                      seg_count, dtype, cur,
                                      (ptrdiff_t) shared_module->
                                      data_buf[cur], seg_count, dtype,
                                      shared_module->sm_data_win);
            shared_module->sm_data_win->w_osc_module->osc_fence(0,
                                                                shared_module->
                                                                sm_data_win);
        }
        /* For other iterations, do operations on the shared data buffer */
        else {
            shared_module->sm_data_win->
                w_osc_module->osc_accumulate((char *) sbuf +
                                             cur * l_seg_count * extent,
                                             seg_count, dtype, cur,
                                             (ptrdiff_t) shared_module->
                                             data_buf[cur], seg_count,
                                             dtype, op,
                                             shared_module->sm_data_win);
            shared_module->sm_data_win->w_osc_module->osc_fence(0,
                                                                shared_module->
                                                                sm_data_win);
        }
        cur = (cur - 1 + size) % size;
        shared_module->ctrl_buf[cur][0] =
            (shared_module->ctrl_buf[cur][0] + 1) % size;
        shared_module->sm_ctrl_win->w_osc_module->osc_fence(0,
                                                            shared_module->
                                                            sm_ctrl_win);
    }
    /* At last, every process copies data from the shared data buffer */
    char *c;
    c = rbuf;
    for (i = 0; i < size; i++) {
        if (i != size - 1) {
            seg_count = l_seg_count;
        } else {
            seg_count = count - i * l_seg_count;
        }
        shared_module->sm_data_win->w_osc_module->osc_get(c, seg_count,
                                                          dtype, i,
                                                          (ptrdiff_t)
                                                          shared_module->data_buf
                                                          [i], seg_count,
                                                          dtype,
                                                          shared_module->sm_data_win);
        c = c + seg_count * extent;
    }
    shared_module->sm_data_win->w_osc_module->osc_fence(0,
                                                        shared_module->
                                                        sm_data_win);
    return OMPI_SUCCESS;
}
