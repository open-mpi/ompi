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
    /*
    if (count * extent <= 2048) {
        ompi_coll_base_bcast_intra_binomial(buff, count, dtype, root, comm,
                                            module, 2048);
    } else {
        mca_coll_shared_bcast_linear_intra(buff, count, dtype, root, comm,
                                           module);
    } */
    mca_coll_shared_bcast_linear_intra_memcpy(buff, count, dtype, root, comm, module);
    return OMPI_SUCCESS;
}

int mca_coll_shared_bcast_linear_intra_memcpy(void *buff, int count,
                                       struct ompi_datatype_t *dtype,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t * module)
{
    mca_coll_shared_module_t *shared_module =
        (mca_coll_shared_module_t *) module;
    
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);
    //printf("[%d] bcast count %d size %d\n", rank, count, size);

    int id = 0;
    ptrdiff_t extent, lower_bound;
    ompi_datatype_get_extent(dtype, &lower_bound, &extent);
    /* Enable shared module if necessary */
    if (!shared_module->enabled) {
        mca_coll_shared_lazy_enable(module, comm);
    } 
    char *data_buf;
    if ((size_t) count * extent <= COLL_SHARED_STATIC_BLOCK_SIZE) {
        data_buf = shared_module->data_buf[root];
    }
    else if ((size_t) count * extent <= COLL_SHARED_LARGE_BLOCK_SIZE) {
        if (rank == root) {
            while (id == 0) {
                id = mca_coll_shared_mpool_request(mca_coll_shared_component.shared_mpool, count * extent);
            }
        }
        ompi_coll_base_bcast_intra_binomial(&id, 1, MPI_INT, root, comm, module, 2048);
        data_buf = mca_coll_shared_mpool_calculate(mca_coll_shared_component.shared_mpool, id, count * extent);
        //printf("[%d]: root = %d req id = %d data_buf = %p\n", rank, root, id, data_buf);
    }
    else {
        printf("TOO BIG\n");
    }
    
    //shared_module->dynamic_win->w_osc_module->osc_fence(0, shared_module->dynamic_win);
    if (rank == root) {
        memcpy(data_buf, (char*)buff, count * extent);
    }
    //shared_module->dynamic_win->w_osc_module->osc_fence(0, shared_module->dynamic_win);
    mac_coll_shared_barrier_intra(comm, module);
    //printf("[%d]: %c %c %c %c\n", rank, data_buf[0], data_buf[1], data_buf[2], data_buf[3]);
    if (rank != root) {
        memcpy((char*)buff, data_buf, count*extent);
    }
    //shared_module->dynamic_win->w_osc_module->osc_fence(0, shared_module->dynamic_win);
    mac_coll_shared_barrier_intra(comm, module);
    if ((size_t) count * extent > COLL_SHARED_STATIC_BLOCK_SIZE && 
        (size_t) count * extent <= COLL_SHARED_LARGE_BLOCK_SIZE) {
        if (rank == root) {
            mca_coll_shared_mpool_return(mca_coll_shared_component.shared_mpool, id, count * extent);
        }
        //printf("[%d]: root = %d ret id = %d data_buf = %p\n", rank, root, id, data_buf);
    }
    else {
        //printf("TOO BIG\n");
    }

    return OMPI_SUCCESS;
}

int mca_coll_shared_bcast_linear_intra_osc(void *buff, int count,
                                       struct ompi_datatype_t *dtype,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t * module)
{
    mca_coll_shared_module_t *shared_module =
        (mca_coll_shared_module_t *) module;
    MPI_Win cur_win;
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);
    //printf("[%d] bcast count %d size %d\n", rank, count, size);
    int id = 0;
    char **attached_bufs = NULL;
    ptrdiff_t extent, lower_bound;
    ompi_datatype_get_extent(dtype, &lower_bound, &extent);
    /* Enable shared module if necessary */
    if (!shared_module->enabled) {
        mca_coll_shared_lazy_enable(module, comm);
    } 
    char *data_buf;
    if ((size_t) count * extent <= COLL_SHARED_STATIC_BLOCK_SIZE) {
        data_buf = 3 * opal_cache_line_size;
        cur_win = shared_module->static_win;
    }
    else if ((size_t) count * extent <= COLL_SHARED_LARGE_BLOCK_SIZE) {
        if (rank == root) {
            while (id == 0) {
                id = mca_coll_shared_mpool_request(mca_coll_shared_component.shared_mpool, count * extent);
            }
            data_buf = mca_coll_shared_mpool_calculate(mca_coll_shared_component.shared_mpool, id, count * extent);
            attached_bufs = mca_coll_shared_attach_buf(shared_module, comm, data_buf, count * extent);
        }
        else {
            attached_bufs = mca_coll_shared_attach_buf(shared_module, comm, NULL, 0);
        }
        data_buf = attached_bufs[root];
        cur_win = shared_module->dynamic_win;
        //printf("[%d]: root = %d req id = %d data_buf = %p\n", rank, root, id, data_buf);
    }
    else {
        //printf("TOO BIG\n");
    }

    /* Root copy to shared memory */
    cur_win->w_osc_module->osc_fence(0, cur_win);
    if (rank == root) {
        cur_win->w_osc_module->osc_put(buff, count, dtype, root,
                                                          (ptrdiff_t)
                                                          data_buf, count,
                                                          dtype,
                                                          cur_win);
    }
    cur_win->w_osc_module->osc_fence(0, cur_win);
    /* Other processes copy data from shared memory */
    if (rank != root) {
        cur_win->w_osc_module->osc_get(buff, count, dtype, root,
                                                          (ptrdiff_t)
                                                          data_buf, count,
                                                          dtype,
                                                          cur_win);
    }
    cur_win->w_osc_module->osc_fence(0, cur_win);

    if ((size_t) count * extent > COLL_SHARED_STATIC_BLOCK_SIZE && 
        (size_t) count * extent <= COLL_SHARED_LARGE_BLOCK_SIZE) {
        if (rank == root) {
            mca_coll_shared_detach_buf(shared_module, comm, data_buf, &attached_bufs);
            mca_coll_shared_mpool_return(mca_coll_shared_component.shared_mpool, id, count * extent);
        }
        else {
            mca_coll_shared_detach_buf(shared_module, comm, NULL, &attached_bufs);
        }
        //printf("[%d]: root = %d ret id = %d data_buf = %p\n", rank, root, id, data_buf);
    }
    else {
        //printf("TOO BIG\n");
    }

    return OMPI_SUCCESS;
}