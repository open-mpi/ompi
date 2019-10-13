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

static void mca_coll_solo_queue_construct(mca_coll_solo_queue_t * queue);
static void mca_coll_solo_queue_destruct(mca_coll_solo_queue_t * queue);

/* queue constructor */
static void mca_coll_solo_queue_construct(mca_coll_solo_queue_t * queue)
{
    return;
}

/* queue destructor */
static void mca_coll_solo_queue_destruct(mca_coll_solo_queue_t * queue)
{
    return;
}

OBJ_CLASS_INSTANCE(mca_coll_solo_queue_t, opal_object_t, mca_coll_solo_queue_construct, 
                   mca_coll_solo_queue_destruct);

/* Init the queue with node-wise communicator, number of blocks and size of each block. */
void mca_coll_solo_queue_init(mca_coll_solo_queue_t * queue, ompi_communicator_t * node_comm, 
                              int block_num, int block_size)
{
    int node_rank = ompi_comm_rank(node_comm);
    queue->block_size = block_size;
    queue->block_num = block_num;
    int *temp_ptr;
    int id_queue_size = opal_cache_line_size * (block_num + 3);
    if (node_rank == 0) {
        ompi_win_allocate_shared(block_size * block_num + id_queue_size, sizeof(char),
                                 (opal_info_t *) (&ompi_mpi_info_null),
                                 node_comm, &temp_ptr, &(queue->win));
    } else {
        ompi_win_allocate_shared(0, sizeof(char),
                                 (opal_info_t *) (&ompi_mpi_info_null),
                                 node_comm, &temp_ptr, &(queue->win));
    }
    size_t temp_size;
    int temp_disp;
    /* Get the address of the shared memory */
    queue->win->w_osc_module->osc_win_shared_query(queue->win, 0, &temp_size, &temp_disp, 
                                                   &queue->blocks);
    /* Set up the queue as shown in the coll_shared_mpool.h */
    queue->id_queue = queue->blocks + block_size * block_num;
    queue->head = queue->id_queue + opal_cache_line_size * (block_num + 1);
    queue->tail = queue->id_queue + opal_cache_line_size * (block_num + 2);
    queue->win->w_osc_module->osc_fence(0, queue->win);
    if (node_rank == 0) {
        (*((mca_coll_solo_tag_t *) queue->head)).id = 0;
        (*((mca_coll_solo_tag_t *) queue->head)).ref = 0;
        *((COLL_SOLO_WORD *) queue->tail) = block_num;
        int i;
        for (i = 0; i < block_num + 1; i++) {
            char *temp = queue->id_queue + opal_cache_line_size * i;
            *((COLL_SOLO_WORD *) temp) = i + 1;
            if (i == block_num) {
                *((COLL_SOLO_WORD *) temp) = 0;
            }
        }
    }
    queue->win->w_osc_module->osc_fence(0, queue->win);
    return;
}

/*
 * Request a block from the queue
 */
int mca_coll_solo_queue_request(mca_coll_solo_queue_t * queue)
{
    COLL_SOLO_DWORD cur_head, new_head;
    COLL_SOLO_WORD cur_tail;

    do {
        cur_head = *((COLL_SOLO_DWORD *) queue->head);
        cur_tail = *((COLL_SOLO_WORD *) queue->tail);
        if (((mca_coll_solo_tag_t *) &cur_head)->id == cur_tail) {
            return -1;
        }
        new_head = cur_head;
        ((mca_coll_solo_tag_t *) &new_head)->id = (((mca_coll_solo_tag_t *) &new_head)->id + 1) % 
                                                   (queue->block_num + 1);
        ((mca_coll_solo_tag_t *) &new_head)->ref = ((mca_coll_solo_tag_t *) &new_head)->ref + 1;
    } while (!opal_atomic_compare_exchange_strong_64((COLL_SOLO_DWORD *) queue->head, 
                                                     &cur_head, new_head));
    char *temp = queue->id_queue + opal_cache_line_size * ((mca_coll_solo_tag_t *) &cur_head)->id;
    COLL_SOLO_WORD id = *((COLL_SOLO_WORD *) temp);
    *((COLL_SOLO_WORD *) temp) = 0;
    return id;
}

/* 
 * Calculate block address based on block id
 */
char *mca_coll_solo_queue_calculate(mca_coll_solo_queue_t * queue, int id)
{
    return queue->blocks + queue->block_size * (id - 1);
}

/*
 * Return a block to the queue
 */
void mca_coll_solo_queue_return(mca_coll_solo_queue_t * queue, int id)
{
    COLL_SOLO_WORD cur_tail;
    char *temp;
    int32_t zero = 0;
    do {
        zero = 0;
        cur_tail = *((COLL_SOLO_WORD *) queue->tail);
        temp = queue->id_queue + opal_cache_line_size * cur_tail;
    } while (!opal_atomic_compare_exchange_strong_32((COLL_SOLO_WORD *) temp, &zero, id));
    opal_atomic_compare_exchange_strong_32((COLL_SOLO_WORD *) queue->tail, &cur_tail,
                                           (cur_tail + 1) % (queue->block_num + 1));
    return;
}


/* mpool classes */
static void mca_coll_solo_mpool_construct(mca_coll_solo_mpool_t * mpool);
static void mca_coll_solo_mpool_destruct(mca_coll_solo_mpool_t * mpool);

OBJ_CLASS_INSTANCE(mca_coll_solo_mpool_t, opal_object_t, mca_coll_solo_mpool_construct, 
                   mca_coll_solo_mpool_destruct);

/* mpool constructor */
static void mca_coll_solo_mpool_construct(mca_coll_solo_mpool_t * mpool)
{
    /* Create the node_comm which contains all the processes on a node */
    ompi_comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0,
                         (opal_info_t *) (&ompi_mpi_info_null), &(mpool->node_comm));
    int node_size = ompi_comm_size(mpool->node_comm);
    /* Create the queues */
    mpool->small_queue = OBJ_NEW(mca_coll_solo_queue_t);
    mpool->large_queue = OBJ_NEW(mca_coll_solo_queue_t);
    /* verify the mca parameters */
    if (mca_coll_solo_component.mpool_small_block_size > 
        mca_coll_solo_component.mpool_large_block_size) {
        uint32_t temp = mca_coll_solo_component.mpool_small_block_size;
        mca_coll_solo_component.mpool_small_block_size = 
            mca_coll_solo_component.mpool_large_block_size;
        mca_coll_solo_component.mpool_large_block_size = temp;
    }
    if (mca_coll_solo_component.mpool_small_block_num < (uint32_t) node_size) {
        if (mca_coll_solo_component.mpool_small_block_num == 0) {
            mca_coll_solo_component.mpool_small_block_num = node_size * 4;
        }
        else {
            mca_coll_solo_component.mpool_small_block_num = node_size;
        }
    }
    if (mca_coll_solo_component.mpool_large_block_num < (uint32_t) node_size) {
        if (mca_coll_solo_component.mpool_large_block_num == 0) {
            mca_coll_solo_component.mpool_large_block_num = node_size * 2;
        }
        else {
            mca_coll_solo_component.mpool_large_block_num = node_size;
        }
    }
    /* Init the queues */
    mca_coll_solo_queue_init(mpool->small_queue, mpool->node_comm,
                             mca_coll_solo_component.mpool_small_block_num, 
                             mca_coll_solo_component.mpool_small_block_size);
    mca_coll_solo_queue_init(mpool->large_queue, mpool->node_comm,
                             mca_coll_solo_component.mpool_large_block_num,
                             mca_coll_solo_component.mpool_large_block_size);
    return;
}

/* mpool destructor */
static void mca_coll_solo_mpool_destruct(mca_coll_solo_mpool_t * mpool)
{
    OBJ_RELEASE(mpool->small_queue);
    OBJ_RELEASE(mpool->large_queue);
    return;
}

/* Request block from the memory pool */
int mca_coll_solo_mpool_request(mca_coll_solo_mpool_t * mpool, size_t len)
{
    if (len > mca_coll_solo_component.mpool_large_block_size) {
        return -1;
    }
    int id = -1;
    while (id == -1) {
        if (len <= mca_coll_solo_component.mpool_small_block_size) {
            id = mca_coll_solo_queue_request(mpool->small_queue);
        } else {
            id = mca_coll_solo_queue_request(mpool->large_queue);
        }
    }
    return id;
}

/* Calculate block address */
char *mca_coll_solo_mpool_calculate(mca_coll_solo_mpool_t * mpool, int id, size_t len)
{
    if (id <= 0 || len > mca_coll_solo_component.mpool_large_block_size) {
        return NULL;
    }
    char *addr;
    if (len <= mca_coll_solo_component.mpool_small_block_size) {
        addr = mca_coll_solo_queue_calculate(mpool->small_queue, id);
    } else {
        addr = mca_coll_solo_queue_calculate(mpool->large_queue, id);
    }
    return addr;
}

/* Return block to memory pool */
void mca_coll_solo_mpool_return(mca_coll_solo_mpool_t * mpool, int id, size_t len)
{
    if (len <= mca_coll_solo_component.mpool_small_block_size) {
        mca_coll_solo_queue_return(mpool->small_queue, id);
    } else if (len <= mca_coll_solo_component.mpool_large_block_size) {
        mca_coll_solo_queue_return(mpool->large_queue, id);
    } else {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output, 
                            "coll:solo:mca_coll_solo_mpool_return: block size is wrong!");
    }
    return;
}
