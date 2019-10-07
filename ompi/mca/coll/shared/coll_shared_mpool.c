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

/**
 *  queue classes
 */
static void mca_coll_shared_queue_construct(mca_coll_shared_queue_t *queue);
static void mca_coll_shared_queue_destruct(mca_coll_shared_queue_t *queue);

static void mca_coll_shared_queue_construct(mca_coll_shared_queue_t *queue)
{
    return;
}

static void mca_coll_shared_queue_destruct(mca_coll_shared_queue_t *queue)
{
    return;
}

OBJ_CLASS_INSTANCE(
    mca_coll_shared_queue_t,
    opal_object_t,
    mca_coll_shared_queue_construct,
    mca_coll_shared_queue_destruct);

void mca_coll_shared_queue_init(mca_coll_shared_queue_t *queue, ompi_communicator_t *node_comm, int block_num, int block_size)
{
    int node_rank = ompi_comm_rank(node_comm);
    queue->block_size = block_size;
    queue->block_num = block_num;
    int *temp_ptr;
    int id_queue_size = opal_cache_line_size * (block_num + 2);
    if (node_rank == 0) {
        ompi_win_allocate_shared(block_size * block_num + id_queue_size, sizeof(char),
                                 (opal_info_t *)(&ompi_mpi_info_null), node_comm, &temp_ptr, &(queue->win));
    }
    else {
        ompi_win_allocate_shared(0, sizeof(char),
                                 (opal_info_t *)(&ompi_mpi_info_null), node_comm, &temp_ptr, &(queue->win));
    }
    size_t temp_size;
    int temp_disp;
    /* Get the address of the shared memory */
    queue->win->w_osc_module->osc_win_shared_query(queue->win, 0, &temp_size, &temp_disp, &queue->blocks);
    queue->id_queue = queue->blocks + block_size * block_num;
    queue->head = queue->id_queue + opal_cache_line_size * block_num;
    queue->tail = queue->id_queue + opal_cache_line_size * (block_num + 1);
    queue->win->w_osc_module->osc_fence(0, queue->win);
    if (node_rank == 0) {
        (*((mca_coll_shared_tag_t *)queue->head)).id = 0;
        (*((mca_coll_shared_tag_t *)queue->head)).ref = 0;
        *((COLL_SHARED_WORD *)queue->tail) = block_num - 1;
        int i;
        for (i = 0; i < block_num; i++) {
            char *temp = queue->id_queue + opal_cache_line_size * i;
            *((COLL_SHARED_WORD *)temp) = i + 1;
            if (i == block_num - 1) {
                *((COLL_SHARED_WORD *)temp) = 0;
            }
        }
    }
    queue->win->w_osc_module->osc_fence(0, queue->win);
    return;
}

/*
 * Request a block from the queue
 */
int mca_coll_shared_queue_request(mca_coll_shared_queue_t *queue) 
{
    COLL_SHARED_DWORD cur_head, new_head;
    COLL_SHARED_WORD cur_tail;
    do {
        cur_head = *((COLL_SHARED_DWORD *)queue->head);
        cur_tail = *((COLL_SHARED_WORD *)queue->tail);
        if (((mca_coll_shared_tag_t *)&cur_head)->id == cur_tail) {
            return -1;
        }
        new_head = cur_head;
        ((mca_coll_shared_tag_t *)&new_head)->id = (((mca_coll_shared_tag_t *)&new_head)->id + 1) % queue->block_num;
        ((mca_coll_shared_tag_t *)&new_head)->ref = ((mca_coll_shared_tag_t *)&new_head)->ref + 1;
    } while (!opal_atomic_compare_exchange_strong_64((COLL_SHARED_DWORD *)queue->head, &cur_head, new_head));
    char *temp = queue->id_queue + opal_cache_line_size * ((mca_coll_shared_tag_t *)&cur_head)->id;
    COLL_SHARED_WORD id = *((COLL_SHARED_WORD *)temp);
    *((COLL_SHARED_WORD *)temp) = 0;
    return id;
}

/* 
 * Calculate block address based on block id
 */
char *mca_coll_shared_queue_calculate(mca_coll_shared_queue_t *queue, int id) 
{
    return queue->blocks + queue->block_size * id;
}

/*
 * Return a block to the queue
 */
void mca_coll_shared_queue_return(mca_coll_shared_queue_t *queue, int id) {
    COLL_SHARED_WORD cur_tail;
    char *temp;
    int zero = 0;
    do {
        cur_tail = *((COLL_SHARED_WORD *)queue->tail);
        temp = queue->id_queue + opal_cache_line_size * cur_tail;
    } while (!opal_atomic_compare_exchange_strong_32((COLL_SHARED_WORD *)temp, &zero, id));
    opal_atomic_compare_exchange_strong_32((COLL_SHARED_WORD *)queue->tail, &cur_tail, (cur_tail + 1) % queue->block_num);
    return;
}


/*
 *  mpool classes
 */
static void mca_coll_shared_mpool_construct(mca_coll_shared_mpool_t *mpool);
static void mca_coll_shared_mpool_destruct(mca_coll_shared_mpool_t *mpool);

OBJ_CLASS_INSTANCE(
    mca_coll_shared_mpool_t,
    opal_object_t,
    mca_coll_shared_mpool_construct,
    mca_coll_shared_mpool_destruct);

static void mca_coll_shared_mpool_construct(mca_coll_shared_mpool_t *mpool)
{
    /* create the node_comm which contains all the processes on a node */
    ompi_comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0, (opal_info_t *)(&ompi_mpi_info_null), &(mpool->node_comm));
    int node_size = ompi_comm_size(mpool->node_comm);
    mpool->small_queue = OBJ_NEW(mca_coll_shared_queue_t);
    mpool->large_queue = OBJ_NEW(mca_coll_shared_queue_t);
    mca_coll_shared_queue_init(mpool->small_queue, mpool->node_comm, node_size * 4 + 1, COLL_SHARED_SMALL_BLOCK_SIZE);
    mca_coll_shared_queue_init(mpool->large_queue, mpool->node_comm, node_size * 2 + 1, COLL_SHARED_LARGE_BLOCK_SIZE);
    return;
}

static void mca_coll_shared_mpool_destruct(mca_coll_shared_mpool_t *mpool)
{
    OBJ_RELEASE(mpool->small_queue);
    OBJ_RELEASE(mpool->large_queue);
    return;
}

/*
 * Request memory allocation from the memory pool
 */
int mca_coll_shared_mpool_request(mca_coll_shared_mpool_t *mpool, size_t len)
{
    if (len > COLL_SHARED_LARGE_BLOCK_SIZE) {
        return -1;
    }
    int id = -1;
    while (id == -1) {
        if (len <= COLL_SHARED_SMALL_BLOCK_SIZE) {
            id = mca_coll_shared_queue_request(mpool->small_queue);
        }
        else {
            id = mca_coll_shared_queue_request(mpool->large_queue);
        }
    }

    return id;
}

/*
 * Calculate block address
 */
char *mca_coll_shared_mpool_calculate(mca_coll_shared_mpool_t *mpool, int id, size_t len) {
    if (id <= 0 || len > COLL_SHARED_LARGE_BLOCK_SIZE) {
        return NULL;
    }
    char *addr;
    if (len <= COLL_SHARED_SMALL_BLOCK_SIZE) {
        addr = mca_coll_shared_queue_calculate(mpool->small_queue, id);
    }
    else {
        addr = mca_coll_shared_queue_calculate(mpool->large_queue, id);
    }
    return addr;
}

/*
 * Return memory allocation to the memory pool
 */
void mca_coll_shared_mpool_return(mca_coll_shared_mpool_t *mpool, int id, size_t len)
{
    if (len <= COLL_SHARED_SMALL_BLOCK_SIZE) {
        mca_coll_shared_queue_return(mpool->small_queue, id);
    }
    else if (len <= COLL_SHARED_LARGE_BLOCK_SIZE) {
        mca_coll_shared_queue_return(mpool->large_queue, id);
    }
    else {
        /* TODO:  add warning message */
        ;
    }
    return;
}
