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


#include "opal/class/opal_object.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_list.h"
#include "opal/threads/threads.h"

#define COLL_SOLO_DWORD int64_t
#define COLL_SOLO_WORD int32_t
typedef struct {
    /* the block id */
    COLL_SOLO_WORD id;
    /* ref is added to resolve the potential ABA problem */
    COLL_SOLO_WORD ref;
} mca_coll_solo_tag_t;

/**
 * A lock-free array-based queue containing the blocks which can be accessed by any processes 
 * on the same node. 
 * An example of the queue is shown below (block_num is n, the size of each element in id queue, 
 * head and tail is opal_cache_line_size to avoid false sharing):
 * Init:
 * |                        blocks                        |        id queue       | head  | tail |
 * | block1 (avail) | block2 (avail) |...| blockn (avail) | 1 | 2 | 3 |...| n | 0 |  0/0  |   n  |
 * Request a block - 0 in the id queue means it is not available:
 * | block1 (using) | block2 (avail) |...| blockn (avail) | 0 | 2 | 3 |...| n | 0 |  1/1  |   n  |
 * Request another block:
 * | block1 (using) | block2 (using) |...| blockn (avail) | 0 | 0 | 3 |...| n | 0 |  2/2  |   n  |
 * Return block 2:
 * | block1 (using) | block2 (avail) |...| blockn (avail) | 0 | 0 | 3 |...| n | 2 |  2/2  |   0  |
 */
struct mca_coll_solo_queue_t {
    /* the start address of blocks */
    char *blocks;
    /* the number of blocks */
    int block_num;
    /* the size of each block */
    size_t block_size;
    /* the start address of id queue */
    char *id_queue;
    /* the address of head */
    char *head;
    /* the address of tail */
    char *tail;
    /* a node-wise window */
    MPI_Win win;
};

typedef struct mca_coll_solo_queue_t mca_coll_solo_queue_t;

OBJ_CLASS_DECLARATION(mca_coll_solo_queue_t);

/* Init the queue */
void mca_coll_solo_queue_init(mca_coll_solo_queue_t * queue, ompi_communicator_t * node_comm, 
                             int block_num, int block_size);
/* Request a block from the queue, return a block id */
int mca_coll_solo_queue_request(mca_coll_solo_queue_t * queue);
/* Calculate the block address with a block id */
char *mca_coll_solo_queue_calculate(mca_coll_solo_queue_t * queue, int id);
/* Return a block to the queue */
void mca_coll_solo_queue_return(mca_coll_solo_queue_t * queue, int id);

/* Each node has a shared memory pool, which contains two queues of different block sizes.*/
struct mca_coll_solo_mpool_t {
    /* Generic parent class for all Open MPI objects */
    opal_object_t super;
    /* An array-based queue contains small blocks */
    mca_coll_solo_queue_t *small_queue;
    /* An array-based queue contains large blocks */
    mca_coll_solo_queue_t *large_queue;
    /* A communicator contains all the processes on a node */
    ompi_communicator_t *node_comm;
};

typedef struct mca_coll_solo_mpool_t mca_coll_solo_mpool_t;

OBJ_CLASS_DECLARATION(mca_coll_solo_mpool_t);

/* Request block from memory pool */
int mca_coll_solo_mpool_request(mca_coll_solo_mpool_t * mpool, size_t len);

/* Calculate block address */
char *mca_coll_solo_mpool_calculate(mca_coll_solo_mpool_t * mpool, int id, size_t len);

/* Return block to memory pool */
void mca_coll_solo_mpool_return(mca_coll_solo_mpool_t * mpool, int id, size_t len);
