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

#include "opal/class/opal_object.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_list.h"
#include "opal/threads/threads.h"

#define COLL_SHARED_SMALL_BLOCK_SIZE 1048576
#define COLL_SHARED_LARGE_BLOCK_SIZE 8388608

#define COLL_SHARED_DWORD int64_t
#define COLL_SHARED_WORD int32_t
typedef struct {
    COLL_SHARED_WORD id;
    COLL_SHARED_WORD ref;
} mca_coll_shared_tag_t;

struct mca_coll_shared_queue_t {
    char *blocks;
    int block_num;
    size_t block_size;
    char *id_queue;
    char *head;
    char *tail;
    MPI_Win win;
};

typedef struct mca_coll_shared_queue_t mca_coll_shared_queue_t;

OBJ_CLASS_DECLARATION(mca_coll_shared_queue_t);

void mca_coll_shared_queue_init(mca_coll_shared_queue_t *queue, ompi_communicator_t *node_comm, int block_num, int block_size);
int mca_coll_shared_queue_request(mca_coll_shared_queue_t *queue);
char *mca_coll_shared_queue_calculate(mca_coll_shared_queue_t *queue, int id);
void mca_coll_shared_queue_return(mca_coll_shared_queue_t *queue, int id);

struct mca_coll_shared_mpool_t {
    /* Generic parent class for all Open MPI objects */
    opal_object_t super;
    /* An array-based queue contains small blocks */
    mca_coll_shared_queue_t *small_queue;
    /* An array-based queue contains large blocks */
    mca_coll_shared_queue_t *large_queue;
    /* A communicator contains all the processes on a node */
    ompi_communicator_t *node_comm;
};

typedef struct mca_coll_shared_mpool_t mca_coll_shared_mpool_t;

OBJ_CLASS_DECLARATION(mca_coll_shared_mpool_t);

/*
 * Request block from memory pool
 */
int mca_coll_shared_mpool_request(mca_coll_shared_mpool_t *mpool, size_t len);

/*
 * Calculate block address
 */
char *mca_coll_shared_mpool_calculate(mca_coll_shared_mpool_t *mpool, int id, size_t len); 

/*
 * Return block to memory pool
 */
void mca_coll_shared_mpool_return(mca_coll_shared_mpool_t *mpool, int id, size_t len);

