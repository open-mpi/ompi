/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/mca/coll/coll.h"
#include "opal/class/opal_free_list.h"
#include "opal/class/opal_list.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/op/op.h"
#include "ompi/mca/coll/base/coll_base_topo.h"
#include "coll_adapt_inbuf.h"

/* Bcast constant context in bcast context */
struct ompi_coll_adapt_constant_bcast_context_s {
    opal_object_t super;
    int root;
    size_t count;
    size_t seg_count;
    ompi_datatype_t *datatype;
    ompi_communicator_t *comm;
    int real_seg_size;
    int num_segs;
    ompi_request_t *request;
    opal_mutex_t *mutex;
    int *recv_array;
    int *send_array;
    /* Length of the fragment array, which is the number of recevied segments */
    int num_recv_segs;
    /* Number of segments that is finishing recving */
    int num_recv_fini;
    /* Store the number of sent segments */
    int num_sent_segs;
    ompi_coll_tree_t *tree;
    int ibcast_tag;
};

typedef struct ompi_coll_adapt_constant_bcast_context_s ompi_coll_adapt_constant_bcast_context_t;

OBJ_CLASS_DECLARATION(ompi_coll_adapt_constant_bcast_context_t);


/* Bcast context of each segment*/
typedef struct ompi_coll_adapt_bcast_context_s ompi_coll_adapt_bcast_context_t;

typedef int (*ompi_coll_adapt_bcast_cuda_callback_fn_t) (ompi_coll_adapt_bcast_context_t * context);

struct ompi_coll_adapt_bcast_context_s {
    opal_free_list_item_t super;
    char *buff;
    int frag_id;
    int child_id;
    int peer;
    ompi_coll_adapt_constant_bcast_context_t *con;
};

OBJ_CLASS_DECLARATION(ompi_coll_adapt_bcast_context_t);

/* Reduce constant context in reduce context */
struct ompi_coll_adapt_constant_reduce_context_s {
    opal_object_t super;
    size_t count;
    size_t seg_count;
    ompi_datatype_t *datatype;
    ompi_communicator_t *comm;
    size_t real_seg_size;
    /* Increment of each segment */
    int segment_increment;
    int num_segs;
    int rank;
    int root;
    /* The distance between the address of inbuf->buff and the address of inbuf */
    int distance;
    int ireduce_tag;
    /* How many sends are posted but not finished */
    int32_t ongoing_send;
    /* Length of the fragment array, which is the number of recevied segments */
    int32_t num_recv_segs;
    /* Number of sent segments */
    int32_t num_sent_segs;
    /* Next seg need to be received for every children */
    int32_t *next_recv_segs;
    /* Mutex to protect each segment when do the reduce op */
    opal_mutex_t *mutex_op_list;
    /* Reduce operation */
    ompi_op_t *op;
    ompi_coll_tree_t *tree;
    /* Accumulate buff */
    char **accumbuf;
    ptrdiff_t lower_bound;
    char *sbuf;
    char *rbuf;
    opal_free_list_t inbuf_list;
    /* Mutex to protect recv_list */
    opal_mutex_t mutex_recv_list;
    /* A list to store the segments which are received and not yet be sent */
    opal_list_t recv_list;
    ompi_request_t *request;
};

typedef struct ompi_coll_adapt_constant_reduce_context_s ompi_coll_adapt_constant_reduce_context_t;

OBJ_CLASS_DECLARATION(ompi_coll_adapt_constant_reduce_context_t);

/* Reduce context of each segment */
typedef struct ompi_coll_adapt_reduce_context_s ompi_coll_adapt_reduce_context_t;

typedef int (*ompi_coll_adapt_reduce_cuda_callback_fn_t) (ompi_coll_adapt_reduce_context_t * context);

struct ompi_coll_adapt_reduce_context_s {
    opal_free_list_item_t super;
    char *buff;
    int seg_index;
    int child_id;
    int peer;
    ompi_coll_adapt_constant_reduce_context_t *con;
    /* store the incoming segment */
    ompi_coll_adapt_inbuf_t *inbuf;
};

OBJ_CLASS_DECLARATION(ompi_coll_adapt_reduce_context_t);
