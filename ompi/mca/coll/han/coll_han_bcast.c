/*
 * Copyright (c) 2018-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * This files contains all the hierarchical implementations of bcast
 */

#include "coll_han.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "coll_han_trigger.h"

static int mca_coll_han_bcast_t0_task(void *task_args);
static int mca_coll_han_bcast_t1_task(void *task_args);

static inline void
mca_coll_han_set_bcast_args(mca_coll_han_bcast_args_t * args, mca_coll_task_t * cur_task, void *buff,
                            int seg_count, struct ompi_datatype_t *dtype,
                            int root_up_rank, int root_low_rank,
                            struct ompi_communicator_t *up_comm,
                            struct ompi_communicator_t *low_comm,
                            int num_segments, int cur_seg, int w_rank, int last_seg_count,
                            bool noop)
{
    args->cur_task = cur_task;
    args->buff = buff;
    args->seg_count = seg_count;
    args->dtype = dtype;
    args->root_low_rank = root_low_rank;
    args->root_up_rank = root_up_rank;
    args->up_comm = up_comm;
    args->low_comm = low_comm;
    args->num_segments = num_segments;
    args->cur_seg = cur_seg;
    args->w_rank = w_rank;
    args->last_seg_count = last_seg_count;
    args->noop = noop;
}

/*
 * Each segment of the message needs to go though 2 steps to perform MPI_Bcast:
 *     ub: upper level (inter-node) bcast
 *     lb: low level (shared-memory or intra-node) bcast.
 * Hence, in each iteration, there is a combination of collective operations which is called a task.
 *        | seg 0 | seg 1 | seg 2 | seg 3 |
 * iter 0 |  ub   |       |       |       | task: t0, contains ub
 * iter 1 |  lb   |  ub   |       |       | task: t1, contains ub and lb
 * iter 2 |       |  lb   |  ub   |       | task: t1, contains ub and lb
 * iter 3 |       |       |  lb   |  ub   | task: t1, contains ub and lb
 * iter 4 |       |       |       |  lb   | task: t1, contains lb
 */
int
mca_coll_han_bcast_intra(void *buf,
                         int count,
                         struct ompi_datatype_t *dtype,
                         int root,
                         struct ompi_communicator_t *comm, mca_coll_base_module_t * module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;
    int err, seg_count = count, w_rank = ompi_comm_rank(comm);
    ompi_communicator_t *low_comm, *up_comm;
    ptrdiff_t extent, lb;
    size_t dtype_size;

    /* Create the subcommunicators */
    err = mca_coll_han_comm_create(comm, han_module);
    if( OMPI_SUCCESS != err ) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle bcast with this communicator. Fall back on another component\n"));
        /* Put back the fallback collective support and call it once. All
         * future calls will then be automatically redirected.
         */
        HAN_LOAD_FALLBACK_COLLECTIVES(han_module, comm);
        return han_module->previous_bcast(buf, count, dtype, root,
                                          comm, han_module->previous_bcast_module);
    }
    /* Topo must be initialized to know rank distribution which then is used to
     * determine if han can be used */
    mca_coll_han_topo_init(comm, han_module, 2);
    if (han_module->are_ppn_imbalanced) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle bcast with this communicator (imbalance). Fall back on another component\n"));
        /* Put back the fallback collective support and call it once. All
         * future calls will then be automatically redirected.
         */
        HAN_LOAD_FALLBACK_COLLECTIVE(han_module, comm, bcast);
        return han_module->previous_bcast(buf, count, dtype, root,
                                          comm, han_module->previous_bcast_module);
    }

    ompi_datatype_get_extent(dtype, &lb, &extent);
    ompi_datatype_type_size(dtype, &dtype_size);

    /* use MCA parameters for now */
    low_comm = han_module->cached_low_comms[mca_coll_han_component.han_bcast_low_module];
    up_comm = han_module->cached_up_comms[mca_coll_han_component.han_bcast_up_module];
    COLL_BASE_COMPUTED_SEGCOUNT(mca_coll_han_component.han_bcast_segsize, dtype_size,
                                seg_count);

    int num_segments = (count + seg_count - 1) / seg_count;
    OPAL_OUTPUT_VERBOSE((20, mca_coll_han_component.han_output,
                         "In HAN seg_count %d count %d num_seg %d\n",
                         seg_count, count, num_segments));

    int *vranks = han_module->cached_vranks;
    int low_rank = ompi_comm_rank(low_comm);
    int low_size = ompi_comm_size(low_comm);

    int root_low_rank, root_up_rank;
    mca_coll_han_get_ranks(vranks, root, low_size, &root_low_rank, &root_up_rank);
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                         "[%d]: root_low_rank %d root_up_rank %d\n", w_rank, root_low_rank,
                         root_up_rank));

    /* Create t0 tasks for the first segment */
    mca_coll_task_t *t0 = OBJ_NEW(mca_coll_task_t);
    /* Setup up t0 task arguments */
    mca_coll_han_bcast_args_t *t = malloc(sizeof(mca_coll_han_bcast_args_t));
    mca_coll_han_set_bcast_args(t, t0, (char *)buf, seg_count, dtype,
                                root_up_rank, root_low_rank, up_comm, low_comm,
                                num_segments, 0, w_rank, count - (num_segments - 1) * seg_count,
                                low_rank != root_low_rank);
    /* Init the first task */
    init_task(t0, mca_coll_han_bcast_t0_task, (void *) t);
    issue_task(t0);

    /* Create t1 task */
    mca_coll_task_t *t1 = OBJ_NEW(mca_coll_task_t);
    /* Setup up t1 task arguments */
    t->cur_task = t1;
    /* Init the t1 task */
    init_task(t1, mca_coll_han_bcast_t1_task, (void *) t);
    issue_task(t1);

    while (t->cur_seg <= t->num_segments - 2) {
        /* Create t1 task */
        t->cur_task = t1 = OBJ_NEW(mca_coll_task_t);
        t->buff = (char *) t->buff + extent * seg_count;
        t->cur_seg = t->cur_seg + 1;
        /* Init the t1 task */
        init_task(t1, mca_coll_han_bcast_t1_task, (void *) t);
        issue_task(t1);
    }

    free(t);

    return OMPI_SUCCESS;
}

/* t0 task: issue and wait for the upper level ibcast of segment 0 */
int mca_coll_han_bcast_t0_task(void *task_args)
{
    mca_coll_han_bcast_args_t *t = (mca_coll_han_bcast_args_t *) task_args;

    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d]: in t0 %d\n", t->w_rank,
                         t->cur_seg));
    OBJ_RELEASE(t->cur_task);
    if (t->noop) {
        return OMPI_SUCCESS;
    }
    t->up_comm->c_coll->coll_bcast((char *) t->buff, t->seg_count, t->dtype, t->root_up_rank,
                                   t->up_comm, t->up_comm->c_coll->coll_bcast_module);
    return OMPI_SUCCESS;
}

/* t1 task:
 * 1. issue the upper level ibcast of segment cur_seg + 1
 * 2. issue the low level bcast of segment cur_seg
 * 3. wait for the completion of the ibcast
 */
int mca_coll_han_bcast_t1_task(void *task_args)
{
    mca_coll_han_bcast_args_t *t = (mca_coll_han_bcast_args_t *) task_args;
    ompi_request_t *ibcast_req = NULL;
    int tmp_count = t->seg_count;
    ptrdiff_t extent, lb;

    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d]: in t1 %d\n", t->w_rank,
                         t->cur_seg));
    OBJ_RELEASE(t->cur_task);
    ompi_datatype_get_extent(t->dtype, &lb, &extent);
    if (!t->noop) {
        if (t->cur_seg <= t->num_segments - 2 ) {
            if (t->cur_seg == t->num_segments - 2) {
                tmp_count = t->last_seg_count;
            }
            t->up_comm->c_coll->coll_ibcast((char *) t->buff + extent * t->seg_count,
                                            tmp_count, t->dtype, t->root_up_rank,
                                            t->up_comm, &ibcast_req,
                                            t->up_comm->c_coll->coll_ibcast_module);
        }
    }

    /* are we the last segment to be pushed downstream ? */
    tmp_count = (t->cur_seg == (t->num_segments - 1)) ? t->last_seg_count : t->seg_count;
    t->low_comm->c_coll->coll_bcast((char *) t->buff,
                                    tmp_count, t->dtype, t->root_low_rank, t->low_comm,
                                    t->low_comm->c_coll->coll_bcast_module);

    if (NULL != ibcast_req) {
        ompi_request_wait(&ibcast_req, MPI_STATUS_IGNORE);
    }

    return OMPI_SUCCESS;
}

/*
 * Short implementation of bcast that only does hierarchical
 * communications without tasks.
 */
int
mca_coll_han_bcast_intra_simple(void *buf,
                                int count,
                                struct ompi_datatype_t *dtype,
                                int root,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
{
    /* create the subcommunicators */
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;
    ompi_communicator_t *low_comm, *up_comm;
    int err;
#if OPAL_ENABLE_DEBUG
    int w_rank = ompi_comm_rank(comm);
#endif

    /* Create the subcommunicators */
    err = mca_coll_han_comm_create_new(comm, han_module);
    if( OMPI_SUCCESS != err ) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle bcast with this communicator. Fall back on another component\n"));
        /* Put back the fallback collective support and call it once. All
         * future calls will then be automatically redirected.
         */
        HAN_LOAD_FALLBACK_COLLECTIVES(han_module, comm);
        return han_module->previous_bcast(buf, count, dtype, root,
                                          comm, han_module->previous_bcast_module);
    }
    /* Topo must be initialized to know rank distribution which then is used to
     * determine if han can be used */
    mca_coll_han_topo_init(comm, han_module, 2);
    if (han_module->are_ppn_imbalanced) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle bcast with this communicator (imbalance). Fall back on another component\n"));
        /* Put back the fallback collective support and call it once. All
         * future calls will then be automatically redirected.
         */
        HAN_LOAD_FALLBACK_COLLECTIVE(han_module, comm, bcast);
        return han_module->previous_bcast(buf, count, dtype, root,
                                          comm, han_module->previous_bcast_module);
    }

    low_comm = han_module->sub_comm[INTRA_NODE];
    up_comm = han_module->sub_comm[INTER_NODE];

    int *vranks = han_module->cached_vranks;
    int low_rank = ompi_comm_rank(low_comm);
    int low_size = ompi_comm_size(low_comm);
    int root_low_rank, root_up_rank;

    mca_coll_han_get_ranks(vranks, root, low_size, &root_low_rank, &root_up_rank);
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                         "[%d]: root_low_rank %d root_up_rank %d\n",
                         w_rank, root_low_rank, root_up_rank));

    if (low_rank == root_low_rank) {
        up_comm->c_coll->coll_bcast(buf, count, dtype, root_up_rank,
                                    up_comm, up_comm->c_coll->coll_bcast_module);

        /* To remove when han has better sub-module selection.
           For now switching to ibcast enables to make runs with libnbc. */
        //ompi_request_t req;
        //up_comm->c_coll->coll_ibcast(buf, count, dtype, root_up_rank,
        //                             up_comm, &req, up_comm->c_coll->coll_ibcast_module);
        //ompi_request_wait(&req, MPI_STATUS_IGNORE);

    }
    low_comm->c_coll->coll_bcast(buf, count, dtype, root_low_rank,
                                 low_comm, low_comm->c_coll->coll_bcast_module);

    return OMPI_SUCCESS;
}
