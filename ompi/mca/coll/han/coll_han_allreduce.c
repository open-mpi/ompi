/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "coll_han.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "coll_han_trigger.h"

/* Only work with regular situation (each node has equal number of processes) */

void mac_coll_han_set_allreduce_argu(mca_allreduce_argu_t * argu,
                                     mca_coll_task_t * cur_task,
                                     void *sbuf,
                                     void *rbuf,
                                     int seg_count,
                                     struct ompi_datatype_t *dtype,
                                     struct ompi_op_t *op,
                                     int root_up_rank,
                                     int root_low_rank,
                                     struct ompi_communicator_t *up_comm,
                                     struct ompi_communicator_t *low_comm,
                                     int num_segments,
                                     int cur_seg,
                                     int w_rank,
                                     int last_seg_count,
                                     bool noop, ompi_request_t * req, int *completed)
{
    argu->cur_task = cur_task;
    argu->sbuf = sbuf;
    argu->rbuf = rbuf;
    argu->seg_count = seg_count;
    argu->dtype = dtype;
    argu->op = op;
    argu->root_up_rank = root_up_rank;
    argu->root_low_rank = root_low_rank;
    argu->up_comm = up_comm;
    argu->low_comm = low_comm;
    argu->num_segments = num_segments;
    argu->cur_seg = cur_seg;
    argu->w_rank = w_rank;
    argu->last_seg_count = last_seg_count;
    argu->noop = noop;
    argu->req = req;
    argu->completed = completed;
}

/* 
 * Each segment of the messsage needs to go though 4 steps to perform MPI_Allreduce: 
 *     lr: lower level (shared-memory or intra-node) reduce,
 *     ur: upper level (inter-node) reduce,
 *     ub: upper level (inter-node) bcast,
 *     lb: lower level (shared-memory or intra-node) bcast.
 * Hence, in each iteration, there is a combination of collective operations which is called a task.
 *        | seg 0 | seg 1 | seg 2 | seg 3 |
 * iter 0 |  lr   |       |       |       | task: t0, contains lr
 * iter 1 |  ur   |  lr   |       |       | task: t1, contains ur and lr
 * iter 2 |  ub   |  ur   |  lr   |       | task: t2, contains ub, ur and lr
 * iter 3 |  lb   |  ub   |  ur   |  lr   | task: t3, contains lb, ub, ur and lr
 * iter 4 |       |  lb   |  ub   |  ur   | task: t3, contains lb, ub and ur
 * iter 5 |       |       |  lb   |  ub   | task: t3, contains lb and ub
 * iter 6 |       |       |       |  lb   | task: t3, contains lb
 */

int
mca_coll_han_allreduce_intra(const void *sbuf,
                             void *rbuf,
                             int count,
                             struct ompi_datatype_t *dtype,
                             struct ompi_op_t *op,
                             struct ompi_communicator_t *comm, mca_coll_base_module_t * module)
{
    ptrdiff_t extent, lb;
    ompi_datatype_get_extent(dtype, &lb, &extent);
    int w_rank;
    w_rank = ompi_comm_rank(comm);
    int seg_count = count;
    size_t typelng;
    ompi_datatype_type_size(dtype, &typelng);

    /* Create the subcommunicators */
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *) module;
    mca_coll_han_comm_create(comm, han_module);
    ompi_communicator_t *low_comm;
    ompi_communicator_t *up_comm;
    /* Auto tune is enabled */
    if (mca_coll_han_component.han_auto_tune && mca_coll_han_component.han_auto_tuned != NULL) {
        uint32_t n = han_auto_tuned_get_n(ompi_comm_size(han_module->cached_up_comms[0]));
        uint32_t c = han_auto_tuned_get_c(ompi_comm_size(han_module->cached_low_comms[0]));
        uint32_t m = han_auto_tuned_get_m(typelng * count);
        uint32_t id =
            n * mca_coll_han_component.han_auto_tune_c * mca_coll_han_component.han_auto_tune_m +
            c * mca_coll_han_component.han_auto_tune_m + m +
            mca_coll_han_component.han_auto_tune_n * mca_coll_han_component.han_auto_tune_c *
            mca_coll_han_component.han_auto_tune_m;
        uint32_t umod = mca_coll_han_component.han_auto_tuned[id].umod;
        uint32_t lmod = mca_coll_han_component.han_auto_tuned[id].lmod;
        uint32_t fs = mca_coll_han_component.han_auto_tuned[id].fs;
        /* ualg and us are only available when using ADAPT */
        /*
        uint32_t ualg = mca_coll_han_component.han_auto_tuned[id].ualg;
        uint32_t us = mca_coll_han_component.han_auto_tuned[id].us;
        */
        /* Set up umod */
        up_comm = han_module->cached_up_comms[umod];
        /* Set up lmod */
        low_comm = han_module->cached_low_comms[lmod];
        /* Set up fs */
        COLL_BASE_COMPUTED_SEGCOUNT((size_t) fs, typelng, seg_count);
        /* Set up ualg and us, which is only available when using ADAPT */
        /*
        if (umod == 1) {
            ((mca_coll_adapt_module_t *) (up_comm->c_coll->coll_ibcast_module))->adapt_component->
                adapt_ibcast_algorithm = ualg;
            ((mca_coll_adapt_module_t *) (up_comm->c_coll->coll_ibcast_module))->adapt_component->
                adapt_ibcast_algorithm = ualg;
            ((mca_coll_adapt_module_t *) (up_comm->c_coll->coll_ibcast_module))->adapt_component->
                adapt_ibcast_segment_size = us;
            ((mca_coll_adapt_module_t *) (up_comm->c_coll->coll_ibcast_module))->adapt_component->
                adapt_ibcast_segment_size = us;
        }
        */
    } else {
        low_comm = han_module->cached_low_comms[mca_coll_han_component.han_bcast_low_module];
        up_comm = han_module->cached_up_comms[mca_coll_han_component.han_bcast_up_module];
        COLL_BASE_COMPUTED_SEGCOUNT(mca_coll_han_component.han_allreduce_segsize, typelng,
                                    seg_count);
    }

    /* Determine number of elements sent per task. */
    OPAL_OUTPUT_VERBOSE((10, mca_coll_han_component.han_output,
                         "In HAN Allreduce seg_size %d seg_count %d count %d\n",
                         mca_coll_han_component.han_allreduce_segsize, seg_count, count));
    int num_segments = (count + seg_count - 1) / seg_count;

    int low_rank = ompi_comm_rank(low_comm);
    int root_up_rank = 0;
    int root_low_rank = 0;
    /* Create t0 task for the first segment */
    mca_coll_task_t *t0 = OBJ_NEW(mca_coll_task_t);
    /* Setup up t0 task arguments */
    int *completed = (int *) malloc(sizeof(int));
    completed[0] = 0;
    mca_allreduce_argu_t *t = malloc(sizeof(mca_allreduce_argu_t));
    mac_coll_han_set_allreduce_argu(t, t0, (char *) sbuf, (char *) rbuf, seg_count, dtype, op,
                                    root_up_rank, root_low_rank, up_comm, low_comm, num_segments, 0,
                                    w_rank, count - (num_segments - 1) * seg_count,
                                    low_rank != root_low_rank, NULL, completed);
    /* Init t0 task */
    init_task(t0, mca_coll_han_allreduce_t0_task, (void *) (t));
    /* Issure t0 task */
    issue_task(t0);

    /* Create t1 tasks for the current segment */
    mca_coll_task_t *t1 = OBJ_NEW(mca_coll_task_t);
    /* Setup up t1 task arguments */
    t->cur_task = t1;
    /* Init t1 task */
    init_task(t1, mca_coll_han_allreduce_t1_task, (void *) t);
    /* Issue t1 task */
    issue_task(t1);

    /* Create t2 tasks for the current segment */
    mca_coll_task_t *t2 = OBJ_NEW(mca_coll_task_t);
    /* Setup up t2 task arguments */
    t->cur_task = t2;
    /* Init t2 task */
    init_task(t2, mca_coll_han_allreduce_t2_task, (void *) t);
    issue_task(t2);

    /* Create t3 tasks for the current segment */
    mca_coll_task_t *t3 = OBJ_NEW(mca_coll_task_t);
    /* Setup up t3 task arguments */
    t->cur_task = t3;
    /* Init t3 task */
    init_task(t3, mca_coll_han_allreduce_t3_task, (void *) t);
    issue_task(t3);

    while (t->completed[0] != t->num_segments) {
        /* Create t3 tasks for the current segment */
        mca_coll_task_t *t3 = OBJ_NEW(mca_coll_task_t);
        /* Setup up t3 task arguments */
        t->cur_task = t3;
        t->sbuf = (char *) t->sbuf + extent * t->seg_count;
        t->rbuf = (char *) t->rbuf + extent * t->seg_count;
        t->cur_seg = t->cur_seg + 1;
        /* Init t3 task */
        init_task(t3, mca_coll_han_allreduce_t3_task, (void *) t);
        issue_task(t3);
    }
    if (t->completed != NULL) {
        free(t->completed);
        t->completed = NULL;
    }
    free(t);

    return OMPI_SUCCESS;
}

/* t0 task */
int mca_coll_han_allreduce_t0_task(void *task_argu)
{
    mca_allreduce_argu_t *t = (mca_allreduce_argu_t *) task_argu;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                         "[%d] HAN Allreduce:  t0 %d r_buf %d\n", t->w_rank, t->cur_seg,
                         ((int *) t->rbuf)[0]));
    OBJ_RELEASE(t->cur_task);
    ptrdiff_t extent, lb;
    ompi_datatype_get_extent(t->dtype, &lb, &extent);
    t->low_comm->c_coll->coll_reduce((char *) t->sbuf, (char *) t->rbuf, t->seg_count, t->dtype,
                                     t->op, t->root_low_rank, t->low_comm,
                                     t->low_comm->c_coll->coll_reduce_module);
    return OMPI_SUCCESS;
}

/* t1 task */
int mca_coll_han_allreduce_t1_task(void *task_argu)
{
    mca_allreduce_argu_t *t = (mca_allreduce_argu_t *) task_argu;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                         "[%d] HAN Allreduce:  t1 %d r_buf %d\n", t->w_rank, t->cur_seg,
                         ((int *) t->rbuf)[0]));
    OBJ_RELEASE(t->cur_task);
    ptrdiff_t extent, lb;
    ompi_datatype_get_extent(t->dtype, &lb, &extent);
    ompi_request_t *ireduce_req;
    int tmp_count = t->seg_count;
    if (!t->noop) {
        int up_rank = ompi_comm_rank(t->up_comm);
        /* ur of cur_seg */
        if (up_rank == t->root_up_rank) {
            t->up_comm->c_coll->coll_ireduce(MPI_IN_PLACE, (char *) t->rbuf, t->seg_count, t->dtype,
                                             t->op, t->root_up_rank, t->up_comm, &ireduce_req,
                                             t->up_comm->c_coll->coll_ireduce_module);
        } else {
            t->up_comm->c_coll->coll_ireduce((char *) t->rbuf, (char *) t->rbuf, t->seg_count,
                                             t->dtype, t->op, t->root_up_rank, t->up_comm,
                                             &ireduce_req, t->up_comm->c_coll->coll_ireduce_module);
        }
    }
    /* lr of cur_seg+1 */
    if (t->cur_seg <= t->num_segments - 2) {
        if (t->cur_seg == t->num_segments - 2 && t->last_seg_count != t->seg_count) {
            tmp_count = t->last_seg_count;
        }
        t->low_comm->c_coll->coll_reduce((char *) t->sbuf + extent * t->seg_count,
                                         (char *) t->rbuf + extent * t->seg_count, tmp_count,
                                         t->dtype, t->op, t->root_low_rank, t->low_comm,
                                         t->low_comm->c_coll->coll_reduce_module);

    }
    if (!t->noop) {
        ompi_request_wait(&ireduce_req, MPI_STATUSES_IGNORE);
    }

    return OMPI_SUCCESS;
}

/* t2 task */
int mca_coll_han_allreduce_t2_task(void *task_argu)
{
    mca_allreduce_argu_t *t = (mca_allreduce_argu_t *) task_argu;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                         "[%d] HAN Allreduce:  t2 %d r_buf %d\n", t->w_rank, t->cur_seg,
                         ((int *) t->rbuf)[0]));
    OBJ_RELEASE(t->cur_task);
    ptrdiff_t extent, lb;
    ompi_datatype_get_extent(t->dtype, &lb, &extent);
    ompi_request_t *reqs[2];
    int req_count = 0;
    int tmp_count = t->seg_count;
    if (!t->noop) {
        int up_rank = ompi_comm_rank(t->up_comm);
        /* ub of cur_seg */
        t->up_comm->c_coll->coll_ibcast((char *) t->rbuf, t->seg_count, t->dtype, t->root_up_rank,
                                        t->up_comm, &(reqs[0]),
                                        t->up_comm->c_coll->coll_ibcast_module);
        req_count++;
        /* ur of cur_seg+1 */
        if (t->cur_seg <= t->num_segments - 2) {
            if (t->cur_seg == t->num_segments - 2 && t->last_seg_count != t->seg_count) {
                tmp_count = t->last_seg_count;
            }
            if (up_rank == t->root_up_rank) {
                t->up_comm->c_coll->coll_ireduce(MPI_IN_PLACE,
                                                 (char *) t->rbuf + extent * t->seg_count,
                                                 tmp_count, t->dtype, t->op, t->root_up_rank,
                                                 t->up_comm, &(reqs[1]),
                                                 t->up_comm->c_coll->coll_ireduce_module);
            } else {
                t->up_comm->c_coll->coll_ireduce((char *) t->rbuf + extent * t->seg_count,
                                                 (char *) t->rbuf + extent * t->seg_count,
                                                 tmp_count, t->dtype, t->op, t->root_up_rank,
                                                 t->up_comm, &(reqs[1]),
                                                 t->up_comm->c_coll->coll_ireduce_module);
            }
            req_count++;
        }
    }
    /* lr of cur_seg+2 */
    if (t->cur_seg <= t->num_segments - 3) {
        if (t->cur_seg == t->num_segments - 3 && t->last_seg_count != t->seg_count) {
            tmp_count = t->last_seg_count;
        }
        t->low_comm->c_coll->coll_reduce((char *) t->sbuf + 2 * extent * t->seg_count,
                                         (char *) t->rbuf + 2 * extent * t->seg_count, tmp_count,
                                         t->dtype, t->op, t->root_low_rank, t->low_comm,
                                         t->low_comm->c_coll->coll_reduce_module);
    }
    if (!t->noop && req_count > 0) {
        ompi_request_wait_all(req_count, reqs, MPI_STATUSES_IGNORE);
    }


    return OMPI_SUCCESS;
}

/* t3 task */
int mca_coll_han_allreduce_t3_task(void *task_argu)
{
    mca_allreduce_argu_t *t = (mca_allreduce_argu_t *) task_argu;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                         "[%d] HAN Allreduce:  t3 %d r_buf %d\n", t->w_rank, t->cur_seg,
                         ((int *) t->rbuf)[0]));
    OBJ_RELEASE(t->cur_task);
    ptrdiff_t extent, lb;
    ompi_datatype_get_extent(t->dtype, &lb, &extent);
    ompi_request_t *reqs[2];
    int req_count = 0;
    int tmp_count = t->seg_count;
    if (!t->noop) {
        int up_rank = ompi_comm_rank(t->up_comm);
        /* ub of cur_seg+1 */
        if (t->cur_seg <= t->num_segments - 2) {
            if (t->cur_seg == t->num_segments - 2 && t->last_seg_count != t->seg_count) {
                tmp_count = t->last_seg_count;
            }
            t->up_comm->c_coll->coll_ibcast((char *) t->rbuf + extent * t->seg_count, t->seg_count,
                                            t->dtype, t->root_up_rank, t->up_comm, &(reqs[0]),
                                            t->up_comm->c_coll->coll_ibcast_module);
            req_count++;
        }
        /* ur of cur_seg+2 */
        if (t->cur_seg <= t->num_segments - 3) {
            if (t->cur_seg == t->num_segments - 3 && t->last_seg_count != t->seg_count) {
                tmp_count = t->last_seg_count;
            }
            if (up_rank == t->root_up_rank) {
                t->up_comm->c_coll->coll_ireduce(MPI_IN_PLACE,
                                                 (char *) t->rbuf + 2 * extent * t->seg_count,
                                                 tmp_count, t->dtype, t->op, t->root_up_rank,
                                                 t->up_comm, &(reqs[1]),
                                                 t->up_comm->c_coll->coll_ireduce_module);
            } else {
                t->up_comm->c_coll->coll_ireduce((char *) t->rbuf + 2 * extent * t->seg_count,
                                                 (char *) t->rbuf + 2 * extent * t->seg_count,
                                                 tmp_count, t->dtype, t->op, t->root_up_rank,
                                                 t->up_comm, &(reqs[1]),
                                                 t->up_comm->c_coll->coll_ireduce_module);
            }
            req_count++;
        }
    }
    /* lr of cur_seg+3 */
    if (t->cur_seg <= t->num_segments - 4) {
        if (t->cur_seg == t->num_segments - 4 && t->last_seg_count != t->seg_count) {
            tmp_count = t->last_seg_count;
        }
        t->low_comm->c_coll->coll_reduce((char *) t->sbuf + 3 * extent * t->seg_count,
                                         (char *) t->rbuf + 3 * extent * t->seg_count, tmp_count,
                                         t->dtype, t->op, t->root_low_rank, t->low_comm,
                                         t->low_comm->c_coll->coll_reduce_module);
    }
    /* lb of cur_seg */
    t->low_comm->c_coll->coll_bcast((char *) t->rbuf, t->seg_count, t->dtype, t->root_low_rank,
                                    t->low_comm, t->low_comm->c_coll->coll_bcast_module);
    if (!t->noop && req_count > 0) {
        ompi_request_wait_all(req_count, reqs, MPI_STATUSES_IGNORE);
    }

    t->completed[0]++;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                         "[%d] HAN Allreduce:  t3 %d total %d\n", t->w_rank, t->cur_seg,
                         t->completed[0]));

    return OMPI_SUCCESS;
}
