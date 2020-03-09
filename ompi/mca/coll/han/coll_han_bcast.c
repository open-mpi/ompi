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

void mac_coll_han_set_bcast_argu(mca_bcast_argu_t * argu, mca_coll_task_t * cur_task, void *buff,
                                 int seg_count, struct ompi_datatype_t *dtype,
                                 int root_up_rank, int root_low_rank,
                                 struct ompi_communicator_t *up_comm,
                                 struct ompi_communicator_t *low_comm,
                                 int num_segments, int cur_seg, int w_rank, int last_seg_count,
                                 bool noop)
{
    argu->cur_task = cur_task;
    argu->buff = buff;
    argu->seg_count = seg_count;
    argu->dtype = dtype;
    argu->root_low_rank = root_low_rank;
    argu->root_up_rank = root_up_rank;
    argu->up_comm = up_comm;
    argu->low_comm = low_comm;
    argu->num_segments = num_segments;
    argu->cur_seg = cur_seg;
    argu->w_rank = w_rank;
    argu->last_seg_count = last_seg_count;
    argu->noop = noop;
}

/* 
 * Each segment of the messsage needs to go though 2 steps to perform MPI_Bcast: 
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
mca_coll_han_bcast_intra(void *buff,
                         int count,
                         struct ompi_datatype_t *dtype,
                         int root,
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
            c * mca_coll_han_component.han_auto_tune_m + m;
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
                adapt_ibcast_segment_size = us;
        }
        */

    } else {
        /* If auto tune is disabled, use MCA parameters */
        low_comm = han_module->cached_low_comms[mca_coll_han_component.han_bcast_low_module];
        up_comm = han_module->cached_up_comms[mca_coll_han_component.han_bcast_up_module];
        COLL_BASE_COMPUTED_SEGCOUNT(mca_coll_han_component.han_bcast_segsize, typelng,
                                    seg_count);
    }

    int num_segments = (count + seg_count - 1) / seg_count;
    OPAL_OUTPUT_VERBOSE((20, mca_coll_han_component.han_output,
                         "In HAN seg_count %d count %d num_seg %d\n", 
                         seg_count, count, num_segments));

    int *vranks = han_module->cached_vranks;
    int low_rank = ompi_comm_rank(low_comm);
    int low_size = ompi_comm_size(low_comm);

    int root_low_rank;
    int root_up_rank;
    mca_coll_han_get_ranks(vranks, root, low_size, &root_low_rank, &root_up_rank);
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                         "[%d]: root_low_rank %d root_up_rank %d\n", w_rank, root_low_rank,
                         root_up_rank));

    /* Create t0 tasks for the first segment */
    mca_coll_task_t *t0 = OBJ_NEW(mca_coll_task_t);
    /* Setup up t0 task arguments */
    mca_bcast_argu_t *t = malloc(sizeof(mca_bcast_argu_t));
    mac_coll_han_set_bcast_argu(t, t0, (char *) buff, seg_count, dtype,
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
        mca_coll_task_t *t1 = OBJ_NEW(mca_coll_task_t);
        /* Setup up t1 task arguments */
        t->cur_task = t1;
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
int mca_coll_han_bcast_t0_task(void *task_argu)
{
    mca_bcast_argu_t *t = (mca_bcast_argu_t *) task_argu;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d]: in t0 %d\n", t->w_rank,
                         t->cur_seg));
    OBJ_RELEASE(t->cur_task);
    if (t->noop) {
        return OMPI_SUCCESS;
    } else {
        ptrdiff_t extent, lb;
        ompi_datatype_get_extent(t->dtype, &lb, &extent);
        ompi_request_t *ibcast_req;
        t->up_comm->c_coll->coll_ibcast((char *) t->buff, t->seg_count, t->dtype, t->root_up_rank, 
                                        t->up_comm, &ibcast_req, t->up_comm->c_coll->coll_ibcast_module);
        ompi_request_wait(&ibcast_req, MPI_STATUSES_IGNORE);
        return OMPI_SUCCESS;
    }
}

/* t1 task: 
 * 1. issue the upper level ibcast of segment cur_seg + 1
 * 2. issue the low level bcast of segment cur_seg
 * 3. wait for the completion of the ibcast
 */
int mca_coll_han_bcast_t1_task(void *task_argu)
{
    mca_bcast_argu_t *t = (mca_bcast_argu_t *) task_argu;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d]: in t1 %d\n", t->w_rank,
                         t->cur_seg));
    OBJ_RELEASE(t->cur_task);
    ptrdiff_t extent, lb;
    ompi_datatype_get_extent(t->dtype, &lb, &extent);
    ompi_request_t *ibcast_req = NULL;
    int tmp_count = t->seg_count;
    if (!t->noop) {
        if (t->cur_seg <= t->num_segments - 2 ) {
            if (t->cur_seg == t->num_segments - 2  && t->last_seg_count != t->seg_count) {
                tmp_count = t->last_seg_count;
            }
            t->up_comm->c_coll->coll_ibcast((char *) t->buff + extent * t->seg_count,
                                            tmp_count, t->dtype, t->root_up_rank,
                                            t->up_comm, &ibcast_req,
                                            t->up_comm->c_coll->coll_ibcast_module);
        }
    }

    t->low_comm->c_coll->coll_bcast((char *) t->buff,
                                    t->seg_count, t->dtype, t->root_low_rank, t->low_comm,
                                    t->low_comm->c_coll->coll_bcast_module);

    if (!t->noop && ibcast_req != NULL) {
        ompi_request_wait(&ibcast_req, MPI_STATUSES_IGNORE);
    }

    return OMPI_SUCCESS;
}
