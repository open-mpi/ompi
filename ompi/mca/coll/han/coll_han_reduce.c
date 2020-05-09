/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "coll_han.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/pml/pml.h"
#include "coll_han_trigger.h"

void mac_coll_han_set_reduce_argu(mca_reduce_argu_t * argu, mca_coll_task_t * cur_task, void *sbuf, void *rbuf,
                                  int seg_count, struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                  int root_up_rank, int root_low_rank,
                                  struct ompi_communicator_t *up_comm,
                                  struct ompi_communicator_t *low_comm,
                                  int num_segments, int cur_seg, int w_rank, int last_seg_count,
                                  bool noop)
{
    argu->cur_task = cur_task;
    argu->sbuf = sbuf;
    argu->rbuf = rbuf;
    argu->seg_count = seg_count;
    argu->dtype = dtype;
    argu->op = op;
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
 * Each segment of the messsage needs to go though 2 steps to perform MPI_Reduce: 
 *     lb: low level (shared-memory or intra-node) reduce.
*     ub: upper level (inter-node) reduce
 * Hence, in each iteration, there is a combination of collective operations which is called a task.
 *        | seg 0 | seg 1 | seg 2 | seg 3 |
 * iter 0 |  lr   |       |       |       | task: t0, contains lr
 * iter 1 |  ur   |  lr   |       |       | task: t1, contains ur and lr
 * iter 2 |       |  ur   |  lr   |       | task: t1, contains ur and lr
 * iter 3 |       |       |  ur   |  lr   | task: t1, contains ur and lr
 * iter 4 |       |       |       |  ur   | task: t1, contains ur
 */
int
mca_coll_han_reduce_intra(const void *sbuf, 
                          void *rbuf,
                          int count,
                          struct ompi_datatype_t *dtype,
                          ompi_op_t* op,
                          int root,
                          struct ompi_communicator_t *comm, 
                          mca_coll_base_module_t * module)
{
    ptrdiff_t extent, lb;
    ompi_datatype_get_extent(dtype, &lb, &extent);
    int w_rank;
    w_rank = ompi_comm_rank(comm);
    int seg_count = count;
    size_t typelng;
    ompi_datatype_type_size(dtype, &typelng);

    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *) module;
    /* Do not initialize topology if the operation cannot commute */
    if(!ompi_op_is_commute(op)){
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                    "han cannot handle reduce with this operation. It needs to fall back on another component\n"));
        goto prev_reduce_intra;
    }

    /* Topo must be initialized to know rank distribution which then is used to
     * determine if han can be used */
    mca_coll_han_topo_init(comm, han_module, 2);
    if (han_module->are_ppn_imbalanced){
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                    "han cannot handle reduce with this communicator. It needs to fall back on another component\n"));
        goto prev_reduce_intra;
    }

    /* Create the subcommunicators */
    mca_coll_han_comm_create(comm, han_module);
    ompi_communicator_t *low_comm;
    ompi_communicator_t *up_comm;

    /* use MCA parameters for now */
    low_comm = han_module->cached_low_comms[mca_coll_han_component.han_reduce_low_module];
    up_comm = han_module->cached_up_comms[mca_coll_han_component.han_reduce_up_module];
    COLL_BASE_COMPUTED_SEGCOUNT(mca_coll_han_component.han_reduce_segsize, typelng,
                                seg_count);

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
    mca_reduce_argu_t *t = malloc(sizeof(mca_reduce_argu_t));
    mac_coll_han_set_reduce_argu(t, t0, (char *) sbuf, (char *) rbuf, seg_count, dtype,
                                 op, root_up_rank, root_low_rank, up_comm, low_comm,
                                 num_segments, 0, w_rank, count - (num_segments - 1) * seg_count,
                                 low_rank != root_low_rank);
    /* Init the first task */
    init_task(t0, mca_coll_han_reduce_t0_task, (void *) t);
    issue_task(t0);

    /* Create t1 task */
    mca_coll_task_t *t1 = OBJ_NEW(mca_coll_task_t);
    /* Setup up t1 task arguments */
    t->cur_task = t1;
    /* Init the t1 task */
    init_task(t1, mca_coll_han_reduce_t1_task, (void *) t);
    issue_task(t1);

    while (t->cur_seg <= t->num_segments - 2) {
        /* Create t1 task */
        mca_coll_task_t *t1 = OBJ_NEW(mca_coll_task_t);
        /* Setup up t1 task arguments */
        t->cur_task = t1;
        t->sbuf = (char *) t->sbuf + extent * t->seg_count;
        t->rbuf = (char *) t->rbuf + extent * t->seg_count;
        t->cur_seg = t->cur_seg + 1;
        /* Init the t1 task */
        init_task(t1, mca_coll_han_reduce_t1_task, (void *) t);
        issue_task(t1);
    }

    free(t);

    return OMPI_SUCCESS;

prev_reduce_intra:
    return han_module->previous_reduce(sbuf, rbuf, count, dtype, op, root,
                                       comm,
                                       han_module->previous_reduce_module);
}

/* t0 task: issue and wait for the low level reduce of segment 0 */
int mca_coll_han_reduce_t0_task(void *task_argu)
{
    mca_reduce_argu_t *t = (mca_reduce_argu_t *) task_argu;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d]: in t0 %d\n", t->w_rank,
                         t->cur_seg));
    OBJ_RELEASE(t->cur_task);
    ptrdiff_t extent, lb;
    ompi_datatype_get_extent(t->dtype, &lb, &extent);
    t->low_comm->c_coll->coll_reduce((char *) t->sbuf, (char *) t->rbuf, t->seg_count, t->dtype,
                                     t->op, t->root_low_rank, t->low_comm,
                                     t->low_comm->c_coll->coll_reduce_module);
    return OMPI_SUCCESS;
}

/* t1 task */
int mca_coll_han_reduce_t1_task(void *task_argu) {
    mca_reduce_argu_t *t = (mca_reduce_argu_t *) task_argu;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d]: in t1 %d\n", t->w_rank,
                         t->cur_seg));
    OBJ_RELEASE(t->cur_task);
    ptrdiff_t extent, lb;
    ompi_datatype_get_extent(t->dtype, &lb, &extent);
    ompi_request_t *ireduce_req = NULL;
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
    if (!t->noop && ireduce_req) {
        ompi_request_wait(&ireduce_req, MPI_STATUSES_IGNORE);
    }

    return OMPI_SUCCESS;
}

/* In case of non regular situation (imbalanced number of processes per nodes),
 * a fallback is made on the next component that provides a reduce in priority order */
int
mca_coll_han_reduce_intra_simple(const void *sbuf,
                                     void* rbuf,
                                     int count,
                                     struct ompi_datatype_t *dtype,
                                     ompi_op_t *op,
                                     int root,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module)
{
    int w_rank; /* information about the global communicator */
    int root_low_rank, root_up_rank; /* root ranks for both sub-communicators */
    int ret;
    int *vranks, low_rank, low_size;
    ptrdiff_t rsize, rgap = 0;
    void * tmp_buf;

    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;

    /* Do not initialize topology if the operation cannot commute */
    if(!ompi_op_is_commute(op)){
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                    "han cannot handle reduce with this operation. It needs to fall back on another component\n"));
        goto prev_reduce_intra_simple;
    }

    /* Topo must be initialized to know rank distribution which then is used to
     * determine if han can be used */
    mca_coll_han_topo_init(comm, han_module, 2);
    if (han_module->are_ppn_imbalanced){
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                    "han cannot handle reduce with this communicator. It needs to fall back on another component\n"));
        goto prev_reduce_intra_simple;
    }

    mca_coll_han_comm_create(comm, han_module);
    ompi_communicator_t *low_comm =
         han_module->cached_low_comms[mca_coll_han_component.han_reduce_low_module];
    ompi_communicator_t *up_comm =
         han_module->cached_up_comms[mca_coll_han_component.han_reduce_up_module];

    /* Get the 'virtual ranks' mapping corresponding to the communicators */
    vranks = han_module->cached_vranks;
    w_rank = ompi_comm_rank(comm);
    low_rank = ompi_comm_rank(low_comm);

    low_size = ompi_comm_size(low_comm);
    /* Get root ranks for low and up comms */
    mca_coll_han_get_ranks(vranks, root, low_size, &root_low_rank, &root_up_rank);

    if (root_low_rank == low_rank && w_rank != root) {
        rsize = opal_datatype_span(&dtype->super, (int64_t)count, &rgap);
        tmp_buf = malloc(rsize);
        if (NULL == tmp_buf) {
            return OMPI_ERROR;
        }
    } else {
        /* global root rbuf is valid, local non-root do not need buffers */
        tmp_buf = rbuf;
    }
    /* No need to handle MPI_IN_PLACE: only the global root may ask for it and
     * it is ok to use it for intermediary reduces since it is also a local root*/

    /* Low_comm reduce */
    ret = low_comm->c_coll->coll_reduce((char *)sbuf, (char *)tmp_buf,
                count, dtype, op, root_low_rank,
                low_comm, low_comm->c_coll->coll_reduce_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)){
        if (root_low_rank == low_rank && w_rank != root){
            free(tmp_buf);
        }
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/REDUCE: low comm reduce failed. "
                             "Falling back to another component\n"));
        goto prev_reduce_intra_simple;
    }

    /* Up_comm reduce */
    if (root_low_rank == low_rank ){
        if(w_rank != root){
            ret = up_comm->c_coll->coll_reduce((char *)tmp_buf, NULL,
                        count, dtype, op, root_up_rank,
                        up_comm, up_comm->c_coll->coll_reduce_module);
            free(tmp_buf);
        } else {
            /* Take advantage of any optimisation made for IN_PLACE
             * communcations */
            ret = up_comm->c_coll->coll_reduce(MPI_IN_PLACE, (char *)tmp_buf,
                        count, dtype, op, root_up_rank,
                        up_comm, up_comm->c_coll->coll_reduce_module);
        }
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)){
            OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                                 "HAN/REDUCE: low comm reduce failed.\n"));
            return ret;
        }

    }
    return OMPI_SUCCESS;

prev_reduce_intra_simple:
    return han_module->previous_reduce(sbuf, rbuf, count, dtype, op, root,
                                       comm,
                                       han_module->previous_reduce_module);
}


/* Find a fallback on reproducible algorithm
 * use tuned or basic or if impossible whatever available
 */
int
mca_coll_han_reduce_reproducible_decision(struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module)
{
    int w_rank = ompi_comm_rank(comm);
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;

    /* populate previous modules_storage*/
    mca_coll_han_get_all_coll_modules(comm, han_module);

    /* try availability of reproducible modules */
    int fallbacks[] = {TUNED, BASIC};
    int fallbacks_len = sizeof(fallbacks) / sizeof(*fallbacks);
    int i;
    for (i=0; i<fallbacks_len; i++) {
        int fallback = fallbacks[i];
        mca_coll_base_module_t *fallback_module = han_module->modules_storage
            .modules[fallback]
            .module_handler;
        if (fallback_module != NULL && fallback_module->coll_reduce != NULL) {
            if (0 == w_rank) {
                opal_output_verbose(30, mca_coll_han_component.han_output,
                                    "coll:han:reduce_reproducible: "
                                    "fallback on %s\n",
                                    components_name[fallback]);
            }
            han_module->reproducible_reduce_module = fallback_module;
            han_module->reproducible_reduce = fallback_module->coll_reduce;
            return OMPI_SUCCESS;
        }
    }
   /* fallback of the fallback */
    if (0 == w_rank) {
        opal_output_verbose(5, mca_coll_han_component.han_output,
                            "coll:han:reduce_reproducible_decision: "
                            "no reproducible fallback\n");
    }
    han_module->reproducible_reduce_module =
        han_module->previous_reduce_module;
    han_module->reproducible_reduce = han_module->previous_reduce;
    return  OMPI_SUCCESS;
}


/* Fallback on reproducible algorithm */
int
mca_coll_han_reduce_reproducible(const void *sbuf,
                                 void *rbuf,
                                  int count,
                                  struct ompi_datatype_t *dtype,
                                  struct ompi_op_t *op,
                                  int root,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;
    return han_module->reproducible_reduce(sbuf, rbuf, count, dtype,
                                           op, root, comm,
                                           han_module
                                           ->reproducible_reduce_module);
}
