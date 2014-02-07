/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file */

#include "ompi_config.h"

#include "ompi/constants.h"
#include "opal/threads/mutex.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/coll/coll.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/coll/ml/coll_ml.h"
#include "ompi/mca/coll/ml/coll_ml_inlines.h"

static void mca_coll_ml_barrier_task_setup(
                mca_coll_ml_task_status_t *task_status,
                int index, mca_coll_ml_compound_functions_t *func)
{
    task_status->rt_num_dependencies = func->num_dependencies;
    task_status->rt_num_dependent_tasks = func->num_dependent_tasks;
    task_status->rt_dependent_task_indices = func->dependent_task_indices;
}

static int mca_coll_ml_barrier_launch(mca_coll_ml_module_t *ml_module,
                                     ompi_request_t **req)
{
    ompi_free_list_item_t *item;
    mca_coll_ml_collective_operation_progress_t *coll_op;
    mca_bcol_base_payload_buffer_desc_t *src_buffer_desc = NULL;
    
    /* allocate an ml buffer for signaling purposes */
    src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);

    while (NULL == src_buffer_desc) {
        opal_progress();
        src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
    }

    
    /* Blocking call on fragment allocation (Maybe we want to make it non blocking ?) */
    OMPI_FREE_LIST_WAIT_MT(&(ml_module->coll_ml_collective_descriptors),
                        item);

    coll_op = (mca_coll_ml_collective_operation_progress_t *) item;
    assert(NULL != coll_op);

    ML_VERBOSE(10, ("Get coll request %p", coll_op));

    MCA_COLL_ML_OP_BASIC_SETUP(coll_op, 0, 0, NULL, NULL, ml_module->coll_ml_barrier_function);

    coll_op->fragment_data.buffer_desc = src_buffer_desc;
    coll_op->dag_description.num_tasks_completed = 0;

    coll_op->variable_fn_params.buffer_index = src_buffer_desc->buffer_index;

    coll_op->variable_fn_params.sequence_num =
        OPAL_THREAD_ADD32(&(ml_module->collective_sequence_num), 1);

    /* Pointer to a coll finalize function */
    coll_op->process_fn = NULL;

    (*req) = &coll_op->full_message.super;

    OMPI_REQUEST_INIT((*req), false);

    (*req)->req_status._cancelled = 0;
    (*req)->req_state = OMPI_REQUEST_ACTIVE;
    (*req)->req_status.MPI_ERROR = OMPI_SUCCESS;

    /* Set order info if there is a bcol needs ordering */
    MCA_COLL_ML_SET_ORDER_INFO(coll_op, 1);

    return mca_coll_ml_generic_collectives_launcher(coll_op, mca_coll_ml_barrier_task_setup);
}

/**
 * Hierarchical blocking barrier
 */
int mca_coll_ml_barrier_intra(struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module)
{
    int rc;
    ompi_request_t *req;

    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t *) module;

#if OPAL_ENABLE_DEBUG
    static int barriers_count = 0;
#endif

    ML_VERBOSE(10, ("Barrier num %d start.", ++barriers_count));

    rc = mca_coll_ml_barrier_launch(ml_module, &req);
    if (OPAL_UNLIKELY(rc != OMPI_SUCCESS)) {
        ML_ERROR(("Failed to launch a barrier."));
        return rc;
    }

    /* Blocking barrier */
    ompi_request_wait_completion(req);
    ompi_request_free(&req);

    ML_VERBOSE(10, ("Barrier num %d was done.", barriers_count));

    return OMPI_SUCCESS;
}

/**
 * Hierarchical non-blocking barrier
 */
int mca_coll_ml_ibarrier_intra(struct ompi_communicator_t *comm,
                               ompi_request_t **req,
                               mca_coll_base_module_t *module)
{
    int rc;
    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t *) module;

#if OPAL_ENABLE_DEBUG
    static int barriers_count = 0;
#endif

    ML_VERBOSE(10, ("IBarrier num %d start.", ++barriers_count));

    rc = mca_coll_ml_barrier_launch(ml_module, req);
    if (OPAL_UNLIKELY(rc != OMPI_SUCCESS)) {
        ML_ERROR(("Failed to launch a barrier."));
        return rc;
    }

    ML_VERBOSE(10, ("IBarrier num %d was done.", barriers_count));

    return OMPI_SUCCESS;
}
