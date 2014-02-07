/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
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
#include "ompi/mca/coll/ml/coll_ml_allocation.h"

static int mca_coll_ml_memsync_recycle_memory(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t *)coll_op->coll_module;
    mca_bcol_base_memory_block_desc_t *ml_memblock = ml_module->payload_block;
    mca_coll_ml_collective_operation_progress_t *pending_op = NULL;
    int bank = coll_op->full_message.bank_index_to_recycle;
    int rc;
    bool have_resources = true;

    assert(bank >= 0 || 
           bank < (int)ml_memblock->num_banks ||
           ML_MEMSYNC == coll_op->fragment_data.current_coll_op);

    ML_VERBOSE(10,("MEMSYNC: bank %d was recycled coll_op %p", bank, coll_op));

    /* set the bank as free */

    ml_memblock->bank_is_busy[bank] = false;
    ml_memblock->bank_release_counters[bank] = 0;

    /* Check if we have any requests that are waiting for memory */
    while(opal_list_get_size(&ml_module->waiting_for_memory_list) && have_resources) {
        pending_op = (mca_coll_ml_collective_operation_progress_t *)
            opal_list_get_first(&ml_module->waiting_for_memory_list);

        ML_VERBOSE(10, ("Trying to start pending %p", pending_op));
        assert(pending_op->pending & REQ_OUT_OF_MEMORY);
        rc = pending_op->fragment_data.message_descriptor->fragment_launcher(pending_op);
        switch (rc) {
            case OMPI_SUCCESS: 
                ML_VERBOSE(10, ("Pending fragment was started %p", pending_op));
                pending_op->pending ^= REQ_OUT_OF_MEMORY;
                opal_list_remove_item(&ml_module->waiting_for_memory_list,
                        (opal_list_item_t *)pending_op);
                if (0 != pending_op->fragment_data.offset_into_user_buffer) {
                    /* non-zero offset ==> this is not fragment 0 */
                    CHECK_AND_RECYCLE(pending_op);
                }
                break;
            case OMPI_ERR_TEMP_OUT_OF_RESOURCE: 
                ML_VERBOSE(10, ("Already on the list %p", pending_op));
                have_resources = false;
                break;
            default:
                ML_ERROR(("Error happened %d", rc));
                return rc;
        }
    }

    ML_VERBOSE(10, ("Memsync done %p", coll_op));
    return OMPI_SUCCESS;
}

static void mca_coll_ml_barrier_task_setup(
                mca_coll_ml_task_status_t *task_status,
                int index, mca_coll_ml_compound_functions_t *func)
{
    task_status->rt_num_dependencies = func->num_dependencies;
    task_status->rt_num_dependent_tasks = func->num_dependent_tasks;
    task_status->rt_dependent_task_indices = func->dependent_task_indices;
}

static inline __opal_attribute_always_inline__ int mca_coll_ml_memsync_launch(mca_coll_ml_module_t *ml_module,
                                     ompi_request_t **req, int bank_index)
{
    mca_coll_ml_collective_operation_progress_t *coll_op;

    coll_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
            ml_module->coll_ml_memsync_function,
            NULL, NULL, 0, 0);

    assert(NULL != coll_op);

    ML_VERBOSE(10, ("Get coll request %p", coll_op));

    coll_op->fragment_data.buffer_desc = NULL;
    
    /* Caching bank index for future memory recycling callback */
    coll_op->full_message.bank_index_to_recycle = bank_index;

    coll_op->fragment_data.current_coll_op = ML_MEMSYNC;
    /* I don't want to define one more parameter, so under root
     * we pass buffer index */
    coll_op->variable_fn_params.root = bank_index;
    /* As well it's little bit ugly, since it is no wait for this request,
     * in order to recycle it we have to set offset to some value > 1 */
    coll_op->fragment_data.offset_into_user_buffer = 1;
    coll_op->variable_fn_params.buffer_index = MCA_COLL_ML_NO_BUFFER;
    coll_op->variable_fn_params.sequence_num = -1; /* It should be safe to use -1 */
    /* Pointer to a coll finalize function */
    if (OPAL_LIKELY(ml_module->initialized)) {
        coll_op->process_fn = mca_coll_ml_memsync_recycle_memory;
    } else {
        /* No post work on first call */
        coll_op->process_fn = NULL;
    }

    ML_VERBOSE(10,("Memsync start %p", &coll_op));

    return mca_coll_ml_generic_collectives_append_to_queue(coll_op, mca_coll_ml_barrier_task_setup);
}

/**
 * Non blocking memory syncronization
 */
int mca_coll_ml_memsync_intra(mca_coll_ml_module_t *ml_module, int bank_index)
{
    int rc;
    ompi_request_t *req;

    ML_VERBOSE(8, ("MEMSYNC start"));

    if (OPAL_UNLIKELY(0 == opal_list_get_size(&ml_module->active_bcols_list))) {
        /* Josh's change: In the case where only p2p is active, we have no way
         * to reset the bank release counters to zero, I am doing that here since it
         * would actually be "correct" to do it outside of this conditional, however
         * I suspect that reseting the value to zero elsewhere would result in corrupted 
         * flow for non-contiguous data types
         */
        
        /* nasty hack to ensure that resources are released in the single level 
         * ptp case. 
         */
        mca_coll_ml_collective_operation_progress_t dummy_coll;

        dummy_coll.coll_module = (mca_coll_base_module_t *) ml_module;
        dummy_coll.fragment_data.current_coll_op = ML_MEMSYNC;
        dummy_coll.full_message.bank_index_to_recycle = bank_index;

        /* Handling special case when memory syncronization is not required */
        rc = mca_coll_ml_memsync_recycle_memory(&dummy_coll);
        if(OPAL_UNLIKELY(rc != OMPI_SUCCESS)){
            ML_ERROR(("Failed to flush the list."));
            return rc;
        } 
    } else {
        /* retain the communicator until the operation is finished. the communicator
         * will be released by CHECK_AND_RECYCLE */
        OBJ_RETAIN(ml_module->comm);

        rc = mca_coll_ml_memsync_launch(ml_module, &req, bank_index);
        if (OPAL_UNLIKELY(rc != OMPI_SUCCESS)) {
            ML_ERROR(("Failed to launch a barrier."));
            return rc;
        }
    }

    return OMPI_SUCCESS;
}
