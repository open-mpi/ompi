/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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

#ifndef MCA_COLL_ML_INLINES_H
#define MCA_COLL_ML_INLINES_H

#include "ompi_config.h"

BEGIN_C_DECLS

static inline __opal_attribute_always_inline__ int ml_fls(int num)
{
    int i = 1;
    int j = 0;

    if (0 == num) {
        return 0;
    }

    while (i < num) {
        i *= 2;
        j++;
    }

    if (i > num) {
        j--;
    }

   return j;
}

static inline __opal_attribute_always_inline__
        int mca_coll_ml_buffer_recycling(mca_coll_ml_collective_operation_progress_t *ml_request)
{
    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t *)ml_request->coll_module;
    mca_bcol_base_memory_block_desc_t *ml_memblock = ml_module->payload_block;
    uint64_t bank_index = ml_request->fragment_data.buffer_desc->bank_index;
    int rc;

    opal_atomic_add(&ml_memblock->bank_release_counters[bank_index], 1);

    /* Check if the bank is ready for recycling */
    if (ml_memblock->bank_release_counters[bank_index] ==
            ml_memblock->num_buffers_per_bank ) {
        ml_memblock->ready_for_memsync[bank_index] = true;

        ML_VERBOSE(10, ("Sync count %d, bank %d", ml_memblock->memsync_counter, bank_index));
        assert(ml_memblock->bank_is_busy);
        if (ml_memblock->memsync_counter == (int)bank_index) {
            while(ml_memblock->ready_for_memsync[ml_memblock->memsync_counter]) {
                ML_VERBOSE(10, ("Calling for service barrier: ml_buffer_index - %d %d %d == %d.",
                            ml_request->fragment_data.buffer_desc->buffer_index,
                            ml_memblock->memsync_counter,
                            ml_memblock->bank_release_counters[ml_memblock->memsync_counter],
                            ml_memblock->num_buffers_per_bank));
                /* Setting the ready flag to 0 - unready - done */
                ml_memblock->ready_for_memsync[ml_memblock->memsync_counter] = false;

                rc = mca_coll_ml_memsync_intra(ml_module, ml_memblock->memsync_counter);
                if (OMPI_SUCCESS != rc) {
                    ML_ERROR(("Failed to start memory sync !!!"));
                    return rc;
                }

                opal_atomic_add(&ml_memblock->memsync_counter, 1);
                if (ml_memblock->memsync_counter == (int)ml_memblock->num_banks) {
                    ml_memblock->memsync_counter = 0;
                }
                ML_VERBOSE(10, ("After service barrier."));
            }
        } else {
            ML_VERBOSE(10, ("Out of order %d", ml_memblock->memsync_counter));
        }
    }

    return OMPI_SUCCESS;
}

static inline  __opal_attribute_always_inline__ int coll_ml_fragment_completion_processing(
        mca_coll_ml_collective_operation_progress_t *coll_op)
{
    /* local variables */
    int ret = OMPI_SUCCESS;
    size_t bytes_in_this_frag;
    struct full_message_t *full_msg_desc = coll_op->fragment_data.message_descriptor;
    bool ready_to_release = true, out_of_resource = false;

    ML_VERBOSE(10, ("Coll_op %p processing completion", coll_op));
    /* Call unpack/pack function */
    if (OPAL_LIKELY(NULL != coll_op->process_fn)) {
        ret = coll_op->process_fn(coll_op);
        switch(ret) {
            case OMPI_SUCCESS:
                ML_VERBOSE(10, ("unpack done"));
                ready_to_release = true;
                break;
            case ORTE_ERR_NO_MATCH_YET:
                ML_VERBOSE(10, ("unexpected packet"));
                ready_to_release = false;
                break;
            default:
                ML_ERROR(("Error, unexpected error code %d", ret));
                return ret;
        }
    }

    bytes_in_this_frag = coll_op->fragment_data.fragment_size;

    ML_VERBOSE(10, ("Delivered %d bytes in frag %d total %d",
                full_msg_desc->n_bytes_delivered,
                bytes_in_this_frag,
                full_msg_desc->n_bytes_total));

    /* check for full message completion */
    if(full_msg_desc->n_bytes_delivered + bytes_in_this_frag ==
            full_msg_desc->n_bytes_total) {
        /* message complete - don't update number of bytes delivered, just
         * mark the message complete
         */
        full_msg_desc->n_bytes_delivered += bytes_in_this_frag;

        /* decrement the number of fragments */
        full_msg_desc->n_active--;

        ML_VERBOSE(10, ("Signaling completion"));

        /* here we need to be sure that we point to the first fragment only */
        ompi_request_complete(&(coll_op->fragment_data.message_descriptor->super), true);
        coll_op->fragment_data.message_descriptor->super.req_status.MPI_ERROR = OMPI_SUCCESS;
    } else {
        assert(NULL != coll_op->fragment_data.buffer_desc);
        /* update the number of bytes delivered */
        full_msg_desc->n_bytes_delivered += bytes_in_this_frag;
        /* decrement the number of fragments */
        full_msg_desc->n_active--;
        /* here we need to start the next fragment */
        ML_VERBOSE(10, ("Launch frags for %p", coll_op));
        if (full_msg_desc->n_bytes_scheduled < full_msg_desc->n_bytes_total) {
            ret = coll_op->fragment_data.message_descriptor->fragment_launcher(coll_op);
            if (OPAL_UNLIKELY(OMPI_ERR_TEMP_OUT_OF_RESOURCE == ret)) {
                out_of_resource = true;
            } else if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                ML_VERBOSE(10, ("Failed to launch fragment"));
                return ret;
            }
        }
    }

    if (ready_to_release) {
        /* Check if we have to recycle memory.
         * Note: It is safe to recycle ML buffers since the ML buffer data
         * already was unpacked to user buffer
         */
         if (NULL != coll_op->fragment_data.buffer_desc) {
             ret = mca_coll_ml_buffer_recycling(coll_op);
             if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                 return ret;
             }
         }
        /* if this is not fragment 0, return fragment to the free list.
         * fragment 0 will be returned in mca_ml_request_free() which
         * is called from the MPI wait() and test() routines.
         * We can recover the pointer to the fragement descriptor from
         * the MPI level request object, wich is the first element
         * in the fragment descriptor.
         */
         /* I contend that this is a bug. This is not the right way to check
             * for the first fragment as it assumes that the first fragment would always
             * for any collective have zero as the first offset or that other subsequent
             * fragments would not. It is not safe to assume this. The correct check is
             * the following one
             */

        ML_VERBOSE(10, ("Master ? %p %d", coll_op,  coll_op->fragment_data.offset_into_user_buffer));
        /* This check is in fact a bug. Not the correct definiton of first
         * fragment. First fragment is the only fragment that satisfies the
         * following criteria
         */
        /*if (0 != coll_op->fragment_data.offset_into_user_buffer &&
                !out_of_resource) {
                */
        if (((&coll_op->full_message != coll_op->fragment_data.message_descriptor) &&
	     !out_of_resource) || IS_COLL_SYNCMEM(coll_op)) {
            /* non-zero offset ==> this is not fragment 0 */
            CHECK_AND_RECYCLE(coll_op);
        }
    }

    /* return */
    return OMPI_SUCCESS;
}

/* task completion */
static inline __opal_attribute_always_inline__ int coll_ml_task_dependency_processing(
        mca_coll_ml_task_status_t *task)
{
    /* update dependencies */
    mca_coll_ml_collective_operation_progress_t *my_schedule_instance =
        task->ml_coll_operation;
    int n_dependent_tasks = task->rt_num_dependent_tasks;
    int dep_task;

    for (dep_task = 0; dep_task < n_dependent_tasks; dep_task++)
    {
        int task_index;
        task_index = task->rt_dependent_task_indices[dep_task];
        my_schedule_instance->dag_description.status_array[task_index].n_dep_satisfied++;
    }

    /* return */
    return OMPI_SUCCESS;
}

/* collective task completion processing -
 * "task" may be removed from list in this routine.
 * Thread safety is assumed to be handled outside this routine.
 */
static inline __opal_attribute_always_inline__ int mca_coll_ml_task_completion_processing(
        mca_coll_ml_task_status_t **task_status_g, opal_list_t *list)
{
    /* local variables */
    int ret = OMPI_SUCCESS;
    mca_coll_ml_task_status_t *task_status = *task_status_g;

    mca_coll_ml_collective_operation_progress_t *coll_op =
        task_status->ml_coll_operation;

    /* Pasha: Since all our collectives so far use the root
       flag, I replacing the call for custom call back function
       with setting root_flag.
       If we will see that we need some custom functionality,
       we will enable it later.
     */

    task_status->ml_coll_operation->variable_fn_params.root_flag = true;

#if 0
    /* process task completion function,
       if any was defined  */
    if (OPAL_LIKELY(NULL != task_status->task_comp_fn)) {
        ret = task_status->task_comp_fn(task_status);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }
    }
#endif

    /* update dependencies */
    ret = coll_ml_task_dependency_processing(task_status);
    if (ret != OMPI_SUCCESS) {
        ML_VERBOSE(3,("Failed to coll_ml_task_dependency_processing"));
        return ret;
    }

    /* process task completion function,
       if any was defined  */
    if (OPAL_LIKELY(NULL != task_status->task_comp_fn)) {
        ret = task_status->task_comp_fn(task_status);
        if (ret != OMPI_SUCCESS) {
            ML_VERBOSE(3,("Failed to task_comp_fn"));
            return ret;
        }
    }

    /* remove the descriptor from the incomplete list
       (Pasha: if the list was provided) */
    /* No need to put this an any new list - it is associcated
     * with the mca_coll_ml_collective_operation_progress_t
     * descriptor already
     */

    if (NULL != list) {
        (*task_status_g) = (mca_coll_ml_task_status_t *)
            opal_list_remove_item(list, (opal_list_item_t *)(task_status));
    }

    /* update completion counter */
    coll_op->dag_description.num_tasks_completed++;

    if(coll_op->dag_description.num_tasks_completed ==
            coll_op->coll_schedule->n_fns)
    {
        /* the actual fragment descriptor is not on any list, as
         * we can get at it from the task descriptors
         */
        ret = coll_ml_fragment_completion_processing(coll_op);
        if (OMPI_SUCCESS != ret) {
            ML_VERBOSE(3,("Failed to coll_ml_fragment_completion_processing"));
            return ret;
        }
    }

    /* return */
    return ret;
}

static inline __opal_attribute_always_inline__ int mca_coll_ml_generic_collectives_append_to_queue(
                                        mca_coll_ml_collective_operation_progress_t  *op_prog,
                                        mca_coll_ml_task_setup_fn_t task_setup)
{
    int fn_index;
    mca_coll_ml_collective_operation_description_t *op_desc =
        op_prog->coll_schedule;
    mca_coll_ml_compound_functions_t *func = NULL;
    mca_coll_ml_task_status_t *task_status = NULL;
    mca_coll_ml_component_t *cm = &mca_coll_ml_component;

    ML_VERBOSE(9, ("Calling mca_coll_ml_generic_collectives_launcher"));

    /* Init all tasks, before we start them */
    for (fn_index = 0; fn_index < op_desc->n_fns; fn_index++) {
        func = &op_desc->component_functions[fn_index];
        task_status = &op_prog->dag_description.status_array[fn_index];

        ML_VERBOSE(9, ("Processing function index %d", fn_index));

        assert(NULL != func);

        /* Init task status */
        task_status->n_dep_satisfied = 0; /* start from zero */
        task_status->bcol_fn = func->bcol_function;
        /* setup run time parametres */
        /* Pasha: do we need the if proctection ? */
        if (OPAL_LIKELY(NULL != task_setup)) {
            task_setup(task_status, fn_index, func);
        }

        /* the pointer to operation progress supposed to be set during
           construction time. Just want to make sure that it is ok */
        assert(task_status->ml_coll_operation == op_prog);

        /* We assume that all pointer to functions are defined and it
         is not reson to check for null */
        assert(NULL != func->bcol_function->coll_fn);

        /* In order to preserve ordering on all ranks we have to add it to tail */
        /* TBD: Need to review the way we launch fragments */
        ML_VERBOSE(9, ("The task %p dependency is %d, appending it on pending list",
                    (void *)task_status, func->num_dependencies));
        OPAL_THREAD_LOCK(&(mca_coll_ml_component.pending_tasks_mutex));
        opal_list_append(&cm->pending_tasks, (opal_list_item_t *)task_status);
        OPAL_THREAD_UNLOCK(&(mca_coll_ml_component.pending_tasks_mutex));
    }

    ML_VERBOSE(9, ("Collective was launched !"));
    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__ int mca_coll_ml_generic_collectives_launcher(
                                        mca_coll_ml_collective_operation_progress_t  *op_prog,
                                        mca_coll_ml_task_setup_fn_t task_setup)
{
    int fn_index;
    int rc, ret;
    mca_coll_ml_collective_operation_description_t *op_desc =
        op_prog->coll_schedule;
    mca_coll_ml_compound_functions_t *func = NULL;
    mca_coll_ml_task_status_t *task_status = NULL;
    mca_coll_ml_component_t *cm = &mca_coll_ml_component;

    ML_VERBOSE(9, ("Calling mca_coll_ml_generic_collectives_launcher"));

    /* Init all tasks, before we start them */
    for (fn_index = 0; fn_index < op_desc->n_fns; fn_index++) {
        func = &op_desc->component_functions[fn_index];
        task_status = &op_prog->dag_description.status_array[fn_index];

        ML_VERBOSE(9, ("Processing function index %d", fn_index));

        assert(NULL != func);

        /* Init task status */
        task_status->n_dep_satisfied = 0; /* start from zero */
        /* task_status->my_index_in_coll_schedule = fn_index;
        pasha: the value is set during init */
        task_status->bcol_fn = func->bcol_function;
        /* Pasha: disabling support for custom complition functions
        task_status->task_comp_fn = func->task_comp_fn;
        */

        /* setup run time parametres */
        /* Pasha: do we need the if proctection ? */
        if (OPAL_LIKELY(NULL != task_setup)) {
            task_setup(task_status, fn_index, func);
        }

        /* the pointer to operation progress supposed to be set during
           construction time. Just want to make sure that it is ok */
        assert(task_status->ml_coll_operation == op_prog);
        /* Task status is done */

        /* launch the task and put it on corresponding list (if required) */

        /* We assume that all pointer to functions are defined and it
         is not reason to check for null */
        assert(NULL != func->bcol_function->coll_fn);
    }

    /* try to start startable */
    for (fn_index = 0; fn_index < op_desc->n_fns; fn_index++) {
        func = &op_desc->component_functions[fn_index];
        task_status = &op_prog->dag_description.status_array[fn_index];
        /* fire the collective immediately if it has no dependencies */
        if (0 == task_status->rt_num_dependencies) {
            rc = func->bcol_function->coll_fn(&op_prog->variable_fn_params,
                    /* Pasha: Need to update the prototype of the func,
                       right now it is ugly hack for compilation */
                    (struct mca_bcol_base_function_t *)&func->constant_group_data);
            switch(rc) {
                case BCOL_FN_NOT_STARTED:
                    /* put it on pending list */
                    ML_VERBOSE(9, ("Call to bcol collecitive return BCOL_FN_NOT_STARTED, putting the task on pending list"));
                    OPAL_THREAD_LOCK(&(mca_coll_ml_component.pending_tasks_mutex));
                    opal_list_append(&cm->pending_tasks, (opal_list_item_t *)task_status);
                    OPAL_THREAD_UNLOCK(&(mca_coll_ml_component.pending_tasks_mutex));
                    break;
                case BCOL_FN_STARTED:
                    /* put it on started list */
                    ML_VERBOSE(9, ("Call to bcol collecitive return BCOL_FN_STARTED, puting the task on active list"));
                    OPAL_THREAD_LOCK(&(mca_coll_ml_component.active_tasks_mutex));
                    opal_list_append(&cm->active_tasks, (opal_list_item_t *)task_status);
                    OPAL_THREAD_UNLOCK(&(mca_coll_ml_component.active_tasks_mutex));
                    break;
                case BCOL_FN_COMPLETE:
                    /* the task is done ! lets start relevant dependencies */
                    ML_VERBOSE(9, ("Call to bcol collecitive return BCOL_FN_COMPLETE"));
                    /* the task does not belong to any list, yes. So passing NULL */
                    ret = mca_coll_ml_task_completion_processing(&task_status, NULL);
                    if (OMPI_SUCCESS != ret) {
                        ML_VERBOSE(9, ("Failed to mca_coll_ml_task_completion_processing"));
                        return ret;
                    }
                    break;
                default:
                    ML_ERROR(("Unknow exit status %d", rc));
                    return OMPI_ERROR;
            }
        } else {
            /* the task is depend on other, lets put it on pending list */
            ML_VERBOSE(9, ("The task %p dependency is %d, putting it on pending list",
                        (void *)task_status, func->num_dependencies));
            OPAL_THREAD_LOCK(&(mca_coll_ml_component.pending_tasks_mutex));
            opal_list_append(&cm->pending_tasks, (opal_list_item_t *)task_status);
            OPAL_THREAD_UNLOCK(&(mca_coll_ml_component.pending_tasks_mutex));
        }
    }
    ML_VERBOSE(9, ("Collective was launched !"));
    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__ mca_coll_ml_collective_operation_progress_t *
mca_coll_ml_alloc_op_prog_single_frag_dag(
        mca_coll_ml_module_t *ml_module,
        mca_coll_ml_collective_operation_description_t *coll_schedule,
        void *src, void *dst, size_t total_bytes,
        size_t offset_into_user_buffer
        )
{
    ompi_free_list_item_t *item;
    mca_coll_ml_collective_operation_progress_t  *coll_op = NULL;
    ompi_request_t *req;

    /* Blocking call on fragment allocation (Maybe we want to make it non blocking ?) */
    OMPI_FREE_LIST_WAIT_MT(&(ml_module->coll_ml_collective_descriptors),
                        item);

    coll_op = (mca_coll_ml_collective_operation_progress_t *) item;
    ML_VERBOSE(10, (">>> Allocating coll op %p", coll_op));
    assert(NULL != coll_op);
    assert(coll_op->dag_description.status_array[0].item.opal_list_item_refcount == 0);
    req = &(coll_op->full_message.super);

    OMPI_REQUEST_INIT(req, false);
    /* Mark the request ACTIVE. It is critical for MPI_Test()*/
    req->req_state = OMPI_REQUEST_ACTIVE;
    req->req_status._cancelled = 0;
    req->req_status.MPI_ERROR = OMPI_SUCCESS;

    MCA_COLL_ML_OP_BASIC_SETUP(coll_op, total_bytes,
            offset_into_user_buffer, src, dst, coll_schedule);

    /* We do not set sequential, since it is not sequential call */
    coll_op->dag_description.num_tasks_completed = 0;

    /* Release reference counter have to be zero */
    assert(0 == coll_op->pending);

    return coll_op;
}

static inline __opal_attribute_always_inline__ mca_coll_ml_collective_operation_progress_t *
mca_coll_ml_duplicate_op_prog_single_frag_dag(
        mca_coll_ml_module_t *ml_module,
        mca_coll_ml_collective_operation_progress_t *old_op)
{
    mca_coll_ml_collective_operation_progress_t  *new_op = NULL;

    new_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
            ml_module->coll_ml_bcast_functions[old_op->fragment_data.current_coll_op],
            old_op->fragment_data.message_descriptor->dest_user_addr,
            old_op->fragment_data.message_descriptor->src_user_addr,
            old_op->fragment_data.message_descriptor->n_bytes_total,
            old_op->fragment_data.message_descriptor->n_bytes_scheduled);

    new_op->fragment_data.current_coll_op = old_op->fragment_data.current_coll_op;
    new_op->fragment_data.message_descriptor = old_op->fragment_data.message_descriptor;

    return new_op;
}

static inline __opal_attribute_always_inline__ mca_coll_ml_collective_operation_progress_t *
                                                    mca_coll_ml_alloc_op_prog_single_frag_seq(
                                        mca_coll_ml_module_t *ml_module,
                                        mca_coll_ml_collective_operation_description_t *coll_schedule,
                                        void *src, void *dst,
                                        size_t total_bytes,
                                        size_t offset_into_user_buffer
                                        )
{
    ompi_free_list_item_t *item;
    mca_coll_ml_collective_operation_progress_t  *coll_op = NULL;

    /* Blocking call on fragment allocation (Maybe we want to make it non blocking ?) */
    OMPI_FREE_LIST_WAIT_MT(&(ml_module->coll_ml_collective_descriptors),
                        item);

    coll_op = (mca_coll_ml_collective_operation_progress_t *) item;

    assert(NULL != coll_op);

    MCA_COLL_ML_OP_BASIC_SETUP(coll_op, total_bytes,
            offset_into_user_buffer, src, dst, coll_schedule);

    /* set sequential data */
    /* pasha - do we have something to set ? */

    return coll_op;
}

static inline __opal_attribute_always_inline__
                void mca_coll_ml_convertor_get_send_frag_size(mca_coll_ml_module_t *ml_module,
                                     size_t *frag_size, struct full_message_t *message_descriptor)
{
    size_t fragment_size = *frag_size;
    opal_convertor_t *dummy_convertor = &message_descriptor->dummy_convertor;

    /* The last frag needs special service */
    if (fragment_size >
          (size_t) message_descriptor->send_converter_bytes_packed) {
        *frag_size = message_descriptor->send_converter_bytes_packed;
        message_descriptor->send_converter_bytes_packed = 0;

        return;
    }
    if( (message_descriptor->dummy_conv_position + fragment_size) >
            message_descriptor->n_bytes_total ) {
        message_descriptor->dummy_conv_position = (message_descriptor->dummy_conv_position + fragment_size)
            - message_descriptor->n_bytes_total;
    } else {
        message_descriptor->dummy_conv_position += fragment_size;
    }

    opal_convertor_generic_simple_position(dummy_convertor, &message_descriptor->dummy_conv_position);
    *frag_size -= dummy_convertor->partial_length;

    message_descriptor->send_converter_bytes_packed -= (*frag_size);
}

static inline __opal_attribute_always_inline__ int
mca_coll_ml_launch_sequential_collective (mca_coll_ml_collective_operation_progress_t *coll_op)
{
    mca_bcol_base_coll_fn_desc_t *bcol_func;
    int ifunc, n_fn, ih, ret;
    mca_coll_ml_collective_operation_description_t *sched =
        coll_op->coll_schedule;

    n_fn = sched->n_fns;
    ih = coll_op->sequential_routine.current_active_bcol_fn;

    /* if collectives are already pending just add this one to the list */
    if (opal_list_get_size (&mca_coll_ml_component.sequential_collectives)) {
        opal_list_append(&mca_coll_ml_component.sequential_collectives, (opal_list_item_t *) coll_op);

        return OMPI_SUCCESS;
    }

    for (ifunc = ih; ifunc < n_fn; ifunc++, coll_op->sequential_routine.current_active_bcol_fn++) {
        ret = coll_op->sequential_routine.seq_task_setup(coll_op);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }

        bcol_func = (sched->component_functions[ifunc].bcol_function);
        ret = bcol_func->coll_fn(&coll_op->variable_fn_params,
                    (struct mca_bcol_base_function_t *) &sched->component_functions[ifunc].constant_group_data);

        if (BCOL_FN_COMPLETE == ret) {
            if (ifunc == n_fn - 1) {
                ret = coll_ml_fragment_completion_processing(coll_op);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                    mca_coll_ml_abort_ml("Failed to run coll_ml_fragment_completion_processing");
                }

                return OMPI_SUCCESS;
            }
        } else {
            if (BCOL_FN_STARTED == ret) {
                coll_op->sequential_routine.current_bcol_status = SEQ_TASK_IN_PROG;
            } else {
                coll_op->sequential_routine.current_bcol_status = SEQ_TASK_PENDING;
            }

            ML_VERBOSE(10, ("Adding pending bcol to the progress list to access by ml_progress func-id %d", ifunc));
            opal_list_append(&mca_coll_ml_component.sequential_collectives, (opal_list_item_t *) coll_op);

            break;
        }
    }

    return OMPI_SUCCESS;
}

END_C_DECLS

#endif
