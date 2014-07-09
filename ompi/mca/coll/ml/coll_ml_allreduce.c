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

#include <stdlib.h>

#include "ompi/constants.h"
#include "opal/threads/mutex.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/bcol/bcol.h"
#include "opal/sys/atomic.h"
#include "coll_ml.h"
#include "coll_ml_select.h"
#include "coll_ml_allocation.h"

static int mca_coll_ml_allreduce_small_unpack(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    int ret;
    /* need to put in more */
    int count = coll_op->variable_fn_params.count;
    ompi_datatype_t *dtype = coll_op->variable_fn_params.dtype;

    void *dest = (void *)((uintptr_t)coll_op->full_message.dest_user_addr +
            (uintptr_t)coll_op->fragment_data.offset_into_user_buffer);
    void *src = (void *)((uintptr_t)coll_op->fragment_data.buffer_desc->data_addr +
            (size_t)coll_op->variable_fn_params.rbuf_offset);

    ret = ompi_datatype_copy_content_same_ddt(dtype, (int32_t) count, (char *) dest,
            (char *) src);
    if (ret < 0) {
        return OMPI_ERROR;
    }

    ML_VERBOSE(10, ("sbuf addr %p, sbuf offset %d, rbuf addr %p, rbuf offset %d.",
                    src, coll_op->variable_fn_params.sbuf_offset, dest,
                    coll_op->variable_fn_params.rbuf_offset));

    return OMPI_SUCCESS;
}

static int mca_coll_ml_allreduce_task_setup(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    int fn_idx, h_level, my_index, root;
    mca_sbgp_base_module_t *sbgp;
    mca_coll_ml_topology_t *topo = coll_op->coll_schedule->topo_info;

    fn_idx      = coll_op->sequential_routine.current_active_bcol_fn;
    h_level     = coll_op->coll_schedule->component_functions[fn_idx].h_level;
    sbgp        = topo->component_pairs[h_level].subgroup_module;
    my_index    = sbgp->my_index;

    /* In the case of allreduce, the local leader is always the root */
    root = 0;
    if (my_index == root) {
        coll_op->variable_fn_params.root_flag = true;
        coll_op->variable_fn_params.root_route = NULL;
    } else {
        coll_op->variable_fn_params.root_flag = false;
        coll_op->variable_fn_params.root_route = &topo->route_vector[root];
    }

    /* NTH: This was copied from the old allreduce launcher. */
    if (0 < fn_idx) {
        coll_op->variable_fn_params.sbuf = coll_op->variable_fn_params.rbuf;
        coll_op->variable_fn_params.userbuf = coll_op->variable_fn_params.rbuf;
    }

    return OMPI_SUCCESS;
}

static int mca_coll_ml_allreduce_frag_progress(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    /* local variables */
    void *buf;

    size_t dt_size;
    int ret, frag_len, count;

    ptrdiff_t lb, extent;

    mca_bcol_base_payload_buffer_desc_t *src_buffer_desc;
    mca_coll_ml_collective_operation_progress_t *new_op;

    mca_coll_ml_module_t *ml_module = OP_ML_MODULE(coll_op);

    ret = ompi_datatype_get_extent(coll_op->variable_fn_params.dtype, &lb, &extent);
    if (ret < 0) {
     return OMPI_ERROR;
    }

    dt_size = (size_t) extent;

    /* Keep the pipeline filled with fragments */
    while (coll_op->fragment_data.message_descriptor->n_active <
        coll_op->fragment_data.message_descriptor->pipeline_depth) {
        /* If an active fragment happens to have completed the collective during
         * a hop into the progress engine, then don't launch a new fragment,
         * instead break and return.
         */
        if (coll_op->fragment_data.message_descriptor->n_bytes_scheduled
            == coll_op->fragment_data.message_descriptor->n_bytes_total) {
            break;
        }

        /* Get an ml buffer */
        src_buffer_desc = mca_coll_ml_alloc_buffer(OP_ML_MODULE(coll_op));
        if (NULL == src_buffer_desc) {
            /* If there exist outstanding fragments, then break out
             * and let an active fragment deal with this later,
             * there are no buffers available.
             */
            if (0 < coll_op->fragment_data.message_descriptor->n_active) {
                return OMPI_SUCCESS;
            }

            /* It is useless to call progress from here, since
             * ml progress can't be executed as result ml memsync
             * call will not be completed and no memory will be
             * recycled. So we put the element on the list, and we will
             * progress it later when memsync will recycle some memory*/

            /* The fragment is already on list and
             * the we still have no ml resources
             * Return busy */
            if (!(coll_op->pending & REQ_OUT_OF_MEMORY)) {
                coll_op->pending |= REQ_OUT_OF_MEMORY;
                opal_list_append(&((OP_ML_MODULE(coll_op))->waiting_for_memory_list),
                                 (opal_list_item_t *)coll_op);
                ML_VERBOSE(10,("Out of resources %p adding to pending queue", coll_op));
            } else {
                ML_VERBOSE(10,("Out of resources %p", coll_op));
            }

            return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }

        /* Get a new collective descriptor and initialize it */
        new_op =  mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                ml_module->coll_ml_allreduce_functions[coll_op->fragment_data.current_coll_op],
                coll_op->fragment_data.message_descriptor->src_user_addr,
                coll_op->fragment_data.message_descriptor->dest_user_addr,
                coll_op->fragment_data.message_descriptor->n_bytes_total,
                coll_op->fragment_data.message_descriptor->n_bytes_scheduled);

        MCA_COLL_IBOFFLOAD_SET_ML_BUFFER_INFO(new_op,
                src_buffer_desc->buffer_index, src_buffer_desc);

        new_op->fragment_data.current_coll_op = coll_op->fragment_data.current_coll_op;
        new_op->fragment_data.message_descriptor = coll_op->fragment_data.message_descriptor;

        /* set the task setup callback  */
        new_op->sequential_routine.seq_task_setup = mca_coll_ml_allreduce_task_setup;
        /* We need this address for pointer arithmetic in memcpy */
        buf = coll_op->fragment_data.message_descriptor->src_user_addr;
        /* calculate the number of data types in this packet */
        count = (coll_op->fragment_data.message_descriptor->n_bytes_total -
                coll_op->fragment_data.message_descriptor->n_bytes_scheduled <
                 (size_t) OP_ML_MODULE(coll_op)->small_message_thresholds[BCOL_ALLREDUCE] ?
                (coll_op->fragment_data.message_descriptor->n_bytes_total -
                coll_op->fragment_data.message_descriptor->n_bytes_scheduled) / dt_size :
                (size_t) coll_op->variable_fn_params.count);

        /* calculate the fragment length */
        frag_len = count*dt_size;

        ret = ompi_datatype_copy_content_same_ddt(coll_op->variable_fn_params.dtype, count,
                (char *) src_buffer_desc->data_addr, (char *) ((uintptr_t) buf + (uintptr_t)
                    coll_op->fragment_data.message_descriptor->n_bytes_scheduled));
        if (ret < 0) {
            return OMPI_ERROR;
        }

        /* No unpack for root */
        new_op->process_fn = mca_coll_ml_allreduce_small_unpack;

        /* Setup fragment specific data */
        new_op->fragment_data.message_descriptor->n_bytes_scheduled += frag_len;
        new_op->fragment_data.buffer_desc = src_buffer_desc;
        new_op->fragment_data.fragment_size = frag_len;
        (new_op->fragment_data.message_descriptor->n_active)++;

        ML_SET_VARIABLE_PARAMS_BCAST(
                new_op,
                OP_ML_MODULE(new_op),
                count,
                MPI_BYTE,
                src_buffer_desc,
                0,
                0,
                frag_len,
                src_buffer_desc->data_addr);
        /* Fill in bcast specific arguments */
        /* TBD: remove buffer_size */
        new_op->variable_fn_params.buffer_size = frag_len;
        new_op->variable_fn_params.count = count;
        new_op->variable_fn_params.hier_factor = coll_op->variable_fn_params.hier_factor;
        new_op->variable_fn_params.op = coll_op->variable_fn_params.op;
        new_op->variable_fn_params.dtype = coll_op->variable_fn_params.dtype;
        new_op->variable_fn_params.root = 0;
        new_op->variable_fn_params.sbuf = src_buffer_desc->data_addr;
        new_op->variable_fn_params.rbuf = src_buffer_desc->data_addr;
        new_op->sequential_routine.current_bcol_status = SEQ_TASK_PENDING;

        MCA_COLL_ML_SET_NEW_FRAG_ORDER_INFO(new_op);

        ML_VERBOSE(10,("FFFF Contig + fragmentation [0-sk, 1-lk, 3-su, 4-lu] %d %d %d",
                    new_op->variable_fn_params.buffer_size,
                    new_op->fragment_data.fragment_size,
                    new_op->fragment_data.message_descriptor->n_bytes_scheduled));
        /* initialize first coll */
        ret = new_op->sequential_routine.seq_task_setup(new_op);
        if (OMPI_SUCCESS != ret) {
            ML_VERBOSE(3,("Fragment failed to initialize itself"));
            return ret;
        }

        /* append this collective !! */
        OPAL_THREAD_LOCK(&(mca_coll_ml_component.sequential_collectives_mutex));
        opal_list_append(&mca_coll_ml_component.sequential_collectives,
                (opal_list_item_t *)new_op);
        OPAL_THREAD_UNLOCK(&(mca_coll_ml_component.sequential_collectives_mutex));

    }

    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__
int parallel_allreduce_start(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                mca_coll_ml_module_t *ml_module,
                                ompi_request_t **req,
                                int small_data_allreduce,
                                int large_data_allreduce)
{
    int ret, n_fragments = 1, frag_len,
        pipeline_depth, n_dts_per_frag ;

    ptrdiff_t lb, extent;
    size_t pack_len, dt_size;

    mca_bcol_base_payload_buffer_desc_t *src_buffer_desc;
    mca_coll_ml_collective_operation_progress_t *coll_op;

    mca_coll_ml_component_t *cm = &mca_coll_ml_component;

    bool contiguous = ompi_datatype_is_contiguous_memory_layout(dtype, count);

    if (MPI_IN_PLACE == sbuf) {
        sbuf = rbuf;
    }

    ret = ompi_datatype_get_extent(dtype, &lb, &extent);
    if (ret < 0) {
        return OMPI_ERROR;
    }

    dt_size = (size_t) extent;
    pack_len = count * dt_size;

    ML_VERBOSE(1,("The allreduce requested %d enable fragmentation %d ",
                    pack_len,
                    cm->enable_fragmentation));
    if (pack_len <= (size_t) ml_module->small_message_thresholds[BCOL_ALLREDUCE]) {
        /* The len of the message can not be larger than ML buffer size */
        assert(pack_len <= ml_module->payload_block->size_buffer);

        ML_VERBOSE(1,("Using small data allreduce (threshold = %d)",
                    ml_module->small_message_thresholds[BCOL_ALLREDUCE]));

        src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
        while (OPAL_UNLIKELY(NULL == src_buffer_desc)) {
            opal_progress();
            src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
        }

        coll_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                ml_module->coll_ml_allreduce_functions[small_data_allreduce],
                sbuf, rbuf, pack_len, 0);

        coll_op->variable_fn_params.rbuf = src_buffer_desc->data_addr;
        coll_op->variable_fn_params.sbuf = src_buffer_desc->data_addr;
        coll_op->variable_fn_params.count = count;

        ret = ompi_datatype_copy_content_same_ddt(dtype, count,
                (void *) (uintptr_t) src_buffer_desc->data_addr, (char *) sbuf);
        if (ret < 0){
            return OMPI_ERROR;
        }

        /* unpack function */
        coll_op->process_fn = mca_coll_ml_allreduce_small_unpack;
    } else if (cm->enable_fragmentation || !contiguous) {
        ML_VERBOSE(1,("Using Fragmented Allreduce"));

        /* fragment the data */
        /* check for retarded application programming decisions */
        if (dt_size > (size_t) ml_module->small_message_thresholds[BCOL_ALLREDUCE]) {
            ML_ERROR(("Sorry, but we don't support datatypes that large"));
            return OMPI_ERROR;
        }

        /* calculate the number of data types that can fit per ml-buffer */
        n_dts_per_frag = ml_module->small_message_thresholds[BCOL_ALLREDUCE] / dt_size;

        /* calculate the number of fragments */
        n_fragments = (count + n_dts_per_frag - 1) / n_dts_per_frag; /* round up */

        /* calculate the actual pipeline depth */
        pipeline_depth = n_fragments < cm->pipeline_depth ? n_fragments : cm->pipeline_depth;

        /* calculate the fragment size */
        frag_len = n_dts_per_frag * dt_size;

        /* allocate an ml buffer */
        src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
        while (NULL == src_buffer_desc) {
            opal_progress();
            src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
        }

        coll_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                ml_module->coll_ml_allreduce_functions[small_data_allreduce],
                sbuf, rbuf, pack_len, 0 /* offset for first pack */);

        /* task setup callback function */
        coll_op->sequential_routine.seq_task_setup = mca_coll_ml_allreduce_task_setup;

        coll_op->process_fn = mca_coll_ml_allreduce_small_unpack;

        coll_op->variable_fn_params.sbuf = (void *) src_buffer_desc->data_addr;
        coll_op->variable_fn_params.rbuf = (void *) src_buffer_desc->data_addr;

        coll_op->fragment_data.message_descriptor->n_active = 1;
        coll_op->full_message.n_bytes_scheduled = frag_len;
        coll_op->full_message.fragment_launcher = mca_coll_ml_allreduce_frag_progress;
        coll_op->full_message.pipeline_depth = pipeline_depth;
        coll_op->fragment_data.current_coll_op = small_data_allreduce;
        coll_op->fragment_data.fragment_size = frag_len;

        coll_op->variable_fn_params.count = n_dts_per_frag;  /* seems fishy */
        coll_op->variable_fn_params.buffer_size = frag_len;

        /* copy into the ml-buffer */
        ret = ompi_datatype_copy_content_same_ddt(dtype, n_dts_per_frag,
                (char *) src_buffer_desc->data_addr, (char *) sbuf);
        if (ret < 0) {
            return OMPI_ERROR;
        }
    } else {
        ML_VERBOSE(1,("Using zero-copy ptp allreduce"));
        coll_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                ml_module->coll_ml_allreduce_functions[large_data_allreduce],
                sbuf, rbuf, pack_len, 0);

        coll_op->variable_fn_params.userbuf =
            coll_op->variable_fn_params.sbuf = sbuf;

        coll_op->variable_fn_params.rbuf = rbuf;

        /* The ML buffer is used for testing. Later, when we
         * switch to use knem/mmap/portals this should be replaced
         * appropriately
         */
        src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
        while (NULL == src_buffer_desc) {
            opal_progress();
            src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
        }

        coll_op->variable_fn_params.count = count;
    }

    MCA_COLL_IBOFFLOAD_SET_ML_BUFFER_INFO(coll_op, src_buffer_desc->buffer_index,
                                          src_buffer_desc);

    /* set the offset */
    coll_op->variable_fn_params.sbuf_offset = 0;
    coll_op->variable_fn_params.rbuf_offset = 0;

    /* Fill in the function arguments */
    coll_op->variable_fn_params.sequence_num =
        OPAL_THREAD_ADD32(&(ml_module->collective_sequence_num), 1);
    coll_op->sequential_routine.current_active_bcol_fn = 0;
    coll_op->variable_fn_params.dtype = dtype;
    coll_op->variable_fn_params.op = op;
    coll_op->variable_fn_params.root = 0;
    coll_op->sequential_routine.seq_task_setup = mca_coll_ml_allreduce_task_setup; /* invoked after each level in sequential
                                                                                    * progress call
                                                                                    */
    MCA_COLL_ML_SET_ORDER_INFO(coll_op, n_fragments);

    ret = mca_coll_ml_launch_sequential_collective (coll_op);
    if (ret != OMPI_SUCCESS) {
        ML_VERBOSE(10, ("Failed to launch"));
        return ret;
    }

    *req = &coll_op->full_message.super;

    return OMPI_SUCCESS;
}

int mca_coll_ml_allreduce(void *sbuf, void *rbuf, int count,
                           struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module)
{
    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t*)module;
    ompi_request_t *req;
    int ret;

    if (OPAL_UNLIKELY(!ompi_op_is_commute(op))) {
        /* coll/ml does not handle non-communative operations at this time. fallback
         * on another collective module */
        return ml_module->fallback.coll_allreduce (sbuf, rbuf, count, dtype, op, comm,
                                                   ml_module->fallback.coll_allreduce_module);
    }

    ret = parallel_allreduce_start(sbuf, rbuf, count, dtype, op, comm,
                                   (mca_coll_ml_module_t *) module, &req,
                                    ML_SMALL_DATA_ALLREDUCE,
                                    ML_LARGE_DATA_ALLREDUCE);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        ML_ERROR(("Failed to launch"));
        return ret;
    }

    ompi_request_wait_completion(req);
    ompi_request_free(&req);

    ML_VERBOSE(10, ("Blocking NB allreduce is done"));

    return OMPI_SUCCESS;
}

int mca_coll_ml_allreduce_nb(void *sbuf, void *rbuf, int count,
                           struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                           struct ompi_communicator_t *comm,
                           ompi_request_t **req,
                           mca_coll_base_module_t *module)
{
    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t*)module;
    int ret;

    if (OPAL_UNLIKELY(!ompi_op_is_commute(op))) {
        /* coll/ml does not handle non-communative operations at this time. fallback
         * on another collective module */
        return ml_module->fallback.coll_iallreduce (sbuf, rbuf, count, dtype, op, comm, req,
                                                    ml_module->fallback.coll_iallreduce_module);
    }

    ret = parallel_allreduce_start(sbuf, rbuf, count, dtype, op, comm,
                                   (mca_coll_ml_module_t *) module, req,
                                    ML_SMALL_DATA_ALLREDUCE,
                                    ML_LARGE_DATA_ALLREDUCE);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        ML_ERROR(("Failed to launch"));
        return ret;
    }

    ML_VERBOSE(10, ("Blocking NB allreduce is done"));

    return OMPI_SUCCESS;
}

int mca_coll_ml_allreduce_dispatch(void *sbuf, void *rbuf, int count,
                                   struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                   struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    int rc;
    bool use_extra_topo;
    ompi_request_t *req;

    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t *) module;

    use_extra_topo = (count > 1) ?
            !ml_module->allreduce_matrix[op->op_type][dtype->id][BCOL_MULTI_ELEM_TYPE] :
            !ml_module->allreduce_matrix[op->op_type][dtype->id][BCOL_SINGLE_ELEM_TYPE];

    if (use_extra_topo) {
        rc = parallel_allreduce_start(sbuf, rbuf, count, dtype,
                                         op, comm, ml_module, &req,
                                         ML_SMALL_DATA_EXTRA_TOPO_ALLREDUCE,
                                         ML_LARGE_DATA_EXTRA_TOPO_ALLREDUCE);
    } else {
        rc = parallel_allreduce_start(sbuf, rbuf, count, dtype,
                                         op, comm, ml_module, &req,
                                         ML_SMALL_DATA_ALLREDUCE,
                                         ML_LARGE_DATA_ALLREDUCE);
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        ML_ERROR(("Failed to launch"));
        return rc;
    }

    ompi_request_wait_completion(req);
    ompi_request_free(&req);

    return OMPI_SUCCESS;
}

int mca_coll_ml_allreduce_dispatch_nb(void *sbuf, void *rbuf, int count,
                                   ompi_datatype_t *dtype, ompi_op_t *op,
                                   ompi_communicator_t *comm,
                                   ompi_request_t **req,
                                   mca_coll_base_module_t *module)
{
    int rc;
    bool use_extra_topo;

    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t *) module;

    use_extra_topo = (count > 1) ?
            !ml_module->allreduce_matrix[op->op_type][dtype->id][BCOL_MULTI_ELEM_TYPE] :
            !ml_module->allreduce_matrix[op->op_type][dtype->id][BCOL_SINGLE_ELEM_TYPE];

    if (use_extra_topo) {
        rc = parallel_allreduce_start(sbuf, rbuf, count, dtype,
                                         op, comm, ml_module, req,
                                         ML_SMALL_DATA_EXTRA_TOPO_ALLREDUCE,
                                         ML_LARGE_DATA_EXTRA_TOPO_ALLREDUCE);
    } else {
        rc = parallel_allreduce_start(sbuf, rbuf, count, dtype,
                                         op, comm, ml_module, req,
                                         ML_SMALL_DATA_ALLREDUCE,
                                         ML_LARGE_DATA_ALLREDUCE);
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        ML_ERROR(("Failed to launch"));
        return rc;
    }

    return OMPI_SUCCESS;
}
