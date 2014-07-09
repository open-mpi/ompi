/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
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
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/bcol/bcol.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/coll/ml/coll_ml.h"
#include "ompi/mca/coll/ml/coll_ml_allocation.h"
#include "ompi/mca/coll/ml/coll_ml_inlines.h"
#define REDUCE_SMALL_MESSAGE_THRESHOLD 2048

static int mca_coll_ml_reduce_unpack(mca_coll_ml_collective_operation_progress_t *coll_op)
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

    if (coll_op->variable_fn_params.root_flag) {
        ML_VERBOSE(1,("In reduce unpack %d",
            *(int *)((unsigned char*) src)));
    }

    ML_VERBOSE(10, ("sbuf addr %p, sbuf offset %d, sbuf val %lf, rbuf addr %p, rbuf offset %d, rbuf val %lf.",
                coll_op->variable_fn_params.sbuf, coll_op->variable_fn_params.sbuf_offset,
                *(double *) ((unsigned char *) coll_op->variable_fn_params.sbuf +
                    (size_t) coll_op->variable_fn_params.sbuf_offset),
                coll_op->variable_fn_params.rbuf, coll_op->variable_fn_params.rbuf_offset,
                *(double *) ((unsigned char *) coll_op->variable_fn_params.rbuf +
                    (size_t) coll_op->variable_fn_params.rbuf_offset)));

    return OMPI_SUCCESS;
}


static int
mca_coll_ml_reduce_task_setup (mca_coll_ml_collective_operation_progress_t *coll_op)
{
    int fn_idx, h_level, next_h_level, my_index;
    mca_sbgp_base_module_t *sbgp;
    mca_coll_ml_topology_t *topo = coll_op->coll_schedule->topo_info;

    fn_idx      = coll_op->sequential_routine.current_active_bcol_fn;
    h_level     = coll_op->coll_schedule->component_functions[fn_idx].h_level;
    next_h_level = (fn_idx < coll_op->coll_schedule->n_fns - 1) ?
        coll_op->coll_schedule->component_functions[fn_idx+1].h_level : -1;
    sbgp        = topo->component_pairs[h_level].subgroup_module;
    my_index    = sbgp->my_index;

    if (coll_op->variable_fn_params.root_flag) {
        ML_VERBOSE(1,("In task completion Data in receiver buffer %d ",
            *(int *)((unsigned char*) coll_op->variable_fn_params.rbuf +
            coll_op->variable_fn_params.rbuf_offset)));
    }

    /* determine the root for this level of the hierarchy */
    if (coll_op->coll_schedule->topo_info->route_vector[coll_op->global_root].level == next_h_level ||
        coll_op->global_root == sbgp->group_list[my_index]) {
        /* I am the global root or I will be talking to the global root in the next round. */
        coll_op->variable_fn_params.root = my_index;
    } else if (coll_op->coll_schedule->topo_info->route_vector[coll_op->global_root].level == h_level) {
        /* the root is in this level of my hierarchy */
        coll_op->variable_fn_params.root = coll_op->coll_schedule->topo_info->route_vector[coll_op->global_root].rank;
    } else {
        coll_op->variable_fn_params.root = 0;
    }

    /* Set the route vector for this root */
    coll_op->variable_fn_params.root_route =
        &coll_op->coll_schedule->topo_info->route_vector[sbgp->group_list[coll_op->variable_fn_params.root]];

    /* Am I the root of this hierarchy? */
    coll_op->variable_fn_params.root_flag = (my_index == coll_op->variable_fn_params.root);

    /* For hierarchy switch btw source and destination buffer
     * No need to make this switch for the first call ..
     * */
    if (0 < fn_idx) {
        int tmp_offset = coll_op->variable_fn_params.sbuf_offset;
        coll_op->variable_fn_params.sbuf_offset =
            coll_op->variable_fn_params.rbuf_offset;
        coll_op->variable_fn_params.rbuf_offset = tmp_offset;
    }

    return OMPI_SUCCESS;
}

static int mca_coll_ml_reduce_frag_progress(mca_coll_ml_collective_operation_progress_t *coll_op)
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
            } else {
                /* It is useless to call progress from here, since
                 * ml progress can't be executed as result ml memsync
                 * call will not be completed and no memory will be
                 * recycled. So we put the element on the list, and we will
                 * progress it later when memsync will recycle some memory*/

                /* The fragment is already on list and
                 * the we still have no ml resources
                 * Return busy */
                if (coll_op->pending & REQ_OUT_OF_MEMORY) {
                    ML_VERBOSE(10,("Out of resources %p", coll_op));
                    return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
                }

                coll_op->pending |= REQ_OUT_OF_MEMORY;
                opal_list_append(&((OP_ML_MODULE(coll_op))->waiting_for_memory_list),
                        (opal_list_item_t *)coll_op);
                ML_VERBOSE(10,("Out of resources %p adding to pending queue", coll_op));
                return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            }
        }

        /* Get a new collective descriptor and initialize it */
        new_op =  mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                ml_module->coll_ml_reduce_functions[ML_SMALL_DATA_REDUCE],
                coll_op->fragment_data.message_descriptor->src_user_addr,
                coll_op->fragment_data.message_descriptor->dest_user_addr,
                coll_op->fragment_data.message_descriptor->n_bytes_total,
                coll_op->fragment_data.message_descriptor->n_bytes_scheduled);

        ML_VERBOSE(1,(" In Reduce fragment progress %d %d ",
                    coll_op->fragment_data.message_descriptor->n_bytes_total,
                    coll_op->fragment_data.message_descriptor->n_bytes_scheduled));
        MCA_COLL_IBOFFLOAD_SET_ML_BUFFER_INFO(new_op,
                src_buffer_desc->buffer_index, src_buffer_desc);

        new_op->fragment_data.current_coll_op = coll_op->fragment_data.current_coll_op;
        new_op->fragment_data.message_descriptor = coll_op->fragment_data.message_descriptor;

        /* set the task setup callback  */
        new_op->sequential_routine.seq_task_setup = mca_coll_ml_reduce_task_setup;
        /* We need this address for pointer arithmetic in memcpy */
        buf = (void*)coll_op->fragment_data.message_descriptor->src_user_addr;
        /* calculate the number of data types in this packet */
        count = (coll_op->fragment_data.message_descriptor->n_bytes_total -
                coll_op->fragment_data.message_descriptor->n_bytes_scheduled <
                ((size_t) OP_ML_MODULE(coll_op)->small_message_thresholds[BCOL_REDUCE]/4 )?
                (coll_op->fragment_data.message_descriptor->n_bytes_total -
                coll_op->fragment_data.message_descriptor->n_bytes_scheduled) / dt_size :
                (size_t) coll_op->variable_fn_params.count);

        /* calculate the fragment length */
        frag_len = count * dt_size;

        ret = ompi_datatype_copy_content_same_ddt(coll_op->variable_fn_params.dtype, count,
                (char *) src_buffer_desc->data_addr, (char *) ((uintptr_t) buf + (uintptr_t)
                    coll_op->fragment_data.message_descriptor->n_bytes_scheduled));
        if (ret < 0) {
            return OMPI_ERROR;
        }

        /* if root unpack the data */
        if (ompi_comm_rank(ml_module->comm) == coll_op->global_root ) {
            new_op->process_fn = mca_coll_ml_reduce_unpack;
            new_op->variable_fn_params.root_flag = true;
        } else {
            new_op->process_fn = NULL;
            new_op->variable_fn_params.root_flag = false;
        }

        new_op->variable_fn_params.root_route = coll_op->variable_fn_params.root_route;

        /* Setup fragment specific data */
        new_op->fragment_data.message_descriptor->n_bytes_scheduled += frag_len;
        new_op->fragment_data.buffer_desc = src_buffer_desc;
        new_op->fragment_data.fragment_size = frag_len;
        (new_op->fragment_data.message_descriptor->n_active)++;

        /* Set in Reduce Buffer arguments */
        ML_SET_VARIABLE_PARAMS_BCAST(new_op, OP_ML_MODULE(new_op), count,
                                     coll_op->variable_fn_params.dtype, src_buffer_desc,
                                     0, (ml_module->payload_block->size_buffer -
                                         ml_module->data_offset)/2, frag_len,
                                     src_buffer_desc->data_addr);

        new_op->variable_fn_params.buffer_size = frag_len;
        new_op->variable_fn_params.sbuf = src_buffer_desc->data_addr;
        new_op->variable_fn_params.rbuf = src_buffer_desc->data_addr;
        new_op->variable_fn_params.root = coll_op->variable_fn_params.root;
        new_op->global_root = coll_op->global_root;
        new_op->variable_fn_params.op = coll_op->variable_fn_params.op;
        new_op->variable_fn_params.hier_factor = coll_op->variable_fn_params.hier_factor;
        new_op->sequential_routine.current_bcol_status = SEQ_TASK_PENDING;
        MCA_COLL_ML_SET_NEW_FRAG_ORDER_INFO(new_op);

        ML_VERBOSE(10,("FFFF Contig + fragmentation [0-sk, 1-lk, 3-su, 4-lu] %d %d %d",
                    new_op->variable_fn_params.buffer_size,
                    new_op->fragment_data.fragment_size,
                    new_op->fragment_data.message_descriptor->n_bytes_scheduled));
        /* initialize first coll */
        new_op->sequential_routine.seq_task_setup(new_op);

        /* append this collective !! */
        OPAL_THREAD_LOCK(&(mca_coll_ml_component.sequential_collectives_mutex));
        opal_list_append(&mca_coll_ml_component.sequential_collectives,
                (opal_list_item_t *)new_op);
        OPAL_THREAD_UNLOCK(&(mca_coll_ml_component.sequential_collectives_mutex));

    }

    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__
int parallel_reduce_start (void *sbuf, void *rbuf, int count,
                           struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                           int root,
                           struct ompi_communicator_t *comm,
                           mca_coll_ml_module_t *ml_module,
                           ompi_request_t **req,
                           int small_data_reduce,
                           int large_data_reduce) {
    ptrdiff_t lb, extent;
    size_t pack_len, dt_size;
    mca_bcol_base_payload_buffer_desc_t *src_buffer_desc = NULL;
    mca_coll_ml_collective_operation_progress_t * coll_op = NULL;
    bool contiguous = ompi_datatype_is_contiguous_memory_layout(dtype, count);
    mca_coll_ml_component_t *cm = &mca_coll_ml_component;
    int ret, n_fragments = 1, frag_len,
        pipeline_depth, n_dts_per_frag, rank;

    if (MPI_IN_PLACE == sbuf) {
        sbuf = rbuf;
    }

    ret = ompi_datatype_get_extent(dtype, &lb, &extent);
    if (ret < 0) {
        return OMPI_ERROR;
    }

    rank = ompi_comm_rank (comm);

    dt_size = (size_t) extent;
    pack_len = count * dt_size;

    /* We use a separate recieve and send buffer so only half the buffer is usable. */
    if (pack_len < (size_t) ml_module->small_message_thresholds[BCOL_REDUCE] / 4) {
        /* The len of the message can not be larger than ML buffer size */
        assert(pack_len <= ml_module->payload_block->size_buffer);

        src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);

        ML_VERBOSE(10,("Using small data reduce (threshold = %d)",
                                        REDUCE_SMALL_MESSAGE_THRESHOLD));
        while (NULL == src_buffer_desc) {
            opal_progress();
            src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
        }

        coll_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                ml_module->coll_ml_reduce_functions[small_data_reduce],
                sbuf, rbuf, pack_len, 0);

        MCA_COLL_IBOFFLOAD_SET_ML_BUFFER_INFO(coll_op,
                src_buffer_desc->buffer_index, src_buffer_desc);

        coll_op->variable_fn_params.rbuf = src_buffer_desc->data_addr;
        coll_op->variable_fn_params.sbuf = src_buffer_desc->data_addr;
        coll_op->variable_fn_params.buffer_index = src_buffer_desc->buffer_index;
        coll_op->variable_fn_params.src_desc = src_buffer_desc;
        coll_op->variable_fn_params.count = count;

        ret = ompi_datatype_copy_content_same_ddt(dtype, count,
                (void *) (uintptr_t) src_buffer_desc->data_addr, (char *) sbuf);
        if (ret < 0){
            return OMPI_ERROR;
        }

    } else if (cm->enable_fragmentation || !contiguous) {
        ML_VERBOSE(1,("Using Fragmented Reduce "));

        /* fragment the data */
        /* check for retarded application programming decisions */
        if (dt_size > (size_t) ml_module->small_message_thresholds[BCOL_REDUCE] / 4) {
            ML_ERROR(("Sorry, but we don't support datatypes that large"));
            return OMPI_ERROR;
        }

        /* calculate the number of data types that can fit per ml-buffer */
        n_dts_per_frag = ml_module->small_message_thresholds[BCOL_REDUCE] / (4 * dt_size);

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
                ml_module->coll_ml_reduce_functions[small_data_reduce],
                sbuf,rbuf,
                pack_len,
                0 /* offset for first pack */);

        MCA_COLL_IBOFFLOAD_SET_ML_BUFFER_INFO(coll_op,
                src_buffer_desc->buffer_index, src_buffer_desc);


        coll_op->variable_fn_params.sbuf = (void *) src_buffer_desc->data_addr;
        coll_op->variable_fn_params.rbuf = (void *) src_buffer_desc->data_addr;

        coll_op->fragment_data.message_descriptor->n_active = 1;
        coll_op->full_message.n_bytes_scheduled = frag_len;
        coll_op->full_message.fragment_launcher = mca_coll_ml_reduce_frag_progress;
        coll_op->full_message.pipeline_depth = pipeline_depth;
        coll_op->fragment_data.current_coll_op = small_data_reduce;
        coll_op->fragment_data.fragment_size = frag_len;

        coll_op->variable_fn_params.count = n_dts_per_frag;  /* seems fishy */
        coll_op->variable_fn_params.buffer_size = frag_len;
        coll_op->variable_fn_params.src_desc = src_buffer_desc;
        /* copy into the ml-buffer */
        ret = ompi_datatype_copy_content_same_ddt(dtype, n_dts_per_frag,
                (char *) src_buffer_desc->data_addr, (char *) sbuf);
        if (ret < 0) {
            return OMPI_ERROR;
        }
    } else {
        ML_VERBOSE(1,("Using zero-copy ptp reduce"));
        coll_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                ml_module->coll_ml_reduce_functions[large_data_reduce],
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

        coll_op->variable_fn_params.buffer_index = src_buffer_desc->buffer_index;
        coll_op->variable_fn_params.src_desc = src_buffer_desc;
        coll_op->variable_fn_params.count = count;
    }

    coll_op->process_fn = (rank != root) ? NULL : mca_coll_ml_reduce_unpack;

    /* Set common parts */
    coll_op->fragment_data.buffer_desc = src_buffer_desc;
    coll_op->variable_fn_params.dtype = dtype;
    coll_op->variable_fn_params.op = op;

    /* NTH: the root, root route, and root flag are set in the task setup */

    /* Fill in the function arguments */
    coll_op->variable_fn_params.sbuf_offset = 0;
    coll_op->variable_fn_params.rbuf_offset = (ml_module->payload_block->size_buffer -
                               ml_module->data_offset)/2;

    /* Keep track of the global root of this operation */
    coll_op->global_root = root;

    coll_op->variable_fn_params.sequence_num =
        OPAL_THREAD_ADD32(&(ml_module->collective_sequence_num), 1);
    coll_op->sequential_routine.current_active_bcol_fn = 0;
    /* set the task setup callback  */
    coll_op->sequential_routine.seq_task_setup = mca_coll_ml_reduce_task_setup;

    /* Reduce requires the schedule to be fixed. If we use other (changing) schedule,
       the operation might result in different result. */
    coll_op->coll_schedule->component_functions = coll_op->coll_schedule->
        comp_fn_arr[coll_op->coll_schedule->topo_info->route_vector[root].level];

    /* Launch the collective */
    ret = mca_coll_ml_launch_sequential_collective (coll_op);
    if (OMPI_SUCCESS != ret) {
        ML_VERBOSE(10, ("Failed to launch reduce collective"));
        return ret;
    }

    *req = &coll_op->full_message.super;

    return OMPI_SUCCESS;
}


int mca_coll_ml_reduce(void *sbuf, void *rbuf, int count,
        struct ompi_datatype_t *dtype, struct ompi_op_t *op,
        int root, struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module) {

    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t*)module;
    int ret = OMPI_SUCCESS;
    ompi_request_t *req;

    if (OPAL_UNLIKELY(!ompi_op_is_commute(op) || !opal_datatype_is_contiguous_memory_layout(&dtype->super, count))) {
        /* coll/ml does not handle non-communative operations at this time. fallback
         * on another collective module */
        return ml_module->fallback.coll_reduce (sbuf, rbuf, count, dtype, op, root, comm,
                                                ml_module->fallback.coll_reduce_module);
    }

    ML_VERBOSE(10,("Calling Ml Reduce "));
    ret = parallel_reduce_start(sbuf, rbuf, count, dtype, op,
                           root, comm, (mca_coll_ml_module_t *)module,
                           &req, ML_SMALL_DATA_REDUCE,
                           ML_LARGE_DATA_REDUCE);
    if (OPAL_UNLIKELY(ret != OMPI_SUCCESS)) {
        ML_VERBOSE(10, ("Failed to launch"));
        return ret;
    }

    /* Blocking reduce */
    ret = ompi_request_wait(&req, MPI_STATUS_IGNORE);

    ML_VERBOSE(10, ("Blocking Reduce is done"));

    return ret;
}


int mca_coll_ml_reduce_nb(void *sbuf, void *rbuf, int count,
        struct ompi_datatype_t *dtype, struct ompi_op_t *op,
        int root, struct ompi_communicator_t *comm,
        ompi_request_t **req,
        mca_coll_base_module_t *module) {

    int ret = OMPI_SUCCESS;
    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t*)module;

    if (OPAL_UNLIKELY(!ompi_op_is_commute(op) || !opal_datatype_is_contiguous_memory_layout(&dtype->super, count))) {
        /* coll/ml does not handle non-communative operations at this time. fallback
         * on another collective module */
        return ml_module->fallback.coll_ireduce (sbuf, rbuf, count, dtype, op, root, comm, req,
                                                 ml_module->fallback.coll_ireduce_module);
    }

    ML_VERBOSE(10,("Calling Ml Reduce "));
    ret = parallel_reduce_start(sbuf, rbuf, count, dtype, op,
                           root, comm, ml_module,
                           req, ML_SMALL_DATA_REDUCE,
                           ML_LARGE_DATA_REDUCE);
    if (OPAL_UNLIKELY(ret != OMPI_SUCCESS)) {
        ML_VERBOSE(10, ("Failed to launch"));
        return ret;
    }


    ML_VERBOSE(10, ("Non-blocking Reduce is done"));

    return OMPI_SUCCESS;

}
