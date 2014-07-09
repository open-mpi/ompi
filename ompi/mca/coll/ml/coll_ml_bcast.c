/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file */

#include "ompi_config.h"

#include <unistd.h>
#include <sys/uio.h>

#include "opal/threads/mutex.h"
#include "opal/sys/atomic.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/bcol/bcol.h"

#include "coll_ml.h"
#include "coll_ml_inlines.h"
#include "coll_ml_colls.h"
#include "coll_ml_allocation.h"

#define ML_BUFFER_ALLOC_WAIT(ml, buffer)        \
do {                                            \
    buffer = mca_coll_ml_alloc_buffer(ml);      \
    while (NULL == buffer) {                    \
        opal_progress();                        \
        buffer = mca_coll_ml_alloc_buffer(ml);  \
    }                                           \
} while (0)

#define COLL_ML_SETUP_ORDERING_INFO(op, last, prev)                   \
do {                                                                  \
    /* Don't change order of commands !!!! */                         \
    (op)->prev_frag = prev;                                           \
    (op)->fragment_data.message_descriptor->last_started_frag = last; \
    /* op->next_to_process_frag = NULL;   */                          \
} while (0)

#define ALLOCATE_AND_PACK_CONTIG_BCAST_FRAG(ml_module, op, coll_index, root,    \
        total_len, frag_len, buf, ml_buff_desc)                                 \
do {                                                                            \
    op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,                   \
            ml_module->coll_ml_bcast_functions[coll_index],                     \
            buf, buf,                                                           \
            total_len,                                                          \
            0 /* offset for first pack */);                                     \
    if (OPAL_LIKELY(frag_len > 0)) {                                            \
        if (ompi_comm_rank(ml_module->comm) == root) {                          \
            /* single frag, pack the data */                                    \
            memcpy((void *)(uintptr_t)(ml_buff_desc)->data_addr,                \
                    buf, frag_len);                                             \
            /* No unpack for root */                                            \
            op->process_fn = NULL;                                              \
        } else {                                                                \
            op->process_fn = mca_coll_ml_bcast_small_unpack_data;               \
        }                                                                       \
    }                                                                           \
    op->full_message.n_bytes_scheduled = frag_len;                              \
} while (0)

#define SMALL_BCAST 0
#define LARGE_BCAST (SMALL_BCAST + 1)

/* bcast data unpack */
static int mca_coll_ml_bcast_converter_unpack_data(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = 0;

    mca_coll_ml_collective_operation_progress_t *next_op;
    mca_coll_ml_module_t *ml_module =
                 (mca_coll_ml_module_t *) coll_op->coll_module;

    size_t max_index =
        ml_module->payload_block->num_banks * ml_module->payload_block->num_buffers_per_bank;

    bool is_first = true;
    int ret;

    /* Check if the fragment delivered in order */
    if (coll_op->fragment_data.buffer_desc->buffer_index !=
            coll_op->fragment_data.message_descriptor->next_expected_index) {
        mca_coll_ml_collective_operation_progress_t *prev_coll_op = coll_op->prev_frag;
        assert(NULL == prev_coll_op->next_to_process_frag);
        /* make sure that previous process will have pointer to the out
         of order process */
        prev_coll_op->next_to_process_frag = coll_op;
        assert(!(coll_op->pending & REQ_OUT_OF_ORDER));
        coll_op->pending |= REQ_OUT_OF_ORDER;
        /* we will unpack it later */
        ML_VERBOSE(10, ("Get %d expecting %d previous %d",
                    coll_op->fragment_data.buffer_desc->buffer_index,
                    coll_op->fragment_data.message_descriptor->next_expected_index,
                    prev_coll_op->fragment_data.buffer_desc->buffer_index));
        return ORTE_ERR_NO_MATCH_YET;
    }

    do {
        iov.iov_len = coll_op->fragment_data.fragment_size;
        iov.iov_base = (void *)((uintptr_t) coll_op->fragment_data.buffer_desc->data_addr);

        ML_VERBOSE(10, ("Data unpack with convertern index %d",
                         coll_op->fragment_data.buffer_desc->buffer_index));

        opal_convertor_unpack(&coll_op->fragment_data.message_descriptor->recv_convertor,
                &iov, &iov_count, &max_data);

        /* update next index */
        ++coll_op->fragment_data.message_descriptor->next_expected_index;
        if (coll_op->fragment_data.message_descriptor->next_expected_index >= max_index) {
            coll_op->fragment_data.message_descriptor->next_expected_index = 0;
        }

        /* Return to queue if the packet is done,
           the exeption is first packet, we release it later.
         */
        next_op = coll_op->next_to_process_frag;
        coll_op->next_to_process_frag = NULL;
        if ((!is_first) &&
                (0 != coll_op->fragment_data.offset_into_user_buffer)) {
            assert(coll_op->pending & REQ_OUT_OF_ORDER);
            coll_op->pending ^= REQ_OUT_OF_ORDER;
            /* Pasha: On one hand - I'm not sure that conceptually it is right place to call buffer recycling. Potentially,
               coll_ml_fragment_completion_processing() sounds like right place for out of order unpack/sync handling.
             * On the other hand - non contiguous data is not supper common and we would like to minimize effect on critical pass
             * for non contiguous data types. */
            ret = mca_coll_ml_buffer_recycling(coll_op);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return OMPI_ERROR;
            }

            CHECK_AND_RECYCLE(coll_op);
        }

        coll_op = next_op;
        is_first = false;
    } while (NULL != coll_op);

    return OMPI_SUCCESS;
}

static int mca_coll_ml_bcast_small_unpack_data(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    void * dest = (void *)((uintptr_t) coll_op->full_message.dest_user_addr +
                           (uintptr_t) coll_op->full_message.n_bytes_delivered);
    void * src = (void *)((uintptr_t) coll_op->fragment_data.buffer_desc->data_addr);

    memcpy(dest, src, coll_op->fragment_data.fragment_size);
    return OMPI_SUCCESS;
}

static int mca_coll_ml_bcast_large_unpack_data(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    void * dest = (void *)((uintptr_t) coll_op->fragment_data.message_descriptor->dest_user_addr +
                           (uintptr_t) coll_op->fragment_data.offset_into_user_buffer);
    void * src = (void *)((uintptr_t) coll_op->fragment_data.buffer_desc->data_addr);

    memcpy(dest, src, coll_op->fragment_data.fragment_size);
    return OMPI_SUCCESS;
}

static int mca_coll_ml_bcast_frag_converter_progress(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    /* local variables */
    int ret, frag_len;
    size_t max_data = 0;

    mca_bcol_base_payload_buffer_desc_t *src_buffer_desc = NULL;
    mca_coll_ml_collective_operation_progress_t *new_op = NULL;
    mca_coll_ml_task_setup_fn_t task_setup = NULL;
    mca_coll_ml_module_t *ml_module = OP_ML_MODULE(coll_op);

    /* Keep the pipeline filled with fragments */
    while (coll_op->fragment_data.message_descriptor->n_active <
                 mca_coll_ml_component.pipeline_depth) {
        /* If an active fragment happens to have completed the collective during
         * a hop into the progress engine, then don't launch a new fragment,
         * instead break and return.
         */
        if (coll_op->fragment_data.message_descriptor->n_bytes_scheduled
            == coll_op->fragment_data.message_descriptor->n_bytes_total) {
            break;
        }

        /* Get an ml buffer */
        src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
        if (OPAL_UNLIKELY(NULL == src_buffer_desc)) {
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
              opal_list_append(&ml_module->waiting_for_memory_list,
                               (opal_list_item_t *)coll_op);
            }

            return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }

        /* Get a new collective descriptor and initialize it */
        new_op = mca_coll_ml_duplicate_op_prog_single_frag_dag
            (ml_module, coll_op);
        /* We need this address for pointer arithmetic in memcpy */
        frag_len = ML_GET_FRAG_SIZE(coll_op, BCOL_BCAST);
        /* Decide based on global flag, not variable one */
        if (coll_op->fragment_data.message_descriptor->root) {
            struct iovec iov;
            uint32_t iov_count = 1;

            /* OBJ_RETAIN(new_op->variable_fn_params.dtype); */
            iov.iov_base = (IOVBASE_TYPE*) src_buffer_desc->data_addr;
            iov.iov_len  = ml_module->small_message_thresholds[BCOL_BCAST];
            assert(0 != iov.iov_len);

            max_data = ml_module->small_message_thresholds[BCOL_BCAST];
            opal_convertor_pack(&new_op->fragment_data.message_descriptor->send_convertor,
                                &iov, &iov_count, &max_data);

            new_op->process_fn = NULL;
            new_op->variable_fn_params.root_flag = true;
            new_op->variable_fn_params.root_route = NULL;

            task_setup = OP_ML_MODULE(new_op)->
                coll_ml_bcast_functions[new_op->fragment_data.current_coll_op]->
                task_setup_fn[COLL_ML_ROOT_TASK_FN];
        } else {
            new_op->process_fn = mca_coll_ml_bcast_converter_unpack_data;
            new_op->variable_fn_params.root_flag = false;
            new_op->variable_fn_params.root_route = coll_op->variable_fn_params.root_route;

            task_setup = OP_ML_MODULE(new_op)->
                coll_ml_bcast_functions[new_op->fragment_data.current_coll_op]->
                task_setup_fn[COLL_ML_GENERAL_TASK_FN];

            max_data = ml_module->small_message_thresholds[BCOL_BCAST];
            mca_coll_ml_convertor_get_send_frag_size(
                                    ml_module, &max_data,
                                    new_op->fragment_data.message_descriptor);
        }

        new_op->fragment_data.message_descriptor->n_bytes_scheduled += max_data;
        new_op->fragment_data.fragment_size = max_data;
        new_op->fragment_data.buffer_desc = src_buffer_desc;

        /* Setup fragment specific data */
        ++(new_op->fragment_data.message_descriptor->n_active);

        COLL_ML_SETUP_ORDERING_INFO(new_op, new_op,
                new_op->fragment_data.message_descriptor->last_started_frag);
        ML_VERBOSE(10, ("Start more, My index %d my prev %d",
                    new_op->fragment_data.buffer_desc->buffer_index,
                    new_op->prev_frag->fragment_data.buffer_desc->buffer_index));

        ML_SET_VARIABLE_PARAMS_BCAST(
                new_op,
                OP_ML_MODULE(new_op),
                frag_len,
                MPI_BYTE,
                src_buffer_desc,
                0,
                0,
                frag_len,
                src_buffer_desc->data_addr);

        /* TBD: remove buffer_size */
        new_op->variable_fn_params.buffer_size = coll_op->variable_fn_params.buffer_size;
        new_op->variable_fn_params.hier_factor = coll_op->variable_fn_params.hier_factor;

        /* Set order info for new frag if there is a bcol needs ordering */
        MCA_COLL_ML_SET_NEW_FRAG_ORDER_INFO(new_op);

        /* Launch this collective !! */
        ret = mca_coll_ml_generic_collectives_append_to_queue(new_op, task_setup);

        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            ML_ERROR(("Failed to launch"));
            return ret;
        }
    }

    return OMPI_SUCCESS;
}

static int mca_coll_ml_bcast_frag_progress(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    /* local variables */
    int ret;
    int frag_len, current_coll_op = coll_op->fragment_data.current_coll_op;
    size_t dt_size;
    void *buf;

    mca_bcol_base_payload_buffer_desc_t *src_buffer_desc = NULL;
    mca_coll_ml_collective_operation_progress_t *new_op = NULL;
    mca_coll_ml_task_setup_fn_t task_setup = NULL;

    ompi_datatype_type_size(coll_op->variable_fn_params.dtype, &dt_size);

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
                ML_VERBOSE(10,("Out of resources %p adding to pending queue", coll_op));
                coll_op->pending |= REQ_OUT_OF_MEMORY;
                opal_list_append(&((OP_ML_MODULE(coll_op))->waiting_for_memory_list),
                                (opal_list_item_t *) coll_op);
            } else {
                ML_VERBOSE(10,("Out of resources %p", coll_op));
            }

            return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }

        /* Get a new collective descriptor and initialize it */
        new_op = mca_coll_ml_duplicate_op_prog_single_frag_dag
            (OP_ML_MODULE(coll_op), coll_op);
        /* We need this address for pointer arithmetic in memcpy */
        buf = coll_op->fragment_data.message_descriptor->dest_user_addr;
        frag_len = ML_GET_FRAG_SIZE(coll_op, BCOL_BCAST);

        /* Decide based on global flag, not variable one */
        if (coll_op->fragment_data.message_descriptor->root) {
            memcpy((void *)(uintptr_t)src_buffer_desc->data_addr,
                    (void *) ((uintptr_t) buf + (uintptr_t) coll_op->
                    fragment_data.message_descriptor->n_bytes_scheduled) , frag_len);

            /* No unpack for root */
            new_op->process_fn = NULL;
            new_op->variable_fn_params.root_flag = true;
            new_op->variable_fn_params.root_route = NULL;
            task_setup = OP_ML_MODULE(new_op)->coll_ml_bcast_functions[current_coll_op]->
                task_setup_fn[COLL_ML_ROOT_TASK_FN];

        } else {
            new_op->process_fn = mca_coll_ml_bcast_large_unpack_data;
            new_op->variable_fn_params.root_flag = false;
            new_op->variable_fn_params.root_route = coll_op->variable_fn_params.root_route;
            task_setup = OP_ML_MODULE(new_op)->coll_ml_bcast_functions[current_coll_op]->
                task_setup_fn[COLL_ML_GENERAL_TASK_FN];
        }

        /* Setup fragment specific data */
        new_op->fragment_data.message_descriptor->n_bytes_scheduled += frag_len;
        new_op->fragment_data.buffer_desc = src_buffer_desc;
        new_op->fragment_data.fragment_size = frag_len;
        new_op->fragment_data.message_descriptor->n_active++;

        ML_SET_VARIABLE_PARAMS_BCAST(
                new_op,
                OP_ML_MODULE(new_op),
                frag_len,
                MPI_BYTE,
                src_buffer_desc,
                0,
                0,
                frag_len,
                src_buffer_desc->data_addr);

        /* Fill in bcast specific arguments */
        /* TBD: remove buffer_size */
        new_op->variable_fn_params.buffer_size = coll_op->variable_fn_params.buffer_size;
        new_op->variable_fn_params.hier_factor = coll_op->variable_fn_params.hier_factor;

        /* Set order info for new frag if there is a bcol needs ordering */
        MCA_COLL_ML_SET_NEW_FRAG_ORDER_INFO(new_op);

        ML_VERBOSE(10, ("FFFF Contig + fragmentation [0-sk, 1-lk, 3-su, 4-lu] %d %d %d",
                         new_op->variable_fn_params.buffer_size ,
                         new_op->fragment_data.fragment_size,
                         new_op->fragment_data.message_descriptor->n_bytes_scheduled));

        /* Launch this collective !! */
        ret = mca_coll_ml_generic_collectives_append_to_queue(new_op, task_setup);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            ML_VERBOSE(10, ("Failed to launch"));
            return ret;
        }
    }

    return OMPI_SUCCESS;
}

#define BCAST_FRAGMENTATION_IS_ENABLED(module)  \
    (module->bcast_fn_index_table[LARGE_BCAST] < ML_BCAST_LARGE_DATA_KNOWN)

static inline __opal_attribute_always_inline__
   int parallel_bcast_start(void *buf, int count, struct ompi_datatype_t *dtype,
                            int root, mca_coll_base_module_t *module, ompi_request_t **req)
{
    size_t pack_len = 0;
    size_t dt_size = 0;
    bool contig = false;
    int bcast_index, n_fragments = 1;

    mca_coll_ml_collective_operation_progress_t * coll_op = NULL;
    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t *) module;
    mca_bcol_base_payload_buffer_desc_t *src_buffer_desc = NULL;
    mca_coll_ml_task_setup_fn_t task_setup;
    OPAL_PTRDIFF_TYPE lb, extent;

    /* actual starting place of the user buffer (lb added) */
    void *actual_buf;

    ML_VERBOSE(10, ("Starting bcast, mca_coll_ml_bcast_uknown_root buf: %p", buf));

    ompi_datatype_type_size(dtype, &dt_size);
    pack_len = count * dt_size;

    /* Setup data buffer */
    ML_BUFFER_ALLOC_WAIT(ml_module, src_buffer_desc);
    /* Get information about memory layout */
    contig = opal_datatype_is_contiguous_memory_layout((opal_datatype_t *)dtype, count);

    ompi_datatype_get_extent (dtype, &lb, &extent);

    actual_buf = (void *) ((uintptr_t) buf + lb);

    /* Allocate collective schedule and pack message */
    if (contig) {
        if (pack_len <= (size_t) ml_module->small_message_thresholds[BCOL_BCAST]) {
            assert(pack_len <=  ml_module->payload_block->size_buffer);
            bcast_index = ml_module->bcast_fn_index_table[SMALL_BCAST];

            ML_VERBOSE(10, ("Contig + small message %d [0-sk, 1-lk, 3-su, 4-lu]", bcast_index));
            ALLOCATE_AND_PACK_CONTIG_BCAST_FRAG(ml_module, coll_op, bcast_index, root, pack_len,
                                                pack_len, actual_buf, src_buffer_desc);

            ML_SET_VARIABLE_PARAMS_BCAST(coll_op, ml_module, count, dtype,
                    src_buffer_desc, 0, 0, ml_module->payload_block->size_buffer,
                    (src_buffer_desc->data_addr));
        } else if (BCAST_FRAGMENTATION_IS_ENABLED(ml_module)) {
            /* We moved the fragmentation decision from communication creation time to
               runtime, since for large messages the if latency is not so critical */
            size_t n_dts_per_frag;
            int frag_len, pipeline_depth = mca_coll_ml_component.pipeline_depth;
            bcast_index = ml_module->bcast_fn_index_table[LARGE_BCAST];

            ML_VERBOSE(10, ("Contig + fragmentation %d [0-sk, 1-lk, 3-su, 4-lu]", bcast_index));

            /* Calculate the number of fragments required for this message */
            frag_len = (pack_len < (size_t) ml_module->small_message_thresholds[BCOL_BCAST] ?
                        pack_len : (size_t) ml_module->small_message_thresholds[BCOL_BCAST]);

            n_dts_per_frag = frag_len/dt_size;
            n_fragments = (pack_len + dt_size*n_dts_per_frag - 1)/(dt_size*n_dts_per_frag);
            pipeline_depth = (n_fragments < pipeline_depth ? n_fragments : pipeline_depth);

            ALLOCATE_AND_PACK_CONTIG_BCAST_FRAG(ml_module, coll_op, bcast_index, root, pack_len,
                                                frag_len, actual_buf, src_buffer_desc);
            ML_SET_VARIABLE_PARAMS_BCAST(coll_op, ml_module, (frag_len/dt_size), dtype,
                    src_buffer_desc, 0, 0, frag_len, (src_buffer_desc->data_addr));

            coll_op->full_message.fragment_launcher = mca_coll_ml_bcast_frag_progress;
            coll_op->full_message.pipeline_depth = pipeline_depth;
            /* Initialize fragment specific information */
            coll_op->fragment_data.current_coll_op = bcast_index;
            /* coll_op->fragment_data.message_descriptor->n_bytes_scheduled += frag_len; */
            coll_op->fragment_data.fragment_size = frag_len;
            coll_op->fragment_data.message_descriptor->n_active++;
            /* should be removed */
            coll_op->variable_fn_params.buffer_size = frag_len;

            ML_VERBOSE(10, ("Contig + fragmentation [0-sk, 1-lk, 3-su, 4-lu] %d %d",
                             coll_op->variable_fn_params.buffer_size,
                             coll_op->fragment_data.fragment_size));
        } else {
            bcast_index = ml_module->bcast_fn_index_table[LARGE_BCAST];
            ML_VERBOSE(10, ("Contig + zero copy %d [0-sk, 1-lk, 3-su, 4-lu]", bcast_index));

            coll_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                                                                ml_module->coll_ml_bcast_functions[bcast_index],
                                                                actual_buf, actual_buf, pack_len,
                                                                0 /* offset for first pack */);
            /* For large messages (bcast) this points to userbuf */
            /* Pasha: temporary work around for basesmuma, userbuf should
               be removed  */
            coll_op->variable_fn_params.userbuf = buf;
            coll_op->process_fn = NULL;
            coll_op->full_message.n_bytes_scheduled = pack_len;

            ML_SET_VARIABLE_PARAMS_BCAST(coll_op, ml_module, count, dtype,
                    src_buffer_desc, 0, 0,
                    ml_module->payload_block->size_buffer, buf);
        }
    } else {
        /* Non contiguous data type */
        bcast_index = ml_module->bcast_fn_index_table[SMALL_BCAST];
        ML_VERBOSE(10, ("NON Contig + fragmentation %d [0-sk, 1-lk, 3-su, 4-lu]", bcast_index));

        coll_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                                                            ml_module->coll_ml_bcast_functions[bcast_index],
                                                            actual_buf, actual_buf, pack_len,
                                                            0 /* offset for first pack */);
        if (OPAL_LIKELY(pack_len > 0)) {
            size_t max_data = 0;

            if (ompi_comm_rank(ml_module->comm) == root) {
                struct iovec iov;
                uint32_t iov_count = 1;

                opal_convertor_copy_and_prepare_for_send(
                        ompi_mpi_local_convertor,
                        &dtype->super, count, buf, 0,
                        &coll_op->full_message.send_convertor);

                opal_convertor_get_packed_size(&coll_op->full_message.send_convertor,
                                    &coll_op->full_message.send_converter_bytes_packed);

                coll_op->full_message.n_bytes_total =
                    coll_op->full_message.send_converter_bytes_packed;

                iov.iov_base = (IOVBASE_TYPE*) src_buffer_desc->data_addr;
                iov.iov_len  =  ml_module->small_message_thresholds[BCOL_BCAST];
                max_data = ml_module->small_message_thresholds[BCOL_BCAST];
                opal_convertor_pack(&coll_op->full_message.send_convertor,
                                    &iov, &iov_count, &max_data);
                coll_op->process_fn = NULL;
                coll_op->full_message.n_bytes_scheduled = max_data;

                /* We need prepare the data for future pipe line comunication */
                coll_op->full_message.fragment_launcher = mca_coll_ml_bcast_frag_converter_progress;
                coll_op->full_message.pipeline_depth = mca_coll_ml_component.pipeline_depth;
                coll_op->full_message.root = true;

            } else {
                opal_convertor_copy_and_prepare_for_send(
                        ompi_mpi_local_convertor,
                        &dtype->super, count, NULL, 0,
                        &coll_op->full_message.dummy_convertor);

                /* In non-root case we use it for #bytes remaining to receive */
                opal_convertor_get_packed_size(&coll_op->full_message.dummy_convertor,
                                    &coll_op->full_message.send_converter_bytes_packed);

                opal_convertor_copy_and_prepare_for_recv(
                        ompi_mpi_local_convertor,
                        &dtype->super, count, buf, 0,
                        &coll_op->full_message.recv_convertor);

                opal_convertor_get_unpacked_size(&coll_op->full_message.recv_convertor,
                        &coll_op->full_message.recv_converter_bytes_packed);

                coll_op->full_message.root = false;
                coll_op->full_message.n_bytes_total =
                    coll_op->full_message.recv_converter_bytes_packed;
                coll_op->process_fn = mca_coll_ml_bcast_converter_unpack_data;

                coll_op->full_message.fragment_launcher = mca_coll_ml_bcast_frag_converter_progress;
                coll_op->full_message.pipeline_depth = mca_coll_ml_component.pipeline_depth;

                max_data = ml_module->small_message_thresholds[BCOL_BCAST];
                coll_op->full_message.dummy_conv_position = 0;
                mca_coll_ml_convertor_get_send_frag_size(
                                             ml_module, &max_data,
                                             &coll_op->full_message);

                coll_op->full_message.n_bytes_scheduled = max_data;
            }
        }
        coll_op->fragment_data.current_coll_op = bcast_index;
        coll_op->fragment_data.message_descriptor->n_active++;
        coll_op->fragment_data.fragment_size = coll_op->full_message.n_bytes_scheduled;

        /* Set initial index */
        coll_op->full_message.next_expected_index = src_buffer_desc->buffer_index;

        /* Prepare linking information for future frags */
        COLL_ML_SETUP_ORDERING_INFO(coll_op, coll_op, NULL);

        /* Since the data is already packed we will use MPI_BYTE and byte count as datatype */
        ML_SET_VARIABLE_PARAMS_BCAST(coll_op, ml_module, coll_op->full_message.n_bytes_scheduled, MPI_BYTE,
                src_buffer_desc, 0, 0, ml_module->payload_block->size_buffer,(src_buffer_desc->data_addr));

        n_fragments = (coll_op->full_message.n_bytes_total +
                       ml_module->small_message_thresholds[BCOL_BCAST] - 1) / ml_module->small_message_thresholds[BCOL_BCAST];
    }

    coll_op->variable_fn_params.hier_factor = 1;
    coll_op->fragment_data.buffer_desc = src_buffer_desc;

    /* Set order info if there is a bcol needs ordering */
    MCA_COLL_ML_SET_ORDER_INFO(coll_op, n_fragments);

    if (ompi_comm_rank(ml_module->comm) == root) {
        coll_op->full_message.root =
            coll_op->variable_fn_params.root_flag = true;
        coll_op->variable_fn_params.root_route = NULL;
        task_setup = ml_module->coll_ml_bcast_functions[bcast_index]->
            task_setup_fn[COLL_ML_ROOT_TASK_FN];
    } else {
        coll_op->full_message.root =
            coll_op->variable_fn_params.root_flag = false;

        coll_op->variable_fn_params.root_route =
            (NULL == coll_op->coll_schedule->topo_info->route_vector ?
             NULL : &coll_op->coll_schedule->topo_info->route_vector[root]);

        task_setup = ml_module->coll_ml_bcast_functions[bcast_index]->
            task_setup_fn[COLL_ML_GENERAL_TASK_FN];
    }

    *req = &coll_op->full_message.super;
    return mca_coll_ml_generic_collectives_launcher(coll_op, task_setup);
}

int mca_coll_ml_parallel_bcast(void *buf, int count, struct ompi_datatype_t *dtype,
                            int root, struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    int ret;
    ompi_request_t *req;

    ret = parallel_bcast_start(buf, count, dtype, root, module, &req);
    if (OPAL_UNLIKELY(ret != OMPI_SUCCESS)) {
        ML_VERBOSE(10, ("Failed to launch"));
        return ret;
    }

    /* Blocking bcast */
    ompi_request_wait_completion(req);
    ompi_request_free(&req);

    ML_VERBOSE(10, ("Bcast is done mca_coll_ml_bcast_known"));

    return OMPI_SUCCESS;
}

int mca_coll_ml_parallel_bcast_nb(void *buf, int count, struct ompi_datatype_t *dtype,
                                  int root, struct ompi_communicator_t *comm,
                                  ompi_request_t **req,
                                  mca_coll_base_module_t *module)
{
    int ret;

    ret = parallel_bcast_start(buf, count, dtype, root, module, req);
    if (OPAL_UNLIKELY(ret != OMPI_SUCCESS)) {
        ML_VERBOSE(10, ("Failed to launch"));
        return ret;
    }

    ML_VERBOSE(10, ("Bcast is done mca_coll_ml_bcast_known"));

    return OMPI_SUCCESS;
}

int mca_coll_ml_bcast_sequential_root(void *buf, int count, struct ompi_datatype_t *dtype,
                                      int root, struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module)
{

    /* local variables */
    int ret, fn_idx;
    size_t pack_len = 0;
    size_t dt_size = 0;

    mca_coll_ml_collective_operation_progress_t * coll_op = NULL;
    mca_coll_ml_compound_functions_t *fixed_schedule;
    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t *) module;
    mca_bcol_base_payload_buffer_desc_t *src_buffer_desc = NULL;
    mca_bcol_base_coll_fn_desc_t *func;
    OPAL_PTRDIFF_TYPE lb, extent;

    /* actual starting place of the user buffer (lb added) */
    void *actual_buf;

    ML_VERBOSE(10, ("Starting static bcast, small messages"));

    assert(NULL != dtype);
    /* Calculate size of the data,
     * on this stage only contiguous data is supported */
    ompi_datatype_type_size(dtype, &dt_size);
    pack_len = count * dt_size;
    ompi_datatype_get_extent (dtype, &lb, &extent);

    actual_buf = (void *) ((uintptr_t) buf + lb);

    /* Setup data buffer */
    src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
    while (NULL == src_buffer_desc) {
        opal_progress();
        src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
    }

    /* Allocate collective schedule and pack message */
    if (pack_len <= (size_t) ml_module->small_message_thresholds[BCOL_BCAST]) {
        /* The len of the message can not be larger than ML buffer size */
        assert(pack_len <=  ml_module->payload_block->size_buffer);

        coll_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                                                            ml_module->coll_ml_bcast_functions[ML_BCAST_SMALL_DATA_SEQUENTIAL],
                                                            actual_buf, actual_buf, pack_len,
                                                            0 /* offset for first pack */);
        if (ompi_comm_rank(comm) == root) {
            /* single frag, pack the data */
            memcpy((void *)(uintptr_t)src_buffer_desc->data_addr,
                    buf, pack_len);
            /* No unpack for root */
            coll_op->process_fn = NULL;
        } else {
            coll_op->process_fn = mca_coll_ml_bcast_small_unpack_data;
        }

        coll_op->variable_fn_params.sbuf =
                   src_buffer_desc->data_addr;
    } else {
        ML_VERBOSE(10, ("ML_BCAST_LARGE_DATA_KNOWN case."));
        coll_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                                                            ml_module->coll_ml_bcast_functions[ML_BCAST_LARGE_DATA_SEQUENTIAL],
                                                            actual_buf, actual_buf, pack_len,
                                                            0 /* offset for first pack */);
        /* For large messages (bcast) this points to userbuf */
        /* Pasha: temporary work around for basesmuma, userbuf should
           be removed  */
        coll_op->variable_fn_params.userbuf =
        coll_op->variable_fn_params.sbuf = actual_buf;

        coll_op->process_fn = NULL;
    }

    /* Fill in the function arguments */
    coll_op->variable_fn_params.sequence_num =
        OPAL_THREAD_ADD32(&(ml_module->collective_sequence_num), 1);
    coll_op->variable_fn_params.count = count;
    coll_op->variable_fn_params.dtype = dtype;

    coll_op->variable_fn_params.buffer_index = src_buffer_desc->buffer_index;
    coll_op->variable_fn_params.src_desc = src_buffer_desc;
    coll_op->variable_fn_params.sbuf_offset = 0;
    coll_op->variable_fn_params.rbuf_offset = 0;

    /* pasha - why we duplicate it ? */
    coll_op->fragment_data.buffer_desc = src_buffer_desc;

    /* pack data into payload buffer - NOTE: assume no fragmenation at this stage */
    if (ompi_comm_rank(comm) == root) {
        coll_op->variable_fn_params.root_flag = true;
        coll_op->variable_fn_params.root_route =
                    &coll_op->coll_schedule->topo_info->route_vector[root];

        coll_op->full_message.n_bytes_scheduled = pack_len;
    } else {
        coll_op->variable_fn_params.root_flag = false;
        coll_op->variable_fn_params.root_route =
                    &coll_op->coll_schedule->topo_info->route_vector[root];
    }

    /* seems like we should fix a schedule here and now */
    fixed_schedule = coll_op->coll_schedule->
        comp_fn_arr[coll_op->variable_fn_params.root_route->level];

    /* now we set this schedule as the compound function list */
    coll_op->coll_schedule->component_functions = fixed_schedule;

    coll_op->sequential_routine.current_active_bcol_fn = 0;

    while (true) {
        /* ready, aim, fire collective(s)!! */
        fn_idx = coll_op->sequential_routine.current_active_bcol_fn;

        func = fixed_schedule[fn_idx].bcol_function;
        ret = func->coll_fn(&coll_op->variable_fn_params,
                (struct mca_bcol_base_function_t *) &fixed_schedule[fn_idx].constant_group_data);
        /* set the coll_fn_started flag to true */
        if (BCOL_FN_COMPLETE == ret) {
            /* done with this routine, bump the active counter */
            coll_op->sequential_routine.current_active_bcol_fn++;
            coll_op->variable_fn_params.root_flag = true;
            /* check for collective completion */
            if (coll_op->sequential_routine.current_active_bcol_fn ==
                    coll_op->coll_schedule->n_fns) {
                /* handle fragment completion */
                ret = coll_ml_fragment_completion_processing(coll_op);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                    mca_coll_ml_abort_ml("Failed to run coll_ml_fragment_completion_processing");
                }

                /* break out of while loop */
                break;
            }
        } else {
            /* put entire collective opperation onto sequential queue */
            opal_list_append(&mca_coll_ml_component.sequential_collectives,
                            (opal_list_item_t *) coll_op);
            break;
        }
    }

    /* Blocking bcast */
    ompi_request_wait_completion(&coll_op->full_message.super);
    ompi_request_free((ompi_request_t **) &coll_op);

    ML_VERBOSE(10, ("Bcast is done"));

    return OMPI_SUCCESS;
}
