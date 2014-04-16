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

static int mca_coll_ml_allgather_small_unpack_data(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    bool rcontig = coll_op->full_message.recv_data_continguous;
    int n_ranks_in_comm = ompi_comm_size(OP_ML_MODULE(coll_op)->comm);

    void *dest = (void *)((uintptr_t)coll_op->full_message.dest_user_addr +
            (uintptr_t)coll_op->full_message.n_bytes_delivered);
    void *src = (void *)((uintptr_t)coll_op->fragment_data.buffer_desc->data_addr +
            (size_t)coll_op->variable_fn_params.rbuf_offset);

    if (rcontig) {
        memcpy(dest, src, n_ranks_in_comm * coll_op->full_message.n_bytes_scheduled);
    } else {
        mca_coll_ml_convertor_unpack(src, n_ranks_in_comm * coll_op->full_message.n_bytes_scheduled,
                                          &coll_op->fragment_data.message_descriptor->recv_convertor);
    }

    return OMPI_SUCCESS;
}

static inline void copy_data (mca_coll_ml_collective_operation_progress_t *coll_op, rank_properties_t *rank_props, int soffset) {
    bool rcontig = coll_op->fragment_data.message_descriptor->recv_data_continguous;
    size_t total_bytes = coll_op->fragment_data.message_descriptor->n_bytes_total;
    size_t pack_len = coll_op->fragment_data.fragment_size;
    int doffset = rank_props->rank;
    void *dest, *src;

    src = (void *) ((uintptr_t)coll_op->fragment_data.buffer_desc->data_addr +
                    (size_t)coll_op->variable_fn_params.rbuf_offset + soffset * pack_len);

    if (rcontig) {
        dest = (void *) ((uintptr_t) coll_op->full_message.dest_user_addr +
                         (uintptr_t) coll_op->fragment_data.offset_into_user_buffer +
                         doffset * total_bytes);

        memcpy(dest, src, pack_len);
    } else {
        size_t position;
        opal_convertor_t *recv_convertor =
            &coll_op->fragment_data.message_descriptor->recv_convertor;

        position = (size_t) coll_op->fragment_data.offset_into_user_buffer +
            doffset * total_bytes;

        opal_convertor_set_position(recv_convertor, &position);
        mca_coll_ml_convertor_unpack(src, pack_len, recv_convertor);
    }
}

static int mca_coll_ml_allgather_noncontiguous_unpack_data(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    int i, j, n_level_one_sbgps;
    size_t soffset;

    mca_coll_ml_topology_t *topo_info = coll_op->coll_schedule->topo_info;
    sub_group_params_t *array_of_all_subgroup_ranks = topo_info->array_of_all_subgroups;

    n_level_one_sbgps = array_of_all_subgroup_ranks->level_one_index;

    for (i = 0 ; i < n_level_one_sbgps; i++) {
        /* determine where in the source buffer the data can be found */
        soffset = array_of_all_subgroup_ranks[i].index_of_first_element;
        for (j = 0 ; j < array_of_all_subgroup_ranks[i].n_ranks; j++, ++soffset) {
            copy_data (coll_op, array_of_all_subgroup_ranks[i].rank_data + j, soffset);
        }
    }

    return OMPI_SUCCESS;
}

/* Allgather dependencies seem easy, everyone needs to work from the "bottom up".
 * Following Pasha, I too will put the simplest dependencies graph and change it later
 * when we add hierarchy. Basically, allgather has the same dependency profile as the
 * sequential broadcast except that there is only a single ordering of tasks.
 */
static int mca_coll_ml_allgather_task_setup(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    int fn_idx, h_level, my_index, root;
    mca_sbgp_base_module_t *sbgp;
    mca_coll_ml_topology_t *topo = coll_op->coll_schedule->topo_info;

    fn_idx      = coll_op->sequential_routine.current_active_bcol_fn;
    h_level     = coll_op->coll_schedule->component_functions[fn_idx].h_level;
    sbgp        = topo->component_pairs[h_level].
                  subgroup_module;
    my_index    = sbgp->my_index;

    /* In the case of allgather, the local leader is always the root */
    root = 0;
    if (my_index == root) {
        coll_op->variable_fn_params.root_flag = true;
        coll_op->variable_fn_params.root_route = NULL;
    } else {
        coll_op->variable_fn_params.root_flag = false;
        coll_op->variable_fn_params.root_route = &topo->route_vector[root];
    }

    return OMPI_SUCCESS;
}

static int mca_coll_ml_allgather_frag_progress(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    /* local variables */
    int ret;
    size_t frag_len, dt_size;

    void *buf;
    mca_bcol_base_payload_buffer_desc_t *src_buffer_desc;
    mca_coll_ml_collective_operation_progress_t *new_op;

    mca_coll_ml_module_t *ml_module = OP_ML_MODULE(coll_op);
    bool scontig = coll_op->fragment_data.message_descriptor->send_data_continguous;

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
        src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
        if (NULL == src_buffer_desc) {
            /* If there exist outstanding fragments, then break out
             * and let an active fragment deal with this later,
             * there are no buffers available.
             */
            if (0 < coll_op->fragment_data.message_descriptor->n_active) {
                return OMPI_SUCCESS;
            } else {
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
                     ml_module->coll_ml_allgather_functions[ML_SMALL_DATA_ALLGATHER],
                     coll_op->fragment_data.message_descriptor->src_user_addr,
                     coll_op->fragment_data.message_descriptor->dest_user_addr,
                     coll_op->fragment_data.message_descriptor->n_bytes_total,
                     coll_op->fragment_data.message_descriptor->n_bytes_scheduled);

        new_op->fragment_data.current_coll_op = coll_op->fragment_data.current_coll_op;
        new_op->fragment_data.message_descriptor = coll_op->fragment_data.message_descriptor;

        /* set the task setup callback  */
        new_op->sequential_routine.seq_task_setup = mca_coll_ml_allgather_task_setup;

        /*
        MCA_COLL_IBOFFLOAD_SET_ML_BUFFER_INFO(new_op,
                src_buffer_desc->buffer_index, src_buffer_desc);
        */

        /* We need this address for pointer arithmetic in memcpy */
        buf = coll_op->fragment_data.message_descriptor->src_user_addr;

        if (!scontig) {
            frag_len = ml_module->small_message_thresholds[BCOL_ALLGATHER];
            mca_coll_ml_convertor_get_send_frag_size(
                            ml_module, &frag_len,
                            coll_op->fragment_data.message_descriptor);

            mca_coll_ml_convertor_pack(
                (void *) ((uintptr_t) src_buffer_desc->data_addr +
                frag_len * coll_op->coll_schedule->topo_info->hier_layout_info[0].offset +
                frag_len * coll_op->coll_schedule->topo_info->hier_layout_info[0].level_one_index),
                frag_len, &coll_op->fragment_data.message_descriptor->send_convertor);
       } else {
            /* calculate new frag length, there are some issues here */
            frag_len = (coll_op->fragment_data.message_descriptor->n_bytes_total -
                    coll_op->fragment_data.message_descriptor->n_bytes_scheduled <
                    coll_op->fragment_data.fragment_size ?
                    coll_op->fragment_data.message_descriptor->n_bytes_total -
                    coll_op->fragment_data.message_descriptor->n_bytes_scheduled :
                    coll_op->fragment_data.fragment_size);

            /* everybody copies in, based on the new values */
            memcpy((void *) ((uintptr_t)src_buffer_desc->data_addr +
                    frag_len * new_op->coll_schedule->topo_info->hier_layout_info[0].offset +
                    frag_len * new_op->coll_schedule->topo_info->hier_layout_info[0].level_one_index),
                    (void *) ((uintptr_t) buf + (uintptr_t)
                            coll_op->fragment_data.message_descriptor->n_bytes_scheduled), frag_len);
        }

        new_op->variable_fn_params.sbuf = (void *) src_buffer_desc->data_addr;
        new_op->variable_fn_params.rbuf = (void *) src_buffer_desc->data_addr;

        /* update the number of bytes scheduled */
        new_op->fragment_data.message_descriptor->n_bytes_scheduled += frag_len;
        /* everyone needs an unpack function */
        new_op->process_fn = mca_coll_ml_allgather_noncontiguous_unpack_data;

        new_op->fragment_data.fragment_size = frag_len;
        new_op->fragment_data.buffer_desc = src_buffer_desc;

        /* Setup fragment specific data */
        ++(new_op->fragment_data.message_descriptor->n_active);

        ML_VERBOSE(10, ("Start more, My index %d ",
                    new_op->fragment_data.buffer_desc->buffer_index));

        /* this is a bit buggy */
        ML_SET_VARIABLE_PARAMS_BCAST(
                new_op,
                OP_ML_MODULE(new_op),
                frag_len /* yes, we have consistent units, so this makes sense */,
                MPI_BYTE /* we fragment according to buffer size
                          * we don't reduce the data thus we needn't
                          * keep "whole" datatypes, we may freely
                          * fragment without regard for multiples
                          * of any specific datatype
                          */,
                src_buffer_desc,
                0,
                0,
                frag_len,
                src_buffer_desc->data_addr);
        /* initialize first coll */
        ret = new_op->sequential_routine.seq_task_setup(new_op);
        if (OMPI_SUCCESS != ret) {
            ML_VERBOSE(3, ("Fragment failed to initialize itself"));
            return ret;
        }

        new_op->variable_fn_params.buffer_size = frag_len;
        new_op->variable_fn_params.hier_factor = coll_op->variable_fn_params.hier_factor;
        new_op->variable_fn_params.root = 0;

        MCA_COLL_ML_SET_NEW_FRAG_ORDER_INFO(new_op);

        /* append this collective !! */
        OPAL_THREAD_LOCK(&(mca_coll_ml_component.sequential_collectives_mutex));
        opal_list_append(&mca_coll_ml_component.sequential_collectives,
                                    (opal_list_item_t *)new_op);
        OPAL_THREAD_UNLOCK(&(mca_coll_ml_component.sequential_collectives_mutex));
    }

    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__
int mca_coll_ml_allgather_start (void *sbuf, int scount,
                                 struct ompi_datatype_t *sdtype,
                                 void* rbuf, int rcount,
                                 struct ompi_datatype_t *rdtype,
                                 struct ompi_communicator_t *comm,
                                 mca_coll_base_module_t *module,
                                 ompi_request_t **req)
{
    size_t pack_len, sdt_size;
    int ret, n_fragments = 1, comm_size;

    mca_coll_ml_topology_t *topo_info;
    mca_bcol_base_payload_buffer_desc_t *src_buffer_desc;

    mca_coll_ml_component_t *cm = &mca_coll_ml_component;

    mca_coll_ml_collective_operation_progress_t *coll_op;
    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t *) module;

    ptrdiff_t lb, extent;
    bool scontig, rcontig, in_place = false;

    /* check for in place setting */
    if (MPI_IN_PLACE == sbuf) {
        in_place = true;
        sdtype = rdtype;
        scount = rcount;
    }

    /* scontig could be != to rcontig */
    scontig = ompi_datatype_is_contiguous_memory_layout(sdtype, scount);
    rcontig = ompi_datatype_is_contiguous_memory_layout(rdtype, rcount);

    comm_size = ompi_comm_size(comm);

    ML_VERBOSE(10, ("Starting allgather"));

    assert(NULL != sdtype);
    /* Calculate size of the data,
     * at this stage, only contiguous data is supported */

    /* this is valid for allagther */
    ompi_datatype_type_size(sdtype, &sdt_size);
    pack_len = scount * sdt_size;

    if (in_place) {
        sbuf = (char *) rbuf + ompi_comm_rank(comm) * pack_len;
    }

    /* Allocate collective schedule and pack message */
    /* this is the total ending message size that will need to fit in the ml-buffer */
    if (pack_len <= (size_t) ml_module->small_message_thresholds[BCOL_ALLGATHER]) {
        /* The len of the message can not be larger than ML buffer size */
        ML_VERBOSE(10, ("Single frag %d %d %d", pack_len, comm_size, ml_module->payload_block->size_buffer));
        assert(pack_len * comm_size <= ml_module->payload_block->size_buffer);

        src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
        while (NULL == src_buffer_desc) {
            opal_progress();
            src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
        }

        /* change 1 */
        coll_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                ml_module->coll_ml_allgather_functions[ML_SMALL_DATA_ALLGATHER],
                sbuf, rbuf, pack_len, 0 /* offset for first pack */);

        MCA_COLL_IBOFFLOAD_SET_ML_BUFFER_INFO(coll_op,
                src_buffer_desc->buffer_index, src_buffer_desc);

        coll_op->fragment_data.current_coll_op = ML_SMALL_DATA_ALLGATHER;
        /* task setup callback function */
        coll_op->sequential_routine.seq_task_setup = mca_coll_ml_allgather_task_setup;

        /* change 2 */
        if (!scontig) {
            coll_op->full_message.n_bytes_scheduled =
                mca_coll_ml_convertor_prepare(sdtype, scount, sbuf,
                    &coll_op->full_message.send_convertor, MCA_COLL_ML_NET_STREAM_SEND);

            mca_coll_ml_convertor_pack(
                        (void *) ((uintptr_t) src_buffer_desc->data_addr + pack_len *
                                  (coll_op->coll_schedule->topo_info->hier_layout_info[0].offset +
                                   coll_op->coll_schedule->topo_info->hier_layout_info[0].level_one_index)),
                        pack_len, &coll_op->full_message.send_convertor);
        } else {
            /* change 3 */
            memcpy((void *)((uintptr_t) src_buffer_desc->data_addr + pack_len *
                            (coll_op->coll_schedule->topo_info->hier_layout_info[0].offset +
                             coll_op->coll_schedule->topo_info->hier_layout_info[0].level_one_index)),
                   sbuf, pack_len);

            coll_op->full_message.n_bytes_scheduled = pack_len;
        }

        if (!rcontig) {
            mca_coll_ml_convertor_prepare(rdtype, rcount * comm_size, rbuf,
                &coll_op->full_message.recv_convertor, MCA_COLL_ML_NET_STREAM_RECV);
        }

        if (coll_op->coll_schedule->topo_info->ranks_contiguous) {
            coll_op->process_fn = mca_coll_ml_allgather_small_unpack_data;
        } else {
            coll_op->process_fn = mca_coll_ml_allgather_noncontiguous_unpack_data;
        }

        /* whole ml-buffer is used to send AND receive */
        coll_op->variable_fn_params.sbuf = (void *) src_buffer_desc->data_addr;
        coll_op->variable_fn_params.rbuf = (void *) src_buffer_desc->data_addr;

        /* we can set the initial offset here */
        coll_op->variable_fn_params.sbuf_offset = 0;
        coll_op->variable_fn_params.rbuf_offset = 0;

        coll_op->variable_fn_params.count = scount;
        coll_op->fragment_data.fragment_size =
                             coll_op->full_message.n_bytes_scheduled;

        /* For small CINCO, we may use the native datatype */
        coll_op->variable_fn_params.dtype = sdtype;
        coll_op->variable_fn_params.buffer_size = pack_len;
        coll_op->variable_fn_params.root = 0;
    } else if (cm->enable_fragmentation || pack_len * comm_size < (1 << 20)) {
        /* calculate the number of fragments and the size of each frag */
        size_t n_dts_per_frag, frag_len;
        int pipeline_depth = mca_coll_ml_component.pipeline_depth;

        /* Calculate the number of fragments required for this message careful watch the integer division !*/
        frag_len = (pack_len <= (size_t) ml_module->small_message_thresholds[BCOL_ALLGATHER] ?
                pack_len : (size_t) ml_module->small_message_thresholds[BCOL_ALLGATHER]);

        n_dts_per_frag = frag_len / sdt_size;
        n_fragments = (pack_len + sdt_size * n_dts_per_frag - 1) / (sdt_size * n_dts_per_frag);
        pipeline_depth = (n_fragments < pipeline_depth ? n_fragments : pipeline_depth);

        src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
        while (NULL == src_buffer_desc) {
            opal_progress();
            src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
        }

        /* change 4 */
        coll_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                ml_module->coll_ml_allgather_functions[ML_SMALL_DATA_ALLGATHER],
                sbuf, rbuf, pack_len,
                0 /* offset for first pack */);

        MCA_COLL_IBOFFLOAD_SET_ML_BUFFER_INFO(coll_op,
                src_buffer_desc->buffer_index, src_buffer_desc);
        topo_info = coll_op->coll_schedule->topo_info;

        /* task setup callback function */
        coll_op->sequential_routine.seq_task_setup = mca_coll_ml_allgather_task_setup;

        if (!scontig) {
            coll_op->full_message.send_converter_bytes_packed =
                        mca_coll_ml_convertor_prepare(
                                sdtype, scount, NULL,
                                &coll_op->full_message.dummy_convertor,
                                MCA_COLL_ML_NET_STREAM_SEND);

            coll_op->full_message.dummy_conv_position = 0;
            mca_coll_ml_convertor_get_send_frag_size(
                                    ml_module, &frag_len,
                                    &coll_op->full_message);

            /* change 5 */
            mca_coll_ml_convertor_prepare(sdtype, scount, sbuf,
                    &coll_op->full_message.send_convertor, MCA_COLL_ML_NET_STREAM_SEND);

            mca_coll_ml_convertor_pack(
                    (void *) ((uintptr_t) src_buffer_desc->data_addr + frag_len *
                              (topo_info->hier_layout_info[0].offset +
                               topo_info->hier_layout_info[0].level_one_index)),
                    frag_len, &coll_op->full_message.send_convertor);
        } else {
            /* change 6 */
            memcpy((void *)((uintptr_t)src_buffer_desc->data_addr + frag_len *
                            (topo_info->hier_layout_info[0].offset +
                             topo_info->hier_layout_info[0].level_one_index)),
                    sbuf, frag_len);
        }

        if (!rcontig) {
            mca_coll_ml_convertor_prepare(rdtype, rcount * comm_size, rbuf,
                    &coll_op->full_message.recv_convertor, MCA_COLL_ML_NET_STREAM_RECV);
        }

        coll_op->process_fn = mca_coll_ml_allgather_noncontiguous_unpack_data;

        /* hopefully this doesn't royaly screw things up idea behind this is the
         * whole ml-buffer is used to send and receive
         */
        coll_op->variable_fn_params.sbuf = (void *) src_buffer_desc->data_addr;
        coll_op->variable_fn_params.rbuf = (void *) src_buffer_desc->data_addr;

        /* we can set the initial offset here */
        coll_op->variable_fn_params.sbuf_offset = 0;
        coll_op->variable_fn_params.rbuf_offset = 0;

        coll_op->fragment_data.buffer_desc = src_buffer_desc;

        coll_op->fragment_data.fragment_size = frag_len;
        coll_op->fragment_data.message_descriptor->n_active = 1;

        coll_op->full_message.n_bytes_scheduled = frag_len;
        coll_op->full_message.fragment_launcher = mca_coll_ml_allgather_frag_progress;

        coll_op->full_message.pipeline_depth = pipeline_depth;
        coll_op->fragment_data.current_coll_op = ML_SMALL_DATA_ALLGATHER;

        /* remember this is different for frags !! Caused data corruption when
         * not properly set. Need to be sure you have consistent units.
         */
        coll_op->variable_fn_params.count = frag_len;
        coll_op->variable_fn_params.dtype = MPI_BYTE; /* for fragmented data, we work in
                                                       * units of bytes. This means that
                                                       * all of our arithmetic is done
                                                       * in terms of bytes
                                                       */

        coll_op->variable_fn_params.root = 0;
        coll_op->variable_fn_params.frag_size = frag_len;
        coll_op->variable_fn_params.buffer_size = frag_len;
    } else {
        /* change 7 */
        ML_VERBOSE(10, ("ML_ALLGATHER_LARGE_DATA_KNOWN case."));
        coll_op = mca_coll_ml_alloc_op_prog_single_frag_dag(ml_module,
                ml_module->coll_ml_allgather_functions[ML_LARGE_DATA_ALLGATHER],
                sbuf, rbuf, pack_len, 0 /* offset for first pack */);
        topo_info = coll_op->coll_schedule->topo_info;
        if (MCA_BCOL_BASE_NO_ML_BUFFER_FOR_LARGE_MSG & topo_info->all_bcols_mode) {
            MCA_COLL_IBOFFLOAD_SET_ML_BUFFER_INFO(coll_op, MCA_COLL_ML_NO_BUFFER, NULL);
        } else {
            src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
            while (NULL == src_buffer_desc) {
                opal_progress();
                src_buffer_desc = mca_coll_ml_alloc_buffer(ml_module);
            }

            MCA_COLL_IBOFFLOAD_SET_ML_BUFFER_INFO(coll_op, src_buffer_desc->buffer_index, src_buffer_desc);
        }

        /* not sure if I really need this here */
        coll_op->sequential_routine.seq_task_setup = mca_coll_ml_allgather_task_setup;
        coll_op->process_fn = NULL;
        /* probably the most important piece */
        coll_op->variable_fn_params.sbuf = sbuf;
        coll_op->variable_fn_params.rbuf = rbuf;
        coll_op->variable_fn_params.sbuf_offset = 0;
        coll_op->variable_fn_params.rbuf_offset = 0;
        coll_op->variable_fn_params.count = scount;
        coll_op->variable_fn_params.dtype = sdtype;/* for zero copy, we want the
                                                    * native datatype and actual count
                                                    */
        coll_op->variable_fn_params.root = 0;

        /* you still need to copy in your own data into the rbuf */
        /* don't need to do this if you have in place data */
        if (!in_place) {
            memcpy((char *) rbuf + ompi_comm_rank(comm) * pack_len, sbuf, pack_len);
        }
    }

    coll_op->full_message.send_count = scount;
    coll_op->full_message.recv_count = rcount;

    coll_op->full_message.send_data_continguous = scontig;
    coll_op->full_message.recv_data_continguous = rcontig;

    ompi_datatype_get_extent(sdtype, &lb, &extent);
    coll_op->full_message.send_extent = (size_t) extent;

    ompi_datatype_get_extent(rdtype, &lb, &extent);
    coll_op->full_message.recv_extent = (size_t) extent;


    /* Fill in the function arguments */
    coll_op->variable_fn_params.sequence_num =
        OPAL_THREAD_ADD32(&(ml_module->collective_sequence_num), 1);
    coll_op->variable_fn_params.hier_factor = comm_size;

    MCA_COLL_ML_SET_ORDER_INFO(coll_op, n_fragments);


    ret = mca_coll_ml_launch_sequential_collective (coll_op);
    if (OMPI_SUCCESS != ret) {
        ML_VERBOSE(10, ("Failed to launch"));
        return ret;
    }

    *req = &coll_op->full_message.super;

    return OMPI_SUCCESS;
}

int mca_coll_ml_allgather(void *sbuf, int scount,
                          struct ompi_datatype_t *sdtype,
                          void* rbuf, int rcount,
                          struct ompi_datatype_t *rdtype,
                          struct ompi_communicator_t *comm,
                          mca_coll_base_module_t *module)
{
    ompi_request_t *req;
    int ret;

    ML_VERBOSE(10, ("Starting blocking allgather"));

    ret = mca_coll_ml_allgather_start (sbuf, scount, sdtype,
                                       rbuf, rcount, rdtype,
                                       comm, module, &req);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    ret = ompi_request_wait (&req, MPI_STATUS_IGNORE);

    ML_VERBOSE(10, ("Blocking allgather is complete"));

    return ret;
}

int mca_coll_ml_allgather_nb(void *sbuf, int scount,
                             struct ompi_datatype_t *sdtype,
                             void* rbuf, int rcount,
                             struct ompi_datatype_t *rdtype,
                             struct ompi_communicator_t *comm,
                             ompi_request_t **req,
                             mca_coll_base_module_t *module)
{
    int ret;

    ML_VERBOSE(10, ("Starting non-blocking allgather"));

    ret = mca_coll_ml_allgather_start (sbuf, scount, sdtype,
                                       rbuf, rcount, rdtype,
                                       comm, module, req);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    ML_VERBOSE(10, ("Non-blocking allgather started"));

    return ret;
}
