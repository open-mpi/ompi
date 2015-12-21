/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2013 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/include/ompi/constants.h"
#include "ompi/mca/bcol/bcol.h"
#include "bcol_ptpcoll_allreduce.h"

/*
 * Recursive K-ing allreduce
 */
static inline int bcol_ptpcoll_allreduce_narray_schedule_extra_node_exchange (mca_bcol_ptpcoll_module_t *ptpcoll_module, netpatterns_k_exchange_node_t *k_node,
                                                                              void *data_buffer, size_t data_size, ompi_request_t **requests, int *active_requests,
                                                                              int tag)
{
    ompi_communicator_t *comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    int peer_comm_rank, k, offset, rc;

    if (EXCHANGE_NODE == k_node->node_type) {
        /* the send data resides in the first part of the buffer */
        for (k = 0, offset = data_size ; k < k_node->n_extra_sources ; ++k, offset += data_size) {
            peer_comm_rank = ptpcoll_module->super.sbgp_partner_module->group_list[k_node->rank_extra_sources_array[k]];

            PTPCOLL_VERBOSE(10, ("Recv data from %d, addr %p len %d tag %d",
                                 peer_comm_rank, data_buffer, data_size, tag));
            rc = MCA_PML_CALL(irecv((void *)((unsigned char *)data_buffer + offset),
                                    data_size, MPI_BYTE, peer_comm_rank, tag, comm,
                                    &requests[*active_requests]));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to receive data"));
                return OMPI_ERROR;
            }

            ++(*active_requests);
        }
    } else {
        peer_comm_rank = ptpcoll_module->super.sbgp_partner_module->group_list[k_node->rank_extra_sources_array[0]];

        PTPCOLL_VERBOSE(10, ("Send data to %d, addr %p len %d tag %d",
                             peer_comm_rank, data_buffer, data_size, tag));

        rc = MCA_PML_CALL(isend(data_buffer, data_size, MPI_BYTE, peer_comm_rank,
                                tag, MCA_PML_BASE_SEND_STANDARD, comm,
                                &(requests[*active_requests])));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to send data"));
            return OMPI_ERROR;
        }

        ++(*active_requests);
    }

    return OMPI_SUCCESS;
}

static inline void bcol_ptpcoll_allreduce_narray_reduce (void *data_buffer, struct ompi_datatype_t *data_type, int count, struct ompi_op_t *op, int sources)
{
    size_t data_size = mca_bcol_base_get_buff_length(data_type, count);

    for (int k = 0, offset = data_size ; k < sources ; ++k, offset += data_size) {
        ompi_op_reduce(op, (char *) data_buffer + offset, data_buffer, count, data_type);
    }
}

static int bcol_ptpcoll_allreduce_narraying_progress (bcol_function_args_t *input_args,
                                                      struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *) const_args->bcol_module;
    void *data_buffer = (void *) ( (unsigned char *) input_args->sbuf +
                                   (size_t) input_args->sbuf_offset);
    struct ompi_datatype_t *data_type = input_args->dtype;
    uint32_t buffer_index = input_args->buffer_index;
    struct ompi_op_t *op = input_args->op;
    int count = input_args->count;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int tag = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag - 1;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    int k, rc, peer, group_peer;
    int offset = 0;
    ompi_communicator_t *comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;

    netpatterns_k_exchange_node_t *k_node = &ptpcoll_module->knomial_exchange_tree;
    int k_radix = k_node->tree_order;

    size_t data_size = mca_bcol_base_get_buff_length(data_type, count);
    int *iteration =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration);

    /* if we are just staring the collective and there are extra sources then schedule the
     * extra node exchange. otherwise check if the exchange is complete. */
    if (-1 == *iteration) {
        if (0 < k_node->n_extra_sources) {
            if (!(*active_requests)) {
                rc = bcol_ptpcoll_allreduce_narray_schedule_extra_node_exchange (ptpcoll_module, k_node, data_buffer, data_size,
                                                                             requests, active_requests, tag);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                    return rc;
                }
            }

            /* check for extra node exchange completion */
            if (!mca_bcol_ptpcoll_test_all_for_match (active_requests, requests, &rc)) {
                return (OMPI_SUCCESS == rc) ? BCOL_FN_STARTED : rc;
            }

            if (EXCHANGE_NODE == k_node->node_type) {
                bcol_ptpcoll_allreduce_narray_reduce (data_buffer, data_type, count, op, k_node->n_extra_sources);
            }
        }

        /* start recursive k-ing */
        *iteration = 0;
    }

    if (*iteration < k_node->n_exchanges) {
        if (*active_requests) {
            if (!mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc)) {
                return (OMPI_SUCCESS == rc) ? BCOL_FN_STARTED : rc;
            }

            ++(*iteration);
            bcol_ptpcoll_allreduce_narray_reduce (data_buffer, data_type, count, op, k_radix - 1);
        }
    }

    for ( ; *iteration < k_node->n_exchanges ; ++(*iteration)) {
        for (k = 0; k < k_radix - 1; k++) {
            group_peer = k_node->rank_exchanges[*iteration][k];

            peer = group_list[group_peer];

            PTPCOLL_VERBOSE(10, ("Send data to %d, addr %p len %d tag %d",
                                 peer, data_buffer, data_size, tag));
            rc = MCA_PML_CALL(isend(data_buffer, data_size, MPI_BYTE, peer, tag,
                                    MCA_PML_BASE_SEND_STANDARD, comm,
                                    &(requests[*active_requests])));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to send data"));
                return OMPI_ERROR;
            }

            ++(*active_requests);
        }

        for (k = 0, offset = data_size ; k < k_radix - 1 ; ++k, offset += data_size) {
            group_peer = k_node->rank_exchanges[*iteration][k];
            peer = group_list[group_peer];

            PTPCOLL_VERBOSE(10, ("Recv data from %d, addr %p len %d tag %d",
                                 peer, data_buffer, data_size, tag));
            rc = MCA_PML_CALL(irecv((void *)((unsigned char *)data_buffer + offset ),
                                    data_size, MPI_BYTE, peer, tag, comm,
                                    &requests[*active_requests]));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to receive data"));
                return OMPI_ERROR;
            }

            ++(*active_requests);
        }

        if (!mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc)) {
            return (OMPI_SUCCESS == rc) ? BCOL_FN_STARTED : rc;
        }

        bcol_ptpcoll_allreduce_narray_reduce (data_buffer, data_type, count, op, k_radix - 1);
    }

    /* ensure extra nodes get the result */
    if (0 < k_node->n_extra_sources)  {
        if (!(*active_requests)) {
            int peer_comm_rank;

            if (EXTRA_NODE == k_node->node_type) {
                peer_comm_rank = ptpcoll_module->super.sbgp_partner_module->group_list[k_node->rank_extra_sources_array[0]];

                PTPCOLL_VERBOSE(10, ("EXTRA_NODE: Recv data from %d, addr %p len %d tag %d",
                                     peer_comm_rank, data_buffer, data_size, tag));
                rc = MCA_PML_CALL(irecv(data_buffer, data_size, MPI_BYTE, peer_comm_rank,
                                        tag, comm, &requests[*active_requests]));
                if( OMPI_SUCCESS != rc ) {
                    PTPCOLL_VERBOSE(10, ("Failed to receive data"));
                    return OMPI_ERROR;
                }

                ++(*active_requests);
            } else {
                for (k = 0; k < k_node->n_extra_sources; k++) {
                    peer_comm_rank = ptpcoll_module->super.sbgp_partner_module->group_list[k_node->rank_extra_sources_array[k]];

                    PTPCOLL_VERBOSE(10, ("EXCHANGE_NODE: Send data to %d, addr %p len %d tag %d",
                                         peer_comm_rank, data_buffer, data_size, tag));
                    rc = MCA_PML_CALL(isend(data_buffer, data_size, MPI_BYTE, peer_comm_rank,
                                            tag, MCA_PML_BASE_SEND_STANDARD, comm,
                                            &(requests[*active_requests])));

                    if( OMPI_SUCCESS != rc ) {
                        PTPCOLL_VERBOSE(10, ("Failed to send data"));
                        return OMPI_ERROR;
                    }

                    ++(*active_requests);
                }
            }
        }

        if (!mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc)) {
            return (OMPI_SUCCESS == rc) ? BCOL_FN_STARTED : rc;
        }
    }

    return BCOL_FN_COMPLETE;
}

int bcol_ptpcoll_allreduce_narraying_init(bcol_function_args_t *input_args,
                                          struct mca_bcol_base_function_t *const_args){

    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;
    uint64_t sequence_number = input_args->sequence_num;
    uint32_t buffer_index = input_args->buffer_index;
    int count = input_args->count;
    struct ompi_datatype_t *dtype = input_args->dtype;
    size_t buffer_size;
    int tag;

    tag = (PTPCOLL_TAG_OFFSET + sequence_number * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag = tag = -tag;
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask = 1;

    /* start with extra node exchange if needed */
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration = -1;
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests = 0;
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].status = PTPCOLL_NOT_STARTED;

    /*
     * ML bufer is segmented into k segments and each of the k segment is used
     * for reductions
     */
    /* This has to be based on ml buffer size. Need to take into account the space used
     * by the headers of other bcol modules. */
    buffer_size  = ptpcoll_module->ml_mem.size_buffer - BCOL_HEADER_MAX;
    assert(buffer_size >= count * dtype->super.size *
           ptpcoll_module->k_nomial_radix);
    (void)buffer_size;  // silence compiler warning
    (void)dtype;
    (void)count;

    return bcol_ptpcoll_allreduce_narraying_progress (input_args, const_args);
}

static inline int compute_seg_index(int peer, int kpow_num, int tree_order) {

    int peer_base, peer_position, peer_base_rank, peer_index;

    peer_base = peer / (kpow_num * tree_order);
    peer_base_rank = peer_base * kpow_num * tree_order ;
    peer_position = peer_base_rank == 0 ? peer : peer % (peer_base_rank);
    peer_index    = peer_position / kpow_num ;

    return peer_index;
}

int compute_knomial_allgather_offsets(int group_index, int count, struct
                                      ompi_datatype_t *dtype,int k_radix,int n_exchanges,
                                      int **offsets){

    int modulo_group_size;
    size_t seg_count, seg_size, seg_index, seg_offset;
    size_t block_offset, block_count;
    int exchange_step;
    ptrdiff_t lb, extent;

    if (0 >= n_exchanges) {
        PTPCOLL_VERBOSE(10,("Nothing to initialize "));
        return 0;
    }
    modulo_group_size = 1;
    seg_count = count / k_radix;
    ompi_datatype_get_extent(dtype, &lb, &extent);
    seg_size = seg_count * extent;

    seg_index = group_index % k_radix;
    seg_offset = seg_index * seg_size;

    offsets[0][BLOCK_OFFSET] = block_offset = 0;
    offsets[0][BLOCK_COUNT] = block_count = count;
    offsets[0][LOCAL_REDUCE_SEG_OFFSET] = seg_offset;
    offsets[0][SEG_SIZE] = seg_size;


    for(exchange_step = 1; exchange_step < n_exchanges; exchange_step++) {

        /* Previous step's segment is this exchange step's block */
        block_count = seg_count;
        block_offset = seg_offset;

        /* Divide the segment into k parts */
        seg_count = seg_count / k_radix;
        seg_size = seg_count * extent;

        /* Among different segments in block, which segment should I reduce ? */
        /* For allgather phase, I will not send out this segment to peers */
        modulo_group_size *= k_radix;
        seg_index = compute_seg_index(group_index, modulo_group_size, k_radix);
        seg_offset = seg_index * seg_size;


        offsets[exchange_step][BLOCK_OFFSET] = block_offset;
        offsets[exchange_step][LOCAL_REDUCE_SEG_OFFSET] = seg_offset;
        offsets[exchange_step][BLOCK_COUNT] = block_count;
        offsets[exchange_step][SEG_SIZE] = seg_size;

        /* Change to absolute offset */
        seg_offset = block_offset + seg_offset;

    }

    return 0;
}

static inline int compute_send_segment_size(int block_offset,
                                            int send_offset,
                                            int segment_size,
                                            int padded_offset) {
    int send_size = -1;
    /* segment to be sent starts here */
    int segment_offset = block_offset + send_offset ;
    send_size = (segment_offset + segment_size) >= padded_offset ?
        segment_size - (segment_offset + segment_size - padded_offset) : segment_size;
    return send_size;
}

static inline int compute_recv_segment_size(int block_offset,
                                            int recv_offset,
                                            int segment_size,
                                            int padded_offset) {
    int recv_size = -1;
    /* segment to be sent starts here */
    int segment_offset = block_offset + recv_offset ;
    recv_size = (segment_offset + segment_size) >= padded_offset ?
        segment_size - (segment_offset + segment_size - padded_offset) : segment_size;

    return recv_size;
}

/*
 *
 * K-nomial Reduce Scatter
 * Example k=3 n=9
 *
 * | ABCDEFGH |0|
 *
 * Number of Exchange steps = log (basek) n
 * Number of steps in exchange step = k (radix)
 *
 * block_size = Size of data that is reduce in exchange step
 * segment_size = Size of data that is send or received by rank in radix step
 *
 * block_size = segment_size * k
 *
 * my_block_start_addr = Address of the segment in the block where I reference my
 * offsets
 *
 * This is version 1 : Experimenting with decoupling offset calcuations
 */
int bcol_ptpcoll_allreduce_recursivek_scatter_reduce(mca_bcol_ptpcoll_module_t *ptpcoll_module,
                                                     const int buffer_index, void *sbuf,
                                                     void *rbuf,
                                                     struct ompi_op_t *op,
                                                     const int count, struct ompi_datatype_t *dtype,
                                                     const int relative_group_index,
                                                     const int padded_start_byte){
    int blocks_in_step =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask;
    int tag = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag - 1;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    netpatterns_k_exchange_node_t *k_node = &ptpcoll_module->knomial_exchange_tree;
    mca_bcol_ptpcoll_component_t *cm =
        &mca_bcol_ptpcoll_component;
    void *my_block_start_addr = NULL, *my_block_addr = NULL;
    int i, k, group_peer, peer ;
    int k_radix = k_node->tree_order;
    int rc = OMPI_SUCCESS;
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int completed;
    void *my_recv_start_addr, *my_recv_addr;
    size_t block_offset, reduce_seg_offset, send_offset, recv_offset;
    int seg_size, block_size;
    int block_count, seg_count;
    ptrdiff_t lb, extent;
    ompi_datatype_get_extent(dtype, &lb, &extent);

    my_recv_start_addr = rbuf;
    my_block_start_addr = sbuf;
    block_count = count;
    block_size = count * extent;


    for (i = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration;
         i < k_node->n_exchanges; i++, blocks_in_step *= cm->narray_knomial_radix) {

        block_offset = ptpcoll_module->allgather_offsets[i][BLOCK_OFFSET];
        reduce_seg_offset = ptpcoll_module->allgather_offsets[i][LOCAL_REDUCE_SEG_OFFSET];
        block_count = ptpcoll_module->allgather_offsets[i][BLOCK_COUNT];
        seg_size = ptpcoll_module->allgather_offsets[i][SEG_SIZE];
        block_size = block_count * extent;

        PTPCOLL_VERBOSE(10,("Block offset %d, reduce_seg_offset %d, block_count %d seg_size %d",
                            block_offset, reduce_seg_offset, block_count, seg_size));

        seg_count = block_count / k_radix;
        my_block_addr = (void*)((char*)my_block_start_addr + block_offset);
        my_recv_addr = (void*)((char*)my_recv_start_addr + block_offset);

        for (k = 0; k < k_radix - 1; k++) {
            size_t soffset;
            int snd_size = 0;

            group_peer = k_node->rank_exchanges[i][k];
            peer = group_list[group_peer];

            send_offset = reduce_seg_offset + (seg_size * (k + 1));

            if ((int)send_offset + seg_size  > block_size) {
                send_offset = send_offset % block_size;
            }

            PTPCOLL_VERBOSE(10, ("Send data to %d,send offset %d len %d",
                                 peer, send_offset, seg_size));

            soffset = send_offset;
            snd_size =
                compute_send_segment_size((int)block_offset,(int)soffset,(int)seg_size,padded_start_byte);

            if (snd_size > 0) {
                rc = MCA_PML_CALL(isend((void *)((unsigned char *)my_block_addr
                                                 + soffset),
                                        snd_size, MPI_BYTE,
                                        peer, tag, MCA_PML_BASE_SEND_STANDARD, comm,
                                        &(requests[*active_requests])));

                if( OMPI_SUCCESS != rc ) {
                    PTPCOLL_VERBOSE(10, ("Failed to send the segment to %d", peer));
                    return OMPI_ERROR;
                }
                ++(*active_requests);
            }

        }

        /*
         * Receive the segments to tmp addr and then do a reduction
         */
        for (k = 0; k < k_radix - 1; k++) {
            int recv_size=0;

            group_peer = k_node->rank_exchanges[i][k];
            peer = group_list[group_peer];

            recv_offset = reduce_seg_offset + (seg_size * (k+1));

            if ((int)recv_offset + seg_size  > block_size) {
                recv_offset = recv_offset % block_size;
            }

            PTPCOLL_VERBOSE(10, ("Receive data to receive buffer at offset %d\n",
                                 recv_offset));
            recv_size = compute_recv_segment_size((int)block_offset,
                                                  (int)reduce_seg_offset, (int)seg_size,
                                                  padded_start_byte);

            if (recv_size > 0 ) {
                rc = MCA_PML_CALL(irecv((void *)((unsigned char *)
                                                 my_recv_addr + recv_offset),
                                        recv_size, MPI_BYTE,
                                        peer, tag, comm, &requests[*active_requests]));
                if( OMPI_SUCCESS != rc ) {
                    PTPCOLL_VERBOSE(10, ("Failed to receive the segment from %d", peer));
                    return OMPI_ERROR;
                }
                ++(*active_requests);
            }

        }

        completed = 0;
        while(!completed){
            completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        }

        /* Do a reduction on received buffers */
        {
            void *src_data_buffer = NULL, *dst_data_buffer = NULL;
            int reduce_data_count = 0;

            src_data_buffer = my_block_addr;
            dst_data_buffer = my_recv_addr;

            for (k = 0; k < k_radix - 1; k++) {
                recv_offset = reduce_seg_offset + (seg_size * (k+1));

                if ((int)recv_offset + seg_size  > block_size) {
                    recv_offset = recv_offset % block_size;
                }

                reduce_data_count = (int)(block_offset + reduce_seg_offset) + seg_size >= padded_start_byte ?
                    (seg_size - (((int)(block_offset + reduce_seg_offset) + seg_size) - padded_start_byte))/(int)dtype->super.size
                    : (int)seg_count;

                if (reduce_data_count > 0) {
                    ompi_3buff_op_reduce(op,
                                         (void*)((unsigned char*)my_recv_addr + recv_offset),
                                         (void*)((unsigned char*)src_data_buffer +
                                                 reduce_seg_offset),
                                         (void*)((unsigned char*)dst_data_buffer +
                                                 reduce_seg_offset),
                                         reduce_data_count,dtype);
                }

                src_data_buffer = dst_data_buffer;

            }
        }

        /* After first iteration we have data (to work with) in recv buffer */
        my_block_start_addr = rbuf;

    }

    return rc;
}


int bcol_ptpcoll_allreduce_knomial_allgather(mca_bcol_ptpcoll_module_t *ptpcoll_module,
                                             const int buffer_index,
                                             void *sbuf,void *rbuf, int count, struct
                                             ompi_datatype_t *dtype,
                                             const int relative_group_index,
                                             const int padded_start_byte){

    size_t block_offset = 0, send_offset = 0, recv_offset = 0;
    int seg_size=0, block_size=0;
    int i,k,completed;
    void *my_block_start_addr = rbuf, *my_block_addr;
    size_t block_count = count;
    netpatterns_k_exchange_node_t *k_node = &ptpcoll_module->knomial_exchange_tree;
    int k_radix = k_node->tree_order;
    int peer, group_peer;
    int rc = OMPI_SUCCESS;
    int tag = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag - 1;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    int exchange_step;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    ptrdiff_t lb, extent;
    ompi_datatype_get_extent(dtype, &lb, &extent);


    for (i = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration;
         i < k_node->n_exchanges; i++) {

        exchange_step = k_node->n_exchanges - 1 - i;

        block_offset = ptpcoll_module->allgather_offsets[exchange_step][BLOCK_OFFSET];
        send_offset = ptpcoll_module->allgather_offsets[exchange_step][LOCAL_REDUCE_SEG_OFFSET];
        block_count = ptpcoll_module->allgather_offsets[exchange_step][BLOCK_COUNT];
        seg_size = ptpcoll_module->allgather_offsets[exchange_step][SEG_SIZE];
        block_size = block_count * extent;


        PTPCOLL_VERBOSE(10, ("Send offset %d block_offset %d seg_size %\n",
                             send_offset, block_offset, seg_size));

        my_block_addr = (void*)((unsigned char*)my_block_start_addr + block_offset);

        for (k = 0; k < k_radix - 1; k++) {
            size_t soffset=0; int snd_size = 0;
            group_peer = k_node->rank_exchanges[exchange_step][k];
            peer = group_list[group_peer];

            soffset = send_offset;
            snd_size = compute_send_segment_size((int)block_offset,
                                                 (int)soffset,
                                                 (int)seg_size,
                                                 padded_start_byte);
            if (snd_size > 0) {
                rc = MCA_PML_CALL(isend((void *)((unsigned char *)my_block_addr
                                                 + soffset),
                                        snd_size, MPI_BYTE,
                                        peer, tag, MCA_PML_BASE_SEND_STANDARD, comm,
                                        &(requests[*active_requests])));

                if( OMPI_SUCCESS != rc ) {
                    PTPCOLL_VERBOSE(10, ("Failed to send the segment to %d", peer));
                    return OMPI_ERROR;
                }

                ++(*active_requests);
            }

            PTPCOLL_VERBOSE(10, ("Send data to receive buffer at offset %d to %d\n",
                                 send_offset, peer));
        }

        for (k = 0; k < k_radix - 1; k++) {
            int recv_size=0;

            group_peer = k_node->rank_exchanges[exchange_step][k];
            peer = group_list[group_peer];

            recv_offset = send_offset + (k + 1) * seg_size;

            if ((int)recv_offset + seg_size > block_size){
                recv_offset = recv_offset % block_size;
            }

            PTPCOLL_VERBOSE(10, ("Receive data to receive buffer at offset %d from %d\n",
                                 recv_offset, peer));


            recv_size = compute_recv_segment_size((int)block_offset,
                                                  (int)recv_offset,
                                                  (int)seg_size,
                                                  padded_start_byte);
            if (recv_size > 0) {
                rc = MCA_PML_CALL(irecv((void *)((unsigned char *)
                                                 my_block_addr + recv_offset),
                                        recv_size, MPI_BYTE,
                                        peer, tag, comm, &requests[*active_requests]));

                if( OMPI_SUCCESS != rc ) {
                    PTPCOLL_VERBOSE(10, ("Failed to receive the segment from %d", peer));
                    return OMPI_ERROR;
                }
                ++(*active_requests);
            }

        }

        completed = 0;
        while(!completed){
            completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        }

        block_count = block_count * k_radix;
        block_size = block_count * extent;

    }

    return rc;

}

static inline int compute_padding_count(int count, int k_radix, int n_exchanges){
    bool fpadding = false;
    size_t dsize;
    int i, pad_count=0, kpow;

    /* is padding required */
    dsize = count;
    kpow = 1;
    for ( i=0; i < n_exchanges; i++) {
        if (dsize % k_radix) {
            fpadding = true;
        }
        dsize /= k_radix;
        kpow *= k_radix;
    }

    if (fpadding) {
        pad_count = count % kpow;
        pad_count = kpow - pad_count;
    }

    return pad_count;
}


int bcol_ptpcoll_allreduce_recursivek_scatter_reduce_allgather_init(bcol_function_args_t *input_args,
                                                                    struct mca_bcol_base_function_t *const_args){

    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;
    struct ompi_op_t *op = input_args->op;
    int tag;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    uint64_t sequence_number = input_args->sequence_num;
    uint32_t buffer_index = input_args->buffer_index;
    void *src_buffer = (void *) (
        (unsigned char *)input_args->sbuf +
        (size_t)input_args->sbuf_offset);

    void *recv_buffer = (void *) (
        (unsigned char *)input_args->rbuf +
        (size_t)input_args->rbuf_offset);

    int count = input_args->count;
    struct ompi_datatype_t *dtype = input_args->dtype;
    int *iteration =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration);
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int *status =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].status);
    ptrdiff_t lb, extent;

    /* Get the knomial tree */
    netpatterns_k_exchange_node_t *k_node = &ptpcoll_module->knomial_exchange_tree;
    int k_radix = k_node->tree_order;
    int n_exchanges = k_node->n_exchanges;
    int padded_start_byte;
    int padding_count = compute_padding_count(count, k_radix, n_exchanges);

    ompi_datatype_get_extent(dtype, &lb, &extent);
    padded_start_byte = count * extent;


    /* Init for making the functions Re-entrant */
    tag = (PTPCOLL_TAG_OFFSET + sequence_number * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag = tag = -tag;
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask = 1;
    *active_requests = 0;
    *iteration = -1;
    *status = PTPCOLL_NOT_STARTED;
    *iteration = 0;

    compute_knomial_allgather_offsets(my_group_index,count + padding_count, dtype,k_radix,n_exchanges,
                                      ptpcoll_module->allgather_offsets);

    /* Perform a recursive k'ing reduce scatter */
    bcol_ptpcoll_allreduce_recursivek_scatter_reduce(ptpcoll_module, buffer_index,
                                                     src_buffer, recv_buffer, op, count + padding_count, dtype,
                                                     my_group_index,padded_start_byte);


    /* Perform a recursive k'ing allgather */
    bcol_ptpcoll_allreduce_knomial_allgather(ptpcoll_module,
                                             buffer_index,
                                             src_buffer, recv_buffer, count + padding_count, dtype,
                                             my_group_index, padded_start_byte);

    return BCOL_FN_COMPLETE;
}

int bcol_ptpcoll_allreduce_recursivek_scatter_reduce_extra(mca_bcol_ptpcoll_module_t *ptpcoll_module,
                                                           int buffer_index,
                                                           void *sbuf,
                                                           void *rbuf,
                                                           struct ompi_op_t *op,
                                                           const int count, struct ompi_datatype_t *dtype){
    int tag = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag - 1;
    netpatterns_k_exchange_node_t *k_node = &ptpcoll_module->knomial_exchange_tree;
    int k, peer ;
    int rc = OMPI_SUCCESS;
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    int block_count, block_size;
    char *tmprecv_buffer = NULL, *data_src_buffer, *data_dst_buffer;
    ptrdiff_t lb, extent;
    ompi_datatype_get_extent(dtype, &lb, &extent);

    block_count = count;
    block_size = count * extent;


    if (0 < block_size) {
        tmprecv_buffer = (void*)malloc(block_size);
    }

    data_src_buffer = sbuf;
    data_dst_buffer = rbuf;

    if (EXCHANGE_NODE == k_node->node_type) {
        for (k = 0; k < k_node->n_extra_sources; k++){

            peer = ptpcoll_module->super.sbgp_partner_module->group_list[
                k_node->rank_extra_sources_array[k]];

            rc = MCA_PML_CALL(recv((void *)((unsigned char *)tmprecv_buffer),
                                   block_size, MPI_BYTE,
                                   peer, tag, comm, MPI_STATUS_IGNORE));

            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to receive the segment from %d", peer));
                rc = OMPI_ERROR;
                goto clean;
            }

            ompi_3buff_op_reduce(op, (void*)((unsigned char*)data_src_buffer),
                                 (void*)((unsigned char*)tmprecv_buffer),
                                 (void*)((unsigned char*)data_dst_buffer),
                                 block_count,dtype);
            data_src_buffer = data_dst_buffer;
        }
    } else {
        peer = ptpcoll_module->super.sbgp_partner_module->group_list[
            k_node->rank_extra_sources_array[0]];

        rc = MCA_PML_CALL(send((void *)((unsigned char *)sbuf),
                               block_size, MPI_BYTE,
                               peer, tag, MCA_PML_BASE_SEND_STANDARD, comm));

        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to send data"));
            rc = OMPI_ERROR;
            goto clean;
        }
    }

clean:
    if (tmprecv_buffer) {
        free(tmprecv_buffer);
    }
    return rc;
}

int bcol_ptpcoll_allreduce_knomial_allgather_extra(mca_bcol_ptpcoll_module_t *ptpcoll_module,
                                                   int buffer_index,
                                                   void *sbuf,
                                                   void *rbuf,
                                                   const int count, struct ompi_datatype_t *dtype){
    int tag = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag - 1;
    netpatterns_k_exchange_node_t *k_node = &ptpcoll_module->knomial_exchange_tree;
    int k, peer ;
    int rc = OMPI_SUCCESS;
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    int block_size, completed;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    ptrdiff_t lb, extent;
    ompi_datatype_get_extent(dtype, &lb, &extent);


    block_size = count * extent;

    if (EXTRA_NODE == k_node->node_type) {
        peer = ptpcoll_module->super.sbgp_partner_module->group_list[
            k_node->rank_extra_sources_array[0]];

        rc = MCA_PML_CALL(irecv((void *)((unsigned char *)rbuf),
                                block_size, MPI_BYTE,
                                peer, tag, comm, &requests[*active_requests]));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to receive data"));
            return OMPI_ERROR;
        }

        ++(*active_requests);
    } else {
        for (k = 0; k < k_node->n_extra_sources; k++) {
            peer = ptpcoll_module->super.sbgp_partner_module->group_list[
                k_node->rank_extra_sources_array[k]];

            rc = MCA_PML_CALL(isend((void *)((unsigned char *)rbuf),
                                    block_size, MPI_BYTE,
                                    peer, tag, MCA_PML_BASE_SEND_STANDARD, comm,
                                    &(requests[*active_requests])));

            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to send data"));
                return OMPI_ERROR;
            }

            ++(*active_requests);
        }

    }

    completed = 0;

    while(!completed){
        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
    }

    return rc;
}

int bcol_ptpcoll_allreduce_recursivek_scatter_reduce_allgather_extra_init(bcol_function_args_t *input_args,
                                                                          struct mca_bcol_base_function_t *const_args){

    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;
    struct ompi_op_t *op = input_args->op;
    int tag;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    uint64_t sequence_number = input_args->sequence_num;
    uint32_t buffer_index = input_args->buffer_index;
    void *src_buffer = (void *) (
        (unsigned char *)input_args->sbuf +
        (size_t)input_args->sbuf_offset);

    void *recv_buffer = (void *) (
        (unsigned char *)input_args->rbuf +
        (size_t)input_args->rbuf_offset);

    int count = input_args->count;
    struct ompi_datatype_t *dtype = input_args->dtype;
    int *iteration =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration);
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int *status =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].status);
    ptrdiff_t lb, extent;
    /* Get the knomial tree */
    netpatterns_k_exchange_node_t *k_node = &ptpcoll_module->knomial_exchange_tree;
    int k_radix = k_node->tree_order;
    int n_exchanges = k_node->n_exchanges;
    int padded_start_byte;
    int padding_count = compute_padding_count(count, k_radix, n_exchanges);
    void *tmpsrc_buffer = NULL;

    ompi_datatype_get_extent(dtype, &lb, &extent);
    padded_start_byte = count * extent;

    /* Init for making the functions Re-entrant */
    tag = (PTPCOLL_TAG_OFFSET + sequence_number * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag = tag = -tag;
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask = 1;
    *active_requests = 0;
    *iteration = -1;
    *status = PTPCOLL_NOT_STARTED;
    *iteration = 0;

    compute_knomial_allgather_offsets(my_group_index,count + padding_count, dtype,k_radix,n_exchanges,
                                      ptpcoll_module->allgather_offsets);

    if (EXCHANGE_NODE == k_node->node_type) {
        bcol_ptpcoll_allreduce_recursivek_scatter_reduce_extra(ptpcoll_module,
                                                               buffer_index,
                                                               src_buffer, recv_buffer, op, count, dtype);
        tmpsrc_buffer = src_buffer;
        if ( k_node->n_extra_sources > 0){
            tmpsrc_buffer = recv_buffer;
        }
        bcol_ptpcoll_allreduce_recursivek_scatter_reduce(ptpcoll_module, buffer_index,
                                                         tmpsrc_buffer, recv_buffer, op, count + padding_count, dtype,
                                                         my_group_index,padded_start_byte);
        bcol_ptpcoll_allreduce_knomial_allgather(ptpcoll_module,
                                                 buffer_index,
                                                 src_buffer, recv_buffer, count + padding_count, dtype,
                                                 my_group_index, padded_start_byte);
        bcol_ptpcoll_allreduce_knomial_allgather_extra(ptpcoll_module,
                                                       buffer_index,
                                                       src_buffer, recv_buffer, count, dtype);

    }
    else if (EXTRA_NODE == k_node->node_type) {
        bcol_ptpcoll_allreduce_recursivek_scatter_reduce_extra(ptpcoll_module,
                                                               buffer_index,
                                                               src_buffer, recv_buffer, op, count, dtype);
        bcol_ptpcoll_allreduce_knomial_allgather_extra(ptpcoll_module,
                                                       buffer_index,
                                                       src_buffer, recv_buffer, count, dtype);
    }

    return BCOL_FN_COMPLETE;
}



/*
 * Register allreduce functions to the BCOL function table,
 * so they can be selected
 */
int bcol_ptpcoll_allreduce_init(mca_bcol_base_module_t *super)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module =
        (mca_bcol_ptpcoll_module_t *) super;

    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    comm_attribs.bcoll_type = BCOL_ALLREDUCE;
    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 1024 * 1024;

    /* not an accurate attribute, none of these algorithms
     * are non-blocking
     */
    comm_attribs.waiting_semantics = NON_BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000; /* range 1 */

    inv_attribs.datatype_bitmap = 0xffffffff;
    inv_attribs.op_types_bitmap = 0xffffffff;

    comm_attribs.data_src = DATA_SRC_KNOWN;

    mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                 bcol_ptpcoll_allreduce_narraying_init,
                                 bcol_ptpcoll_allreduce_narraying_progress);

    inv_attribs.bcol_msg_min = 10000000;
    inv_attribs.bcol_msg_max = 10485760; /* range 4 */

    if (ptpcoll_module->pow_knum == ptpcoll_module->group_size) {
        mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                     bcol_ptpcoll_allreduce_recursivek_scatter_reduce_allgather_init,
                                     NULL);

    } else {

        mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                                     bcol_ptpcoll_allreduce_recursivek_scatter_reduce_allgather_extra_init,
                                     NULL);

    }

    return OMPI_SUCCESS;
}
