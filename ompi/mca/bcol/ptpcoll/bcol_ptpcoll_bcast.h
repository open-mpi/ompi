/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_PTPCOLL_BCAST_H
#define MCA_BCOL_PTPCOLL_BCAST_H

#include "ompi_config.h"
#include "bcol_ptpcoll.h"
#include "bcol_ptpcoll_utils.h"

BEGIN_C_DECLS

int bcol_ptpcoll_bcast_init(mca_bcol_base_module_t *super);

int bcol_ptpcoll_bcast_k_nomial_anyroot (bcol_function_args_t *input_args, 
        struct mca_bcol_base_function_t *const_args);
int bcol_ptpcoll_bcast_k_nomial_anyroot_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);

int bcol_ptpcoll_bcast_k_nomial_known_root(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);
int bcol_ptpcoll_bcast_k_nomial_known_root_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);

int bcol_ptpcoll_bcast_binomial_scatter_gatther_anyroot(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);
int bcol_ptpcoll_bcast_binomial_scatter_gatther_anyroot_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);

int bcol_ptpcoll_bcast_binomial_scatter_gatther_anyroot_extra(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);
int bcol_ptpcoll_bcast_binomial_scatter_gatther_anyroot_extra_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);

int bcol_ptpcoll_bcast_binomial_scatter_gatther_known_root(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);
int bcol_ptpcoll_bcast_binomial_scatter_gatther_known_root_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);

int bcol_ptpcoll_bcast_binomial_scatter_gatther_known_root_extra(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);
int bcol_ptpcoll_bcast_binomial_scatter_gatther_known_root_extra_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);


/* macros */
#define K_NOMIAL_ROOT_BCAST_NB_BINOMIAL_SCATTER(                                                                    \
        radix_mask_pow,                                                                                             \
        my_group_index, group_size, group_list,                                                                     \
        data_buffer, segment_size, count, tag,                                                                      \
        comm, send_requests, num_pending_sends)                                                                     \
do {                                                                                                                \
    int rc = OMPI_SUCCESS;                                                                                          \
    int dst;                                                                                                        \
    int comm_dst;                                                                                                   \
    int send_size;                                                                                                  \
    int send_offset;                                                                                                \
    int delta;                                                                                                      \
    int dst_boundary_rank;                                                                                          \
    int radix_mask = radix_mask_pow >= 0 ? 1 << radix_mask_pow : 0;                                                 \
                                                                                                                    \
    while(radix_mask > 0) {                                                                                         \
        /* For each level of tree, do sends */                                                                      \
        dst = my_group_index ^ radix_mask;                                                                          \
        comm_dst = group_list[dst];                                                                                 \
                                                                                                                    \
        dst_boundary_rank = dst & ((~(int)0) << (radix_mask_pow));                                                  \
                                                                                                                    \
        send_offset = segment_size * dst_boundary_rank;                                                             \
        /* Pasha: make sure that we handle the corner cases */                                                      \
        delta = count - send_offset;                                                                                \
        if (delta <= 0) {                                                                                           \
            send_size = 0; /* we have to send something, other way it will hang */                                  \
        } else  {                                                                                                   \
            /* the tail case */                                                                                     \
            send_size = (int)                                                                                       \
            (delta - (int)segment_size * radix_mask) < 0 ? delta :                                                  \
            (int)segment_size * radix_mask;                                                                         \
        }                                                                                                           \
                                                                                                                    \
        /* Non blocking send .... */                                                                                \
        PTPCOLL_VERBOSE(9 ,                                                                                         \
                ("Bcast p2s, Isend to %d[%d],count %d,tag %d,addr %p [%p] send_size %d,send_offset %d, radix %d %d",\
                 dst, comm_dst, count, tag,                                                                         \
                 data_buffer, (void *)((unsigned char *)data_buffer + (size_t)send_offset),                         \
                 send_size,                                                                                         \
                 send_offset,                                                                                       \
                 radix_mask,                                                                                        \
                 radix_mask_pow                                                                                     \
                ));                                                                                                 \
        rc = MCA_PML_CALL(isend((void *)((unsigned char *)data_buffer + (size_t)send_offset),                       \
                    send_size, MPI_BYTE,                                                                            \
                    comm_dst, tag,                                                                                  \
                    MCA_PML_BASE_SEND_STANDARD, comm,                                                               \
                    &(send_requests[*num_pending_sends])));                                                         \
        PTPCOLL_VERBOSE(10, ("send request addr is %p", send_requests[*num_pending_sends]));                        \
        if( OMPI_SUCCESS != rc ) {                                                                                  \
            PTPCOLL_VERBOSE(10, ("Failed to isend data"));                                                          \
            return OMPI_ERROR;                                                                                      \
        }                                                                                                           \
        ++(*num_pending_sends);                                                                                     \
        radix_mask >>= 1;                                                                                           \
        radix_mask_pow--;                                                                                           \
    }                                                                                                               \
} while(0)

#define NARRAY_SCATTER_NB(narray_node, process_shift, group_size,                          \
        data_buffer, base_block_size, count, tag, comm, send_requests,                     \
        num_pending_sends)                                                                 \
do {                                                                                       \
    int n, rc = OMPI_SUCCESS;                                                              \
    int dst;                                                                               \
    int comm_dst;                                                                          \
    int offset;                                                                            \
    int size_count = count;                                                                \
    \
    /* Send out data to all relevant childrens  */                                         \
    for (n = 0; n < narray_node->n_children && size_count > 0; n++) {                      \
        \
        dst = narray_node->children_ranks[n] + process_shift;                              \
        if (dst >= group_size) {                                                           \
            dst -= group_size;                                                             \
        }                                                                                  \
        \
        comm_dst = group_list[dst];                                                        \
        offset = n * base_block_size;                                                      \
        size_count -= base_block_size;                                                     \
        if (OPAL_UNLIKELY(size_count < 0)) {                                               \
            count = base_block_size + size_count;                                          \
        } else {                                                                           \
            count = base_block_size;                                                       \
        }                                                                                  \
        \
        /* Non blocking send .... */                                                       \
        PTPCOLL_VERBOSE(9 , ("Bcast, Isend data to %d[%d], count %d, tag %d, addr %p",     \
                    dst, comm_dst, count, tag,                                             \
                    data_buffer));                                                         \
        rc = MCA_PML_CALL(isend((void *)((char *)data_buffer + (size_t)offset), count, MPI_BYTE,\
                    comm_dst, tag,                                                         \
                    MCA_PML_BASE_SEND_STANDARD, comm,                                      \
                    &(send_requests[*num_pending_sends])));                                \
        if( OMPI_SUCCESS != rc ) {                                                         \
            PTPCOLL_VERBOSE(10, ("Failed to isend data"));                                 \
            return OMPI_ERROR;                                                             \
        }                                                                                  \
        ++(*num_pending_sends);                                                            \
    }                                                                                      \
} while(0)

#define NARRAY_SCATTER_B(narray_node, process_shift, group_size,                                                    \
                          data_buffer, base_block_size, count, tag, comm, send_requests,                            \
                          num_pending_sends, completed)                                                             \
do {                                                                                                                \
    NARRAY_SCATTER_NB(narray_node, process_shift, group_size,                                                       \
            data_buffer, base_block_size, count, tag, comm, send_requests,                                          \
            num_pending_sends);                                                                                     \
    if (*num_pending_sends > 0) {                                                                                   \
        completed = mca_bcol_ptpcoll_test_all_for_match(num_pending_sends, send_requests, &rc);                     \
        if (OMPI_SUCCESS != rc) {                                                                                   \
            return OMPI_ERROR;                                                                                      \
        }                                                                                                           \
    } else {                                                                                                        \
        completed = 1;                                                                                              \
    }                                                                                                               \
} while (0)

#define CHECK_IF_ROOT_OR_VROOT(module, i)  \
    (module->pow_2 == module->ml_mem.ml_buf_desc[i].radix_mask_pow)

/* inline functions */
static inline __opal_attribute_always_inline__ 
int bcol_ptpcoll_bcast_binomial_scatter_gatther_send_extra(
        mca_bcol_ptpcoll_module_t *ptpcoll_module,
        void *data_buffer, int count, int tag, 
        int extra_peer, ompi_communicator_t *comm,
        int *active_requests, ompi_request_t **requests)
{
    int rc = OMPI_SUCCESS;
    int completed = 0;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;

    /* tag is -1 already */
    /* send the all data to your extra peer */
    PTPCOLL_VERBOSE(10, ("bcol_ptpcoll_bcast_binomial_scatter_gatther_send_extra to %d tag %d", 
                extra_peer, tag));
    rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,
                group_list[extra_peer], tag,
                MCA_PML_BASE_SEND_STANDARD, comm,
                &(requests[*active_requests])));
    if( OMPI_SUCCESS != rc ) {
        PTPCOLL_VERBOSE(10, ("Failed to send data"));
        return OMPI_ERROR;
    }

    ++(*active_requests);

    completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
    if (0 == completed) {
        PTPCOLL_VERBOSE(10, ("PR Extra send was not completed"));
        /* we have to store the iteration number somewhere */
        return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
    }

    return BCOL_FN_COMPLETE;
}

static inline __opal_attribute_always_inline__ 
int bcol_ptpcoll_send_n_extra(mca_bcol_ptpcoll_module_t *ptpcoll_module,
        void *data_buffer, int count, int tag, 
        int *extra_peers, int num_peers, int skip,
        ompi_communicator_t *comm,
        int *active_requests, ompi_request_t **requests)
{
    int rc = OMPI_SUCCESS;
    int completed = 0;
    int i;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;

    /* send the all data to your extra peer */
    for (i = 0; i < num_peers; i++) {
        PTPCOLL_VERBOSE(10, ("send_n_extra to %d tag %d", 
                    extra_peers[i], tag));
        if (extra_peers[i] == skip) {
            PTPCOLL_VERBOSE(10, ("SKIP"));
            continue;
        }

        rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,
                    group_list[extra_peers[i]], tag,
                    MCA_PML_BASE_SEND_STANDARD, comm,
                    &(requests[*active_requests])));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to send data"));
            return OMPI_ERROR;
        }

        ++(*active_requests);
    }

    completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
    if (0 == completed) {
        PTPCOLL_VERBOSE(10, ("PR Extra send was not completed"));
        /* we have to store the iteration number somewhere */
        return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
    }

    return BCOL_FN_COMPLETE;
}

static inline __opal_attribute_always_inline__ 
int bcol_ptpcoll_bcast_binomial_gather_anyroot(mca_bcol_ptpcoll_module_t *ptpcoll_module,
        int buffer_index, void *data_buffer, int count, int base_block_size)
{
    int rc;
    int completed = 0; /* not completed */
    int *active_requests = 
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int i;
    int *iteration = 
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration;
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests = 
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    void *curr_data_sbuffer = NULL, 
         *curr_data_rbuffer = NULL;
    int radix_mask_pow = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask_pow;
    int delta;
    int tag = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag - 1;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;

    PTPCOLL_VERBOSE(10, ("bcol_ptpcoll_bcast_binomial_gather_anyroot %d %d %d",
                ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration,
                ptpcoll_module->pow_2,
                1 << ptpcoll_module->pow_2));

    /* we assume the iteration #iteration already was completed with probe */
    for (i = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration; 
            i < ptpcoll_module->pow_2; i++) {
        int pow2 = 1 << i;
        int peer_index = my_group_index ^ pow2;
        int comm_rank  = group_list[peer_index];
        int slen, rlen, 
            send_offset, 
            recv_offset;

        if (i > radix_mask_pow) {
            /* *active_requests = 0; */
            /* send - receive data from the peer */
            slen = rlen = pow2 * base_block_size;
            send_offset = base_block_size * ((my_group_index) & ((~(int)0) << i));
            recv_offset = base_block_size * ((peer_index)     & ((~(int)0) << i));
            curr_data_sbuffer = (void *)((unsigned char *)data_buffer + send_offset);
            curr_data_rbuffer = (void *)((unsigned char *)data_buffer + recv_offset);

            delta = count - recv_offset;
            if (delta > 0) {
                if (delta < rlen) {
                    /* recv the tail */
                    rlen = delta;
                }
                PTPCOLL_VERBOSE(10, ("[ pow2 %d, radix %d ] recv data %p (offset %d) , len %d , dest %d",
                            pow2,
                            1 << ptpcoll_module->pow_2,
                            curr_data_rbuffer,
                            recv_offset,
                            rlen,
                            comm_rank));
                rc = MCA_PML_CALL(irecv(curr_data_rbuffer, rlen, MPI_BYTE, 
                            comm_rank, tag, comm, &requests[*active_requests]));
                if( OMPI_SUCCESS != rc ) {
                    PTPCOLL_VERBOSE(10, ("Failed to receive data"));
                    return OMPI_ERROR;
                }
                ++(*active_requests);
            }

            delta = count - send_offset;
            if (delta > 0) {
                if (delta < slen) {
                    /* recv the tail */
                    slen = delta;
                }
                PTPCOLL_VERBOSE(10, ("[ pow2 %d, radix %d ] sending data %p (offset %d) , len %d , dest %d",
                            pow2,
                            1 << ptpcoll_module->pow_2,
                            curr_data_sbuffer,
                            send_offset,
                            slen,
                            comm_rank));
                rc = MCA_PML_CALL(isend(curr_data_sbuffer, slen, MPI_BYTE,
                            comm_rank, tag,
                            MCA_PML_BASE_SEND_STANDARD, comm,
                            &(requests[*active_requests])));
                if( OMPI_SUCCESS != rc ) {
                    PTPCOLL_VERBOSE(10, ("Failed to send data"));
                    return OMPI_ERROR;
                }
                ++(*active_requests);
            }

            if (*active_requests > 0) {
                completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
                if (0 == completed) {
                    *iteration = i;
                    /* we have to store the iteration number somewhere */
                    return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
                }
            }
        } else if (i == radix_mask_pow) {
            /* only receive data */
            rlen = pow2 * base_block_size;
            recv_offset = base_block_size * ((peer_index)     & ((~(int)0) << i));
            curr_data_rbuffer = (void *)((unsigned char *)data_buffer + recv_offset);
            delta = count - recv_offset;
            if (0 >= delta) {
                /* we have nothing to send, skip the iteration */
                continue;
            }
            if (delta < rlen) {
                /* recv the tail */
                rlen = delta;
            }
            /* receive data from the peer */
            PTPCOLL_VERBOSE(10, ("[ pow2 %d, radix %d ] recv data %p (offset %d) , len %d , dest %d",
                        pow2,
                        1 << ptpcoll_module->pow_2,
                        curr_data_rbuffer,
                        recv_offset,
                        rlen,
                        comm_rank));
            rc = MCA_PML_CALL(irecv(curr_data_rbuffer, rlen, MPI_BYTE, 
                        comm_rank, tag, comm, &(requests[*active_requests])));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to receive data"));
                return OMPI_ERROR;
            }
            ++(*active_requests);
            completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
            if (0 == completed) {
                *iteration = i;
                PTPCOLL_VERBOSE(10, ("Recv was not completed"));
                /* we have to store the iteration number somewhere */
                return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
            }
            PTPCOLL_VERBOSE(10, ("Recv was completed"));
        } else if (i < radix_mask_pow) {
            /* Only send data */
            slen = pow2 * base_block_size;
            send_offset = base_block_size * ((my_group_index) & ((~(int)0) << i));
            curr_data_sbuffer = (void *)((unsigned char *)data_buffer + send_offset);
            delta = count - send_offset;
            if (0 >= delta) {
                /* we have nothing to send, skip the iteration */
                continue;
            }
            if (delta < slen) {
                slen = delta;
            }
            PTPCOLL_VERBOSE(10, ("[ pow2 %d, radix %d ] sending data %p (offset %d) , len %d , dest %d",
                        pow2,
                        1 << ptpcoll_module->pow_2,
                        curr_data_sbuffer,
                        send_offset,
                        slen,
                        comm_rank));
            rc = MCA_PML_CALL(isend(curr_data_sbuffer, slen, MPI_BYTE,
                        comm_rank, tag, MCA_PML_BASE_SEND_STANDARD, comm,
                        &(requests[*active_requests])));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to send data"));
                return OMPI_ERROR;
            }
            ++(*active_requests);
            completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
            if (0 == completed) {
                *iteration = i;
                /* we have to store the iteration number somewhere */
                return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
            }
        }
    }

    return BCOL_FN_COMPLETE;
}

static inline __opal_attribute_always_inline__ 
int bcol_ptpcoll_bcast_binomial_probe_and_scatter_anyroot(mca_bcol_ptpcoll_module_t *ptpcoll_module,
        int buffer_index, void *data_buffer, int count, int base_block_size)
{
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    int rc;
    int completed = 0; /* not completed */
    int comm_root;
    int i;
    int *radix_mask_pow = 
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask_pow);
    int *active_requests = 
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_status_public_t status;
    ompi_request_t **requests = 
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    int pow2_group_size = ptpcoll_module->pow_2num;
    int pow2_distance;
    int my_left_boundary_rank;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int group_root_index = 0;
    void *curr_data_buffer = NULL;
    int tag = 
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag;
    int recv_count = 0;
    int *coll_status = 
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].status;

    assert(0 == *active_requests);

    PTPCOLL_VERBOSE(10, ("Running bcol_ptpcoll_bcast_binomial_probe_and_scatter_anyroot"));
    for (i = 0; i < cm->num_to_probe && 
            0 == completed; i++) {
        MCA_PML_CALL(iprobe(MPI_ANY_SOURCE, tag,
                    comm, &completed, &status));
        PTPCOLL_VERBOSE(10, ("Bcast, iprobe tag %d",
                    tag));
    }

    /* the function always returns OMPI_SUCCESS, so we don't check return code */
    if (0 == completed) {
        PTPCOLL_VERBOSE(10, ("IPROBE was not matched"));
        /* No data was received, return no match error */
        return BCOL_FN_NOT_STARTED;
    }

    comm_root = status.MPI_SOURCE;


    PTPCOLL_VERBOSE(9, ("IPROBE was matched, root of the data on communicator is %d", comm_root));

    /* For proxy we have to check if we got something from extra node */
    if (PTPCOLL_PROXY & ptpcoll_module->pow_2type) {
        if (group_list[ptpcoll_module->proxy_extra_index] == comm_root) {
            PTPCOLL_VERBOSE(9, ("IPROBE was matched, root of the data on communicator is extra node %d", 
                        comm_root));
            /* scatter the data among other peer in the pow2 group */
            *radix_mask_pow =  ptpcoll_module->pow_2;

            pow2_distance  = ptpcoll_module->pow_2 - 1;
            curr_data_buffer = data_buffer;
            recv_count = count;
            goto PR_SCATTHER;
        }
    } 

    /* Find group index for communicator root of the data */
    group_root_index = get_group_index_and_distance_for_binomial
        (my_group_index, comm_root, pow2_group_size, group_list, &pow2_distance);
    if (OPAL_UNLIKELY(group_root_index < 0)) {
        PTPCOLL_ERROR(("Fatal error, no group root index found, my id %d, pow2_g_size %d comm_root %d", 
                    my_group_index, pow2_group_size, comm_root));
        return OMPI_ERROR;
    }

    PTPCOLL_VERBOSE(10, ("Group root index is %d distance is %d", 
                group_root_index, pow2_distance));

    /* Use group_root_index to calculate the */

    /* Post receive that will fetch the data */
    /* Pasha: Who is packing data ? 
       Should I assume that we get contiguous buffer ? 
       Or should I pack by myself 
       ===================================================================================================
       === On this stage I assume that data is contiguous. So I use MPI_BYTE datatype and COUNT = size ===
       ===================================================================================================
     */

    recv_count = base_block_size * (1 << pow2_distance); /* we may receive larger data */

    my_left_boundary_rank = my_group_index & ((~(int)0) << pow2_distance );

    curr_data_buffer = (void *)((unsigned char *)data_buffer + 
            (size_t) base_block_size * my_left_boundary_rank);

    *radix_mask_pow = pow2_distance;

    pow2_distance--;

PR_SCATTHER:
    PTPCOLL_VERBOSE(10, ("Bcast, receive data from %d[%d], "
                "recv_count %d, tag %d, addr %p, offset %d, pow2_distace %d",
                comm_root, group_root_index, recv_count, 
                tag, curr_data_buffer, 
                my_group_index * base_block_size, pow2_distance));

    rc = MCA_PML_CALL(recv(curr_data_buffer, recv_count, MPI_BYTE, 
                comm_root, tag, comm, MPI_STATUS_IGNORE));
    if( OMPI_SUCCESS != rc ) {
        PTPCOLL_VERBOSE(10, ("Failed to receive data"));
        return OMPI_ERROR;
    }

    PTPCOLL_VERBOSE(10, ("Bcast, Data was received"));

    /* Sending forward the data over K-nomial tree */
    *coll_status = PTPCOLL_SCATTER_STARTED;
    K_NOMIAL_ROOT_BCAST_NB_BINOMIAL_SCATTER(
            pow2_distance,
            my_group_index, group_size, group_list,
            data_buffer, base_block_size, 
            count, tag, comm, requests,
            active_requests);

    /* Since the next step (gather) does not really require 
       completion on scatter , we may return complete  */
    return BCOL_FN_COMPLETE;
}

static inline __opal_attribute_always_inline__
int bcol_ptpcoll_binomial_root_to_src(int group_root, int my_rank, 
        int pow2_size, int group_size, int *distance)
{
    int root, relative_rank, src, 
        pow2_distance = 0, i;

    if (group_root < pow2_size) {
        root = group_root;
    } else {
        /* the source of the data is extra node,
           the real root it represented by some rank from
           pow2 group */
        root = group_root - pow2_size;
        /* shortcut for the case when my rank is root for the group */
        if (my_rank == root) {
            *distance = -1;
            return group_root;
        }
    }

    relative_rank = (my_rank - root) < 0 ? my_rank - root + pow2_size :
                                           my_rank - root;

    for (i = 1; i < pow2_size; i<<=1, pow2_distance++) {
        if (relative_rank & i) {
            src = my_rank ^ i; 
            if (src >= pow2_size) 
                src -= pow2_size;

            *distance = pow2_distance;
            return src;
        }   
    }

    /* error case */
    *distance = -1;
    return -1;
}

static inline __opal_attribute_always_inline__
int bcol_ptpcoll_bcast_binomial_test_and_scatter_known_root(mca_bcol_ptpcoll_module_t *ptpcoll_module,
        int buffer_index, void *data_buffer, int count, int base_block_size)
{
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    int rc;
    int *active_requests = 
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests = 
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int tmp_radix_mask_pow = 
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask_pow - 1;
    int tag = 
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag;
    int *status = 
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].status;

    PTPCOLL_VERBOSE(10, ("Running bcol_ptpcoll_bcast_binomial_probe_and_scatter_anyroot"));

    if (0 == mca_bcol_ptpcoll_test_all_for_match(active_requests, 
                requests, &rc)) {
        PTPCOLL_VERBOSE(10, ("Test was not matched - %d", rc));
        return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
    }

    PTPCOLL_VERBOSE(10, ("Bcast, Data was received"));

    /* Sending forward the data over binimial nomial tree */
    *status = PTPCOLL_SCATTER_STARTED;
    K_NOMIAL_ROOT_BCAST_NB_BINOMIAL_SCATTER(
            tmp_radix_mask_pow,
            my_group_index, group_size, group_list,
            data_buffer, base_block_size, 
            count, tag, comm, requests,
            active_requests);


    return BCOL_FN_COMPLETE;
}

#define NARRAY_BLOCK_SIZE(size, module, level_size)                                      \
                     ((size + (module)->full_narray_tree_num_leafs - 1) /                \
                     (module)->full_narray_tree_num_leafs) *                             \
                     ((module)->full_narray_tree_num_leafs /                             \
                     ((0 == level_size) ?                                                \
                      mca_bcol_ptpcoll_component.narray_knomial_radix :                  \
                      level_size))     

static inline __opal_attribute_always_inline__
int bcol_ptpcoll_bcast_narray_test_and_scatter_known_root(mca_bcol_ptpcoll_module_t *ptpcoll_module,
        int buffer_index, void *data_buffer, int count, int process_shift, 
        int relative_group_index)
{
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    int rc;
    int *active_requests = 
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests = 
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    int tag = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag;
    int *status = 
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].status;
    int scatter_count = 0;
    int offset = 0;
    int base_block_size = 0;
    void *curr_data_buffer = NULL;

    PTPCOLL_VERBOSE(10, ("Running bcol_ptpcoll_bcast_narray_test_and_scatter_known_root"));

    if (0 == mca_bcol_ptpcoll_test_all_for_match(active_requests, 
                requests, &rc)) {
        PTPCOLL_VERBOSE(10, ("Test was not matched - %d", rc));
        return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
    }

    /* Sending forward the data over binimial nomial tree */
    *status = PTPCOLL_SCATTER_STARTED;
    if(0 == relative_group_index) {
        scatter_count = count;
    } else {
        scatter_count = NARRAY_BLOCK_SIZE(count, ptpcoll_module, 
                ptpcoll_module->narray_knomial_node[relative_group_index].level_size);
    }

    offset = scatter_count * 
        ptpcoll_module->narray_knomial_node[relative_group_index].rank_on_level;

    /* make sure that we do not overun memory */
    if (OPAL_UNLIKELY(offset + scatter_count > count)) {
        scatter_count = count - offset;
    }

    PTPCOLL_VERBOSE(10, ("Bcast, Data was received %d %d %d",
                scatter_count,
                ptpcoll_module->narray_knomial_node[relative_group_index].level_size,
                ptpcoll_module->narray_knomial_node[relative_group_index].rank_on_level));


    curr_data_buffer = (void *)((unsigned char *)data_buffer + (size_t)offset);

    /* calculating scatter block size for next level of tree */
    base_block_size = NARRAY_BLOCK_SIZE(count, ptpcoll_module, 
        ptpcoll_module->narray_knomial_node[relative_group_index].level_size * 
        mca_bcol_ptpcoll_component.narray_knomial_radix);

    PTPCOLL_VERBOSE(10, ("scatter_known_rootaaa %d %d %d %d %d",scatter_count, offset, base_block_size, 
                ptpcoll_module->narray_knomial_node[relative_group_index].level_size /mca_bcol_ptpcoll_component.narray_knomial_radix,
                ptpcoll_module->full_narray_tree_num_leafs));

    NARRAY_SCATTER_NB((&ptpcoll_module->narray_knomial_node[relative_group_index]), 
            process_shift, ptpcoll_module->full_narray_tree_size,
            curr_data_buffer, base_block_size, scatter_count, tag, comm, 
            requests, active_requests);

    /* Bummer, I tried to prevent this, special case for virtual root */
    if(0 == relative_group_index) {
        if (0 == mca_bcol_ptpcoll_test_all_for_match(active_requests, 
                    requests, &rc)) {
            PTPCOLL_VERBOSE(10, ("Test was not matched - %d", rc));
            *status = PTPCOLL_ROOT_SEND_STARTED;
            return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
        }
    }

    return BCOL_FN_COMPLETE;
}

static inline __opal_attribute_always_inline__ 
int bcol_ptpcoll_bcast_narray_knomial_gather(mca_bcol_ptpcoll_module_t *ptpcoll_module,
        const int buffer_index, void *data_buffer, const int count,
        const int relative_group_index)
{
    int completed = 0; /* not completed */
    int *active_requests = 
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int blocks_in_step = 
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask;
    int tag = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag - 1;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    int group_size = ptpcoll_module->full_narray_tree_size;
    int i, k,
        rc,
        len, slen, rlen,
        peer, group_peer;
    size_t s_offset, 
           r_offset;
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests = 
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    netpatterns_narray_knomial_tree_node_t *narray_node =
        &ptpcoll_module->narray_knomial_node[relative_group_index];
    netpatterns_k_exchange_node_t *k_node =
        &narray_node->k_node;
    mca_bcol_ptpcoll_component_t *cm =
        &mca_bcol_ptpcoll_component;
    size_t base_block_size = 
        NARRAY_BLOCK_SIZE(count, ptpcoll_module, narray_node->level_size);

    PTPCOLL_VERBOSE(10, ("bcol_ptpcoll_bcast_narray_knomial_gather %d %d %d %d %d %d %d",
                ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration,
                base_block_size, count, narray_node->level_size,
                relative_group_index, k_node->n_exchanges, tag));

    /* we assume the iteration #iteration already was completed with probe */
    for (i = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration; 
            i < k_node->n_exchanges; i++, blocks_in_step *= cm->narray_knomial_radix) {

        len = base_block_size * blocks_in_step;

        for (k = 0; k < cm->narray_knomial_radix - 1; k++) {
            group_peer = my_group_index + 
                (k_node->rank_exchanges[i][k] - narray_node->rank_on_level);
            if (group_peer >= group_size) {
                group_peer -= group_size;
            } else if (group_peer < 0) {
                group_peer += group_size;
            }
            peer = group_list[group_peer];

            r_offset = (size_t)k_node->rank_exchanges[i][k] / blocks_in_step *
                len;

            /* check that we do not run out of message boundary */
            if (OPAL_UNLIKELY(r_offset + len > (size_t)count)) {
                rlen = count - r_offset;
                if (OPAL_UNLIKELY(rlen <= 0)) {
                    continue;
                }
            } else {
                rlen = len;
            }
            PTPCOLL_VERBOSE(10, ("Recv data from %d, addr %p offset %d len %d %d %d tag %d",
                        peer, data_buffer, r_offset, rlen, len, blocks_in_step, tag));
            rc = MCA_PML_CALL(irecv((void *)((unsigned char *)data_buffer + r_offset), 
                        rlen, MPI_BYTE, 
                        peer, tag, comm, &requests[*active_requests]));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to receive data"));
                return OMPI_ERROR;
            }
            ++(*active_requests);
        }

        for (k = 0; k < cm->narray_knomial_radix - 1; k++) {
            group_peer = my_group_index + 
                (k_node->rank_exchanges[i][k] - narray_node->rank_on_level);
            if (group_peer >= group_size) {
                group_peer -= group_size;
            } else if (group_peer < 0) {
                group_peer += group_size;
            }
            peer = group_list[group_peer];

            s_offset = (size_t)narray_node->rank_on_level / blocks_in_step *
                len;

            /* check that we do not run out of message boundary */
            if (OPAL_UNLIKELY(s_offset + len > (size_t)count)) {
                slen =  count - s_offset;
                if (OPAL_UNLIKELY(slen <= 0)) {
                    continue;
                }
            } else {
                slen = len;
            }

            PTPCOLL_VERBOSE(10, ("Send data from %d, addr %p offset %d len %d %d %d tag %d",
                        peer, data_buffer, s_offset, slen, len, blocks_in_step, tag));
            rc = MCA_PML_CALL(isend((void *)((unsigned char *)data_buffer + s_offset), 
                        slen, MPI_BYTE,
                        peer, tag, MCA_PML_BASE_SEND_STANDARD, comm,
                        &(requests[*active_requests])));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to send data"));
                return OMPI_ERROR;
            }
            ++(*active_requests);
        }

        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if (0 == completed) {
            /* cache data for next iteration */
            ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration = 
                i; /* why not to store step for next iteration ?! */
            ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask = 
                blocks_in_step * cm->narray_knomial_radix;
            return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
        }
    }

    return BCOL_FN_COMPLETE;
}

END_C_DECLS

#endif
