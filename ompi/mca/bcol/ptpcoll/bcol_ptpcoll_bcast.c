/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/include/ompi/constants.h"
#include "ompi/mca/bcol/bcol.h"
#include "bcol_ptpcoll_bcast.h"
#include "bcol_ptpcoll_utils.h"

#define K_NOMIAL_ROOT_BCAST_NB_NOTEST(step_info, radix,                                    \
        my_group_index, group_list,                                                        \
        data_buffer, count, tag, comm, send_requests, num_pending_sends)                   \
do {                                                                                       \
    int rc = OMPI_SUCCESS;                                                                 \
    int dst;                                                                               \
    int comm_dst;                                                                          \
    *num_pending_sends = 0;                                                                \
                                                                                           \
    while(MCA_COMMON_NETPATTERNS_GET_NEXT_KNOMIAL_PEER_CHECK_LEVEL(step_info)) {           \
        /* For each level of tree, do sends */                                             \
        MCA_COMMON_NETPATTERNS_GET_NEXT_KNOMIAL_PEER(my_group_index,                       \
                                                     radix, step_info, dst);               \
        comm_dst = group_list[dst];                                                        \
                                                                                           \
            /* Non blocking send .... */                                                   \
        PTPCOLL_VERBOSE(9 , ("Bcast, Isend data to %d[%d], count %d, tag %d, addr %p",     \
                    dst, comm_dst, count, tag,                                             \
                    data_buffer));                                                         \
        rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,                              \
                    comm_dst, tag,                                                         \
                    MCA_PML_BASE_SEND_STANDARD, comm,                                      \
                    &(send_requests[*num_pending_sends])));                                \
        PTPCOLL_VERBOSE(10, ("send request addr is %p", send_requests[*num_pending_sends]));   \
        if( OMPI_SUCCESS != rc ) {                                                         \
            PTPCOLL_VERBOSE(10, ("Failed to isend data"));                                 \
            return OMPI_ERROR;                                                             \
        }                                                                                  \
        ++(*num_pending_sends);                                                            \
    }                                                                                      \
} while(0)

#define NARRAY_BCAST_NB(narray_node, process_shift, group_size,                            \
                        data_buffer, count, tag, comm, send_requests,                      \
                        num_pending_sends)                                                 \
do {                                                                                       \
    int n, rc = OMPI_SUCCESS;                                                              \
    int dst;                                                                               \
    int comm_dst;                                                                          \
                                                                                           \
        /* Send out data to all relevant childrens  */                                     \
    for (n = 0; n < narray_node->n_children; n++) {                                        \
                                                                                           \
        dst = narray_node->children_ranks[n] + process_shift;                              \
        if (dst >= group_size) {                                                           \
            dst -= group_size;                                                             \
        }                                                                                  \
        comm_dst = group_list[dst];                                                        \
                                                                                           \
        /* Non blocking send .... */                                                       \
        PTPCOLL_VERBOSE(9 , ("Bcast, Isend data to %d[%d], count %d, tag %d, addr %p",     \
                    dst, comm_dst, count, tag,                                             \
                    data_buffer));                                                         \
        rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,                              \
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


int bcol_ptpcoll_bcast_k_nomial_anyroot_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    int completed = 0;
    int rc;
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;
    uint32_t buffer_index = input_args->buffer_index;

    ompi_request_t **send_requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);

    completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, send_requests, &rc);
    if (OMPI_SUCCESS != rc) {
        return OMPI_ERROR;
    }

    /* DONE */
    if(completed) {
        PTPCOLL_VERBOSE(10, ("bcast root is done"));
        return BCOL_FN_COMPLETE;
    } else {
        PTPCOLL_VERBOSE(10, ("bcast root is started"));
        return BCOL_FN_STARTED;
    }
}

/* K-nomial tree ( with any root ) algorithm */
int bcol_ptpcoll_bcast_k_nomial_anyroot(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;

    int tag;
    int rc;
    int matched = 0; /* not matched */
    int comm_root = 0; /* no root */
    int i;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    int radix = ptpcoll_module->k_nomial_radix;
    int root_radix_mask = ptpcoll_module->pow_knum;
    int peer = -1;
    uint64_t sequence_number = input_args->sequence_num;
    uint32_t buffer_index = input_args->buffer_index;
    int extra_root = -1;

    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_status_public_t status;
    ompi_request_t **send_requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    netpatterns_knomial_step_info_t step_info = {0, 0, 0};

    PTPCOLL_VERBOSE(3, ("BCAST Anyroot, index_this_type %d, num_of_this_type %d",
                    const_args->index_of_this_type_in_collective + 1,
                    const_args->n_of_this_type_in_collective));

    /* keep tag within the limit support by the pml */
    tag = (PTPCOLL_TAG_OFFSET + sequence_number * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;
    /* reset requests */
    *active_requests = 0;

    PTPCOLL_VERBOSE(8, ("bcol_ptpcoll_bcast_k_nomial_anyroot, buffer index: %d \n"
                         "tag: %d "
                         "tag_mask: %d "
                         "sn: %d "
                         "root: %d "
                         "pow_k: %d %d "
                         "buff: %p "
                         "radix: %d",
                         buffer_index,  tag,
                         ptpcoll_module->tag_mask, sequence_number,
                         input_args->root_flag,
                         ptpcoll_module->pow_k, ptpcoll_module->pow_knum,
                         data_buffer,
                         radix));

    if (input_args->root_flag) {
        PTPCOLL_VERBOSE(10, ("I'm root of the data"));
        /*
         * I'm root of the operation
         * send data to (k - 1) * log base k N neighbors
         */
        MCA_COMMON_NETPATTERNS_GET_NEXT_KNOMIAL_INIT(step_info,
                ptpcoll_module->pow_knum, my_group_index);
        K_NOMIAL_ROOT_BCAST_NB_NOTEST(step_info, radix,
                my_group_index, group_list,
                data_buffer, count, tag, comm, send_requests,
                active_requests);

        goto ANY_ROOT_KNOMIAL_EXTRA;
    }

    /*
     * I'm not root, and I don't know to calculate root, so just
     * wait for data from ANY_SOURCE, once you get it, proceed like a root
     */

    for (i = 0; i < cm->num_to_probe; i++) {
        MCA_COMMON_NETPATTERNS_GET_NEXT_KNOMIAL_INIT(step_info, ptpcoll_module->pow_knum, my_group_index);
        while(MCA_COMMON_NETPATTERNS_GET_NEXT_KNOMIAL_PEER_CHECK_LEVEL(step_info)) {
            MCA_COMMON_NETPATTERNS_GET_NEXT_KNOMIAL_PEER(my_group_index, radix, step_info, peer);
            PTPCOLL_VERBOSE(10, ("Bcast, iprobe tag %d rank %d",
                        tag, group_list[peer]));
            MCA_PML_CALL(iprobe(group_list[peer], tag,
                        comm, &matched, &status));
            if (matched) {
                MCA_COMMON_NETPATTERNS_GET_NEXT_KNOMIAL_UPDATE_LEVEL_FOR_BCAST(step_info, radix);
                break;
            }
        }

        /* Check of the */
        if (PTPCOLL_KN_PROXY & ptpcoll_module->pow_ktype) {
            for (i = 0 ; i < ptpcoll_module->kn_proxy_extra_num; i++) {
                PTPCOLL_VERBOSE(10, ("Bcast, iprobe tag %d rank %d",
                            tag, group_list[peer]));
                MCA_PML_CALL(iprobe(group_list[ptpcoll_module->kn_proxy_extra_index[i]], tag,
                            comm, &matched, &status));
                if (matched) {
                    step_info.k_level = root_radix_mask;
                    extra_root = group_list[ptpcoll_module->kn_proxy_extra_index[i]];
                    goto ANY_ROOT_KNOMIAL_BCAST;
                }
            }
        }
    }

    /* the function always returns OMPI_SUCCESS, so we don't check return code */
    if (0 == matched) {
        PTPCOLL_VERBOSE(10, ("IPROBE was not matched"));
        /* No data was received, return no match error */
        return BCOL_FN_NOT_STARTED;
    }

    /* set the source of data */
    comm_root = status.MPI_SOURCE;

    PTPCOLL_VERBOSE(10, ("A. step info %d %d %d", step_info.k_level, step_info.k_step, step_info.k_tmp_peer));

    /* Bcast the data */
    PTPCOLL_VERBOSE(10, ("Starting data bcast"));

ANY_ROOT_KNOMIAL_BCAST:
    /* Post receive that will fetch the data */
    PTPCOLL_VERBOSE(10, ("Bcast, receive data from %d[%d], count %d, tag %d, addr %p",
                comm_root, count, tag, data_buffer));

    rc = MCA_PML_CALL(recv(data_buffer, count, MPI_BYTE, comm_root, tag, comm, MPI_STATUS_IGNORE));
    if( OMPI_SUCCESS != rc ) {
        PTPCOLL_VERBOSE(10, ("Failed to receive data"));
        return OMPI_ERROR;
    }
    PTPCOLL_VERBOSE(10, ("Bcast, Data was received"));

    /* Sending forward the data over K-nomial tree */
    MCA_COMMON_NETPATTERNS_GET_NEXT_KNOMIAL_INIT(step_info, step_info.k_level, my_group_index);

    PTPCOLL_VERBOSE(10, ("B. step info %d %d %d", step_info.k_level, step_info.k_step, step_info.k_tmp_peer));
    K_NOMIAL_ROOT_BCAST_NB_NOTEST(step_info, radix,
                        my_group_index, group_list,
                        data_buffer, count, tag, comm, send_requests,
                        active_requests);

ANY_ROOT_KNOMIAL_EXTRA:
    /* Proxy node but NOT virtual root */
    if (PTPCOLL_KN_PROXY & ptpcoll_module->pow_ktype) {
        for (i = 0 ; i < ptpcoll_module->kn_proxy_extra_num; i++) {
            if (ptpcoll_module->kn_proxy_extra_index[i] == extra_root)
                continue;

            PTPCOLL_VERBOSE(10, ("Extra_Isend to %d", ptpcoll_module->kn_proxy_extra_index[i]));
            rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,
                        group_list[ptpcoll_module->kn_proxy_extra_index[i]], tag - 1,
                        MCA_PML_BASE_SEND_STANDARD, comm,
                        &(send_requests[*active_requests])));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to send data"));
                return OMPI_ERROR;
            }
            ++(*active_requests);
        }
    }

    if (*active_requests > 0) {
        matched =
            mca_bcol_ptpcoll_test_all_for_match
            (active_requests, send_requests, &rc);
    }

    /* If it is last call, we have to recycle memory */
    if(matched) {
        PTPCOLL_VERBOSE(10, ("bcast root is done"));
        return BCOL_FN_COMPLETE;
    } else {
        PTPCOLL_VERBOSE(10, ("bcast root is started"));
        return BCOL_FN_STARTED;
    }
}

static int bcol_ptpcoll_bcast_k_nomial_extra_known_and_anyroot(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int tag;
    int rc;
    int i;
    int completed = 0; /* not completed */
    uint32_t buffer_index = input_args->buffer_index;

    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *iteration =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration);
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;
    ompi_status_public_t status;

    PTPCOLL_VERBOSE(3, ("Knomial Anyroot, index_this_type %d, num_of_this_type %d",
                const_args->index_of_this_type_in_collective + 1,
                const_args->n_of_this_type_in_collective));

    /* keep tag within the limit support by the pml */
    tag = (PTPCOLL_TAG_OFFSET + input_args->sequence_num * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;
    /* reset active requests */
    *active_requests = 0;
    /* reset iteration counter */
    *iteration = -1;

    PTPCOLL_VERBOSE(8, ("bcol_ptpcoll_bcast_k_nomial_anyroot extra, buffer index: %d \n"
                "tag: %d "
                "tag_mask: %d "
                "sn: %d "
                "root: %d "
                "pow_k: %d %d "
                "buff: %p "
                ,buffer_index, tag,
                ptpcoll_module->tag_mask, input_args->sequence_num,
                input_args->root_flag,
                ptpcoll_module->pow_k, ptpcoll_module->pow_knum,
                data_buffer
                ));

    /* we have a power 2 group */
    if (input_args->root_flag) {

        PTPCOLL_VERBOSE(10, ("I'm EXTRA root of the data, v root %d", ptpcoll_module->kn_proxy_extra_index[0]));
        /* send the all data to your proxy peer */
        rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,
                    group_list[ptpcoll_module->kn_proxy_extra_index[0]], tag,
                    MCA_PML_BASE_SEND_STANDARD, comm,
                    &(requests[*active_requests])));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to send data"));
            return OMPI_ERROR;
        }
        ++(*active_requests);

        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if (0 == completed) {
            /* we have to store the iteration number somewhere */
            PTPCOLL_VERBOSE(10, ("Extra was started"));
            return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
        }
    } else {
        for (i = 0; i < cm->num_to_probe &&
                0 == completed; i++) {
            MCA_PML_CALL(iprobe(group_list[ptpcoll_module->kn_proxy_extra_index[0]], tag - 1,
                        comm, &completed, &status));
        }
        if (0 == completed) {
            /* No data was received */
            return BCOL_FN_NOT_STARTED;
        }

        /* the data is ready */
        rc = MCA_PML_CALL(recv(data_buffer, count, MPI_BYTE,
                    group_list[ptpcoll_module->kn_proxy_extra_index[0]], tag - 1,
                    comm, MPI_STATUS_IGNORE));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to send data"));
            return OMPI_ERROR;
        }
    }

    PTPCOLL_VERBOSE(10, ("Extra was done"));
    return BCOL_FN_COMPLETE;
}

static int bcol_ptpcoll_bcast_k_nomial_extra_known_and_anyroot_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    int rc;
    int completed = 0; /* not completed */
    int i;
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[input_args->buffer_index].requests;
    uint32_t buffer_index = input_args->buffer_index;
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    ompi_status_public_t status;
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    /* keep tag within the limit support by the pml */
    int tag = -((PTPCOLL_TAG_OFFSET + input_args->sequence_num * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask));
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;

    PTPCOLL_VERBOSE(10, ("bcol_ptpcoll_bcast_k_nomial_extra_known_and_anyroot_progress extra, was called, tag %d\n", tag));
    if (input_args->root_flag) {
        PTPCOLL_VERBOSE(10, ("I'm EXTRA root of the data"));
        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if (0 == completed) {
            return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
        }
    } else {
        for (i = 0; i < cm->num_to_probe &&
                0 == completed; i++) {
            MCA_PML_CALL(iprobe(group_list[ptpcoll_module->kn_proxy_extra_index[0]], tag - 1,
                        comm, &completed, &status));
        }
        if (0 == completed) {
            return BCOL_FN_STARTED;
        }
        /* the data is ready */

        rc = MCA_PML_CALL(recv(data_buffer, count, MPI_BYTE,
                    group_list[ptpcoll_module->kn_proxy_extra_index[0]], tag - 1,
                    comm, MPI_STATUS_IGNORE));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to send data"));
            return OMPI_ERROR;
        }
    }

    /* Done */
    return BCOL_FN_COMPLETE;                                    \
}

/* Know root means that we know exactly the source of data and we do not have to check multiple
 * sources
 */

#define K_NOMIAL_DATA_SRC(radix, my_group_index, group_size, group_root, data_src, radix_mask)      \
    do {                                                                                            \
        int relative_rank = (my_group_index >= group_root) ? my_group_index - group_root :          \
            my_group_index - group_root + group_size;                                               \
                                                                                                    \
        radix_mask = 1;                                                                             \
        while (radix_mask < group_size) {                                                           \
            if (relative_rank % (radix * radix_mask)) {                                             \
                data_src = relative_rank/(radix * radix_mask) * (radix * radix_mask) + group_root;  \
                if (data_src >= group_size) data_src -= group_size;                                 \
                break;                                                                              \
            }                                                                                       \
            radix_mask *= radix;                                                                    \
        }                                                                                           \
    } while (0)


int bcol_ptpcoll_bcast_k_nomial_known_root_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int tag;
    int rc = OMPI_SUCCESS;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    int radix = ptpcoll_module->k_nomial_radix;
    int radix_mask;
    uint64_t sequence_number = input_args->sequence_num;
    uint32_t buffer_index = input_args->buffer_index;
    int group_root_index = 0;

    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **send_requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    ompi_request_t **recv_request =
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests[0];
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int completed = 0;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);

    tag = (PTPCOLL_TAG_OFFSET + sequence_number * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;

    PTPCOLL_VERBOSE(3, ("BCAST Know root, index_this_type %d, num_of_this_type %d",
                const_args->index_of_this_type_in_collective + 1,
                const_args->n_of_this_type_in_collective));

    PTPCOLL_VERBOSE(10, ("bcol_ptpcoll_bcast_k_nomial_known_root_progress, buffer index: %d \n"
                         "tag: %d "
                         "tag_mask: %d "
                         "sn: %d "
                         "root: %d "
                         "pow_k: %d %d "
                         "buff: %p "
                         "radix: %d",
                         buffer_index, tag,
                         ptpcoll_module->tag_mask, sequence_number,
                         input_args->root_flag,
                         ptpcoll_module->pow_k, ptpcoll_module->pow_knum,
                         data_buffer,
                         radix));

    if (input_args->root_flag) {
        /* Check for completion */
        assert(*active_requests > 0);
        PTPCOLL_VERBOSE(10, ("Requests %d", *active_requests));
        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, send_requests, &rc);
        if (OMPI_SUCCESS != rc) {
            return OMPI_ERROR;
        }
    } else {
        /* No data was received. Waiting for data */
        if (0 == (*active_requests)) {
            int extra_root = -1;
            netpatterns_knomial_step_info_t step_info;
            /* We can not block. So run couple of test for data arrival */
            if (0 == mca_bcol_ptpcoll_test_for_match(recv_request, &rc)) {
                PTPCOLL_VERBOSE(10, ("Test was not matched (active request %d)",
                            *active_requests));
                /* No data was received, return no match error */
                return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
            }

            radix_mask = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask;
            group_root_index = input_args->root_route->rank;

            PTPCOLL_VERBOSE(10, ("Test was matched - radix %d", radix_mask));
            /* Bcast the data */
            MCA_COMMON_NETPATTERNS_GET_NEXT_KNOMIAL_INIT(step_info,
                    radix_mask, my_group_index);
            K_NOMIAL_ROOT_BCAST_NB_NOTEST(step_info, radix,
                    my_group_index, group_list,
                    data_buffer, count, tag, comm, send_requests,
                    active_requests);

            if (PTPCOLL_KN_PROXY & ptpcoll_module->pow_ktype) {
                int i;
                if (radix_mask == ptpcoll_module->pow_knum) {
                    extra_root = group_root_index;
                }
                for (i = 0 ; i < ptpcoll_module->kn_proxy_extra_num; i++) {
                    if (ptpcoll_module->kn_proxy_extra_index[i] == extra_root)
                        continue;
                    PTPCOLL_VERBOSE(10, ("Extra_Isend to %d", ptpcoll_module->kn_proxy_extra_index[i]));
                    rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,
                                group_list[ptpcoll_module->kn_proxy_extra_index[i]], tag - 1,
                                MCA_PML_BASE_SEND_STANDARD, comm,
                                &(send_requests[*active_requests])));
                    if( OMPI_SUCCESS != rc ) {
                        PTPCOLL_VERBOSE(10, ("Failed to send data"));
                        return OMPI_ERROR;
                    }
                    ++(*active_requests);
                }
            }
            if (*active_requests > 0) {
                completed = mca_bcol_ptpcoll_test_all_for_match
                    (active_requests, send_requests, &rc);
            } else {
                completed = 1;
            }
        } else {
         /* Data was received and sent out, check for completion */
            completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, send_requests, &rc);
            if (OMPI_SUCCESS != rc) {
                PTPCOLL_VERBOSE(10, ("Test was not matched (active request %d)",
                            *active_requests));
                return OMPI_ERROR;
            }
        }
    }
    /* DONE */
    if(completed) {
        return BCOL_FN_COMPLETE;
    } else {
        PTPCOLL_VERBOSE(10, ("bcast root is started"));
        return BCOL_FN_STARTED;
    }
}

int bcol_ptpcoll_bcast_k_nomial_known_root(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int tag;
    int rc;
    int comm_root;
    int data_src = -1;
    int group_root_index;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    int radix = ptpcoll_module->k_nomial_radix;
    uint32_t buffer_index = input_args->buffer_index;

    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **send_requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    ompi_request_t **recv_request =
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests[0];
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int matched = 0;
    int k_level, logk_level;
    int extra_root = -1;
    netpatterns_knomial_step_info_t step_info;

    PTPCOLL_VERBOSE(3, ("BCAST Know root, index_this_type %d, num_of_this_type %d",
                    const_args->index_of_this_type_in_collective + 1,
                    const_args->n_of_this_type_in_collective));

    /* reset active request counter */
    (*active_requests) = 0;
    /* keep tag within the limit support by the pml */
    tag = (PTPCOLL_TAG_OFFSET + input_args->sequence_num * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;

    PTPCOLL_VERBOSE(8, ("bcol_ptpcoll_bcast_k_nomial_known_root, buffer index: %d \n"
                         "tag: %d "
                         "tag_mask: %d "
                         "sn: %d "
                         "root: %d "
                         "pow_k: %d %d "
                         "buff: %p "
                         "radix: %d",
                         buffer_index, tag,
                         ptpcoll_module->tag_mask, input_args->sequence_num,
                         input_args->root_flag,
                         ptpcoll_module->pow_k, ptpcoll_module->pow_knum,
                         data_buffer,
                         radix));

    if (input_args->root_flag) {
        PTPCOLL_VERBOSE(10, ("I'm root of the data"));
        /*
         * I'm root of the operation
         * send data to (k - 1) * log base k N neighbors
         */
        MCA_COMMON_NETPATTERNS_GET_NEXT_KNOMIAL_INIT(step_info,
                ptpcoll_module->pow_knum, my_group_index);
        K_NOMIAL_ROOT_BCAST_NB_NOTEST(step_info, radix,
                my_group_index, group_list,
                data_buffer, count, tag, comm, send_requests,
                active_requests);
        goto KNOWN_ROOT_KNOMIAL_BCAST_EXTRA;
    }

    /* I'm not root */
    group_root_index = input_args->root_route->rank;

    /* If Proxy node,  check if extra node is root */
    PTPCOLL_VERBOSE(10, ("Check if I virtual root, groop root %d group_size_pow %d type %d\n",
                group_root_index, ptpcoll_module->pow_knum , ptpcoll_module->pow_ktype));
    if (group_root_index >= ptpcoll_module->pow_knum) {
        /* Chech if the rank is virtual root */
        int virtual_root = (group_root_index -
                ptpcoll_module->pow_knum) / (radix - 1);

        if (my_group_index == virtual_root) {
                MCA_COMMON_NETPATTERNS_GET_NEXT_KNOMIAL_INIT(step_info,
                        ptpcoll_module->pow_knum, my_group_index);
                k_level = ptpcoll_module->pow_knum;
                comm_root = group_list[group_root_index];
                extra_root = group_root_index;
                PTPCOLL_VERBOSE(10, ("Im virtual root klevel %d, comm_root %d  vroot %d\n",
                            k_level, comm_root, virtual_root));
                goto KNOWN_ROOT_KNOMIAL_BCAST;
        } else {
            /* set virtual root as real root of the group */
            group_root_index = virtual_root;
            PTPCOLL_VERBOSE(10, ("My virtual root vroot %d\n", group_root_index));
        }
    }

    data_src = netpatterns_get_knomial_data_source(
                my_group_index, group_root_index, radix, ptpcoll_module->pow_knum,
                &k_level, &logk_level);

    comm_root = group_list[data_src];

KNOWN_ROOT_KNOMIAL_BCAST:
    PTPCOLL_VERBOSE(10, ("Bcast, receive data from %d[%d], count %d, tag %d, addr %p",
                comm_root, data_src, count, tag, data_buffer));

    rc = MCA_PML_CALL(irecv(data_buffer, count, MPI_BYTE, comm_root, tag, comm, recv_request));
    if( OMPI_SUCCESS != rc ) {
        PTPCOLL_VERBOSE(10, ("Failed to receive data"));
        return OMPI_ERROR;
    }

    /* We can not block. So run couple of test for data arrival */
    if (0 == mca_bcol_ptpcoll_test_for_match(recv_request, &rc)) {
        PTPCOLL_VERBOSE(10, ("Test was not matched - %d", rc));
        /* cache the radix mask for future progress */
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask = k_level;
        /* No data was received, return no match error */
        return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
    }

    /* Bcast the data */
    MCA_COMMON_NETPATTERNS_GET_NEXT_KNOMIAL_INIT(step_info,
            k_level, my_group_index);

    K_NOMIAL_ROOT_BCAST_NB_NOTEST(step_info, radix,
            my_group_index, group_list,
            data_buffer, count, tag, comm, send_requests,
            active_requests);

KNOWN_ROOT_KNOMIAL_BCAST_EXTRA:
    /* Proxy node but NOT virtual root */
    if (PTPCOLL_KN_PROXY & ptpcoll_module->pow_ktype) {
        int i;
        for (i = 0 ; i < ptpcoll_module->kn_proxy_extra_num; i++) {
            if (ptpcoll_module->kn_proxy_extra_index[i] == extra_root)
                continue;

            PTPCOLL_VERBOSE(10, ("Extra_Isend to %d", ptpcoll_module->kn_proxy_extra_index[i]));
            rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,
                        group_list[ptpcoll_module->kn_proxy_extra_index[i]], tag - 1,
                        MCA_PML_BASE_SEND_STANDARD, comm,
                        &(send_requests[*active_requests])));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to send data"));
                return OMPI_ERROR;
            }
            ++(*active_requests);
        }
    }

    if (*active_requests > 0) {
        matched =
            mca_bcol_ptpcoll_test_all_for_match
            (active_requests, send_requests, &rc);
    } else {
        matched = 1;
    }

    /* If it is last call, we have to recycle memory */
    if(matched) {
        return BCOL_FN_COMPLETE;
    } else {
        PTPCOLL_VERBOSE(10, ("bcast root is started"));
        return BCOL_FN_STARTED;
    }
}

int bcol_ptpcoll_bcast_binomial_scatter_gatther_anyroot_extra(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int tag;
    int rc;
    int i;
    int completed = 0; /* not completed */
    uint32_t buffer_index = input_args->buffer_index;

    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *iteration =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration);
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;
    ompi_status_public_t status;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;

    PTPCOLL_VERBOSE(3, ("BCAST Anyroot, index_this_type %d, num_of_this_type %d",
                const_args->index_of_this_type_in_collective + 1,
                const_args->n_of_this_type_in_collective));

    /* keep tag within the limit support by the pml */
    tag = (PTPCOLL_TAG_OFFSET + input_args->sequence_num * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;
    /* reset active requests */
    *active_requests = 0;
    /* reset iteration counter */
    *iteration = -1;

    PTPCOLL_VERBOSE(8, ("bcol_ptpcoll_bcast_k_nomial_anyroot extra, buffer index: %d \n"
                "tag: %d "
                "tag_mask: %d "
                "sn: %d "
                "root: %d "
                "pow_k: %d %d "
                "buff: %p "
                "radix: %d" ,
                buffer_index, tag,
                ptpcoll_module->tag_mask, input_args->sequence_num,
                input_args->root_flag,
                ptpcoll_module->pow_k, ptpcoll_module->pow_knum,
                data_buffer,
                2
                ));

    /* we have a power 2 group */
    if (input_args->root_flag) {

        PTPCOLL_VERBOSE(10, ("I'm EXTRA root of the data"));
        /* send the all data to your proxy peer */
        rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,
                    group_list[ptpcoll_module->proxy_extra_index], tag,
                    MCA_PML_BASE_SEND_STANDARD, comm,
                    &(requests[*active_requests])));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to send data"));
            return OMPI_ERROR;
        }
        ++(*active_requests);

        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if (0 == completed) {
            /* we have to store the iteration number somewhere */
            return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
        }
    } else {
        for (i = 0; i < cm->num_to_probe &&
                0 == completed; i++) {
            MCA_PML_CALL(iprobe(group_list[ptpcoll_module->proxy_extra_index], tag - 1,
                        comm, &completed, &status));
        }
        if (0 == completed) {
            /* No data was received */
            return BCOL_FN_NOT_STARTED;
        }

        /* the data is ready */
        rc = MCA_PML_CALL(recv(data_buffer, count, MPI_BYTE,
                    group_list[ptpcoll_module->proxy_extra_index], tag - 1,
                    comm, MPI_STATUS_IGNORE));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to send data"));
            return OMPI_ERROR;
        }
    }

    return BCOL_FN_COMPLETE;
}

int bcol_ptpcoll_bcast_binomial_scatter_gatther_anyroot_extra_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    int rc;
    int completed = 0; /* not completed */
    int i;
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[input_args->buffer_index].requests;
    uint32_t buffer_index = input_args->buffer_index;
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    ompi_status_public_t status;
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    /* keep tag within the limit support by the pml */
    int tag = -((PTPCOLL_TAG_OFFSET + input_args->sequence_num * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask));
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;

    PTPCOLL_VERBOSE(10, ("bcol_ptpcoll_bcast_k_nomial_extra_known_and_anyroot_progress extra, was called, tag %d\n", tag));
    if (input_args->root_flag) {
        PTPCOLL_VERBOSE(10, ("I'm EXTRA root of the data"));
        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if (0 == completed) {
            return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
        }
    } else {
        for (i = 0; i < cm->num_to_probe &&
                0 == completed; i++) {
            MCA_PML_CALL(iprobe(group_list[ptpcoll_module->proxy_extra_index], tag - 1,
                        comm, &completed, &status));
        }
        if (0 == completed) {
            return BCOL_FN_STARTED;
        }
        /* the data is ready */

        rc = MCA_PML_CALL(recv(data_buffer, count, MPI_BYTE,
                    group_list[ptpcoll_module->proxy_extra_index], tag - 1,
                    comm, MPI_STATUS_IGNORE));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to send data"));
            return OMPI_ERROR;
        }
    }

    /* Done */
    return BCOL_FN_COMPLETE;
}

int bcol_ptpcoll_bcast_binomial_scatter_gatther_anyroot_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int rc;
    int completed = 0; /* not completed */
    uint32_t buffer_index = input_args->buffer_index;

    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *iteration =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration);
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    size_t base_block_size = (count +  ptpcoll_module->pow_2num - 1) /
        ptpcoll_module->pow_2num;
    int tag = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag;
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    int *status =
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].status;

    PTPCOLL_VERBOSE(10, ("bcol_ptpcoll_bcast_binomial_scatter_gatther_anyroot_progress, buffer index: %d \n"
                         "tag: %d "
                         "tag_mask: %d "
                         "sn: %d "
                         "root: %d "
                         "pow_2: %d %d "
                         "buff: %p "
                         "radix: %d"
                         "block_size: %d",
                         buffer_index, tag,
                         ptpcoll_module->tag_mask, 0,
                         input_args->root_flag,
                         ptpcoll_module->pow_2, ptpcoll_module->pow_2num,
                         data_buffer,
                         2,
                         base_block_size));

    switch(*status) {
        case PTPCOLL_GATHER_STARTED:
            completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
            if (0 == completed) {
                PTPCOLL_VERBOSE(10, ("Not done, have to complete %d, Return %d", *active_requests, rc));
                return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
            }
            ++(*iteration); /* start from next iteration */
            PTPCOLL_VERBOSE(10, ("Outstanding operation was comleted, starting next one ! %d", *iteration));
            break;
        case PTPCOLL_EXTRA_SEND_STARTED:
            completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
            if (0 == completed) {
                PTPCOLL_VERBOSE(10, ("Not done, have to complete %d, Return %d", *active_requests, rc));
                return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
            }
            return BCOL_FN_COMPLETE;
        default:
            PTPCOLL_VERBOSE(10, ("Unknown status %d", *status));
            return OMPI_ERROR;
    }

    PTPCOLL_VERBOSE(10, ("Stating PR_GATHER"));
    /* Gather, continue the recoursive doubling iterations */
    rc = bcol_ptpcoll_bcast_binomial_gather_anyroot(ptpcoll_module, buffer_index, data_buffer,
            count, base_block_size);
    if (BCOL_FN_COMPLETE != rc) {
        assert(0 != *active_requests);
        PTPCOLL_VERBOSE(10, ("Not done. Return %d", rc));
        return rc;
    }
    PTPCOLL_VERBOSE(10, ("PR_GATHER done"));

    /* it the process is proxy , it has to send full
       message to remote peer */
    if ((PTPCOLL_PROXY & ptpcoll_module->pow_2type) &&
            ! CHECK_IF_ROOT_OR_VROOT(ptpcoll_module, buffer_index)) {
        *status = PTPCOLL_EXTRA_SEND_STARTED;
        rc = bcol_ptpcoll_bcast_binomial_scatter_gatther_send_extra(
                ptpcoll_module,
                data_buffer, count, tag - 1,
                ptpcoll_module->proxy_extra_index, comm,
                active_requests, requests);
        if (BCOL_FN_COMPLETE != rc) {
            return rc;
        }
    }
    /* return */
    return BCOL_FN_COMPLETE;
}

int bcol_ptpcoll_bcast_binomial_scatter_gatther_anyroot(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int tag;
    int rc;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    uint64_t sequence_number = input_args->sequence_num;
    uint32_t buffer_index = input_args->buffer_index;

    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *iteration =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration);
    int *radix_mask_pow =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask_pow);
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    size_t base_block_size = (count +  ptpcoll_module->pow_2num - 1) /
        ptpcoll_module->pow_2num;
    int root_pow2 = ptpcoll_module->pow_2 - 1;
    int *status =
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].status;

    PTPCOLL_VERBOSE(3, ("BCAST Anyroot, index_this_type %d, num_of_this_type %d",
                    const_args->index_of_this_type_in_collective + 1,
                    const_args->n_of_this_type_in_collective));

    /* keep tag within the limit support by the pml */
    tag = (PTPCOLL_TAG_OFFSET + sequence_number * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag = tag = -tag;
    /* reset active requests */
    *active_requests = 0;
    /* reset iteration counter */
    *iteration = -1;
    /* set initial status */
    *status = PTPCOLL_NOT_STARTED;

    PTPCOLL_VERBOSE(8, ("bcol_ptpcoll_bcast_k_nomial_anyroot, buffer index: %d \n"
                         "tag: %d "
                         "tag_mask: %d "
                         "sn: %d "
                         "root: %d "
                         "pow_2: %d %d "
                         "buff: %p "
                         "radix: %d"
                         "block_size: %d",
                         buffer_index, tag,
                         ptpcoll_module->tag_mask, sequence_number,
                         input_args->root_flag,
                         ptpcoll_module->pow_2, ptpcoll_module->pow_2num,
                         data_buffer,
                         2,
                         base_block_size));

    /* we have a power 2 group */
    if (input_args->root_flag) {

        PTPCOLL_VERBOSE(10, ("I'm root of the data"));
        /* for proxy we have little bit more work to do */
        if (PTPCOLL_PROXY & ptpcoll_module->pow_2type) {
            /* send the all data to your extra peer */
            rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,
                        group_list[ptpcoll_module->proxy_extra_index],
                        tag - 1,
                        MCA_PML_BASE_SEND_STANDARD, comm,
                        &(requests[*active_requests])));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to send data"));
                return OMPI_ERROR;
            }
            ++(*active_requests);
        }
        /*
         * I'm root of the operation
         * send data to (k - 1) * log base k N neighbors
         */
        *radix_mask_pow = ptpcoll_module->pow_2;

        K_NOMIAL_ROOT_BCAST_NB_BINOMIAL_SCATTER(root_pow2,
                my_group_index, group_size, group_list,
                data_buffer, base_block_size, count, tag, comm, requests,
                active_requests);

        goto GATHER;
    }

    /* <-- non root flow --> */
    rc = bcol_ptpcoll_bcast_binomial_probe_and_scatter_anyroot(ptpcoll_module, buffer_index,
            data_buffer, count, base_block_size);
    if (BCOL_FN_COMPLETE != rc) {
        PTPCOLL_VERBOSE(10, ("Not done. Return %d", rc));
        return rc;
    }

GATHER:
    *iteration = 0;
    *status = PTPCOLL_GATHER_STARTED;
    rc = bcol_ptpcoll_bcast_binomial_gather_anyroot(ptpcoll_module, buffer_index,
            data_buffer, count, base_block_size);

    if (BCOL_FN_COMPLETE != rc) {
        assert(0 != *active_requests);
        PTPCOLL_VERBOSE(10, ("Not done. Return %d", rc));
        return rc;
    }

    ++(*iteration); /* I need it for progress */

    /* proxy case */
    if ((PTPCOLL_PROXY & ptpcoll_module->pow_2type) &&
            ! CHECK_IF_ROOT_OR_VROOT(ptpcoll_module, buffer_index)) {
        *status = PTPCOLL_EXTRA_SEND_STARTED;
        rc = bcol_ptpcoll_bcast_binomial_scatter_gatther_send_extra(ptpcoll_module,
                data_buffer, count, tag - 1,
                ptpcoll_module->proxy_extra_index, comm,
                active_requests, requests);
        if (BCOL_FN_COMPLETE != rc) {
            return rc;
        }
    }

    return BCOL_FN_COMPLETE;
}

int bcol_ptpcoll_bcast_binomial_scatter_gatther_known_root_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int rc;
    int completed = 0; /* not completed */
    uint32_t buffer_index = input_args->buffer_index;

    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *iteration =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration);
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    size_t base_block_size = (count +  ptpcoll_module->pow_2num - 1) /
        ptpcoll_module->pow_2num;
    int tag = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag;
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    int *status =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].status);

    PTPCOLL_VERBOSE(10, ("bcol_ptpcoll_bcast_binomial_scatter_gatther_known_progress, buffer index: %d \n"
                "tag: %d "
                "tag_mask: %d "
                "sn: %d "
                "root: %d "
                "pow_2: %d %d "
                "buff: %p "
                "radix: %d"
                "block_size: %d",
                buffer_index, tag,
                ptpcoll_module->tag_mask, 0,
                input_args->root_flag,
                ptpcoll_module->pow_2, ptpcoll_module->pow_2num,
                data_buffer,
                2,
                base_block_size));

    switch(*status) {
        case PTPCOLL_WAITING_FOR_DATA:
            PTPCOLL_VERBOSE(10, ("Probe for the data"));
            rc = bcol_ptpcoll_bcast_binomial_test_and_scatter_known_root(ptpcoll_module, buffer_index,
                    data_buffer, count, base_block_size);
            if (BCOL_FN_COMPLETE != rc) {
                assert(0 != *active_requests);
                PTPCOLL_VERBOSE(10, ("Not done. Return %d", rc));
                return rc;
            }
            *iteration = 0;
            *status = PTPCOLL_GATHER_STARTED;
            break;
        case PTPCOLL_GATHER_STARTED:
            completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
            if (0 == completed) {
                PTPCOLL_VERBOSE(10, ("Not done, have to complete %d, Return %d", *active_requests, rc));
                return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
            }
            ++(*iteration); /* start from next iteration */
            PTPCOLL_VERBOSE(10, ("Outstanding operation was comleted, starting next one ! %d", *iteration));
            break;
        case PTPCOLL_EXTRA_SEND_STARTED:
            completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
            if (0 == completed) {
                PTPCOLL_VERBOSE(10, ("Not done, have to complete %d, Return %d", *active_requests, rc));
                return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
            }
            return BCOL_FN_COMPLETE;
        default:
            PTPCOLL_VERBOSE(10, ("Unknown status %d", *status));
            return OMPI_ERROR;
    }

    PTPCOLL_VERBOSE(10, ("Stating PR_GATHER"));
    /* Gather, continue the recoursive doubling iterations */
    rc = bcol_ptpcoll_bcast_binomial_gather_anyroot(ptpcoll_module, buffer_index, data_buffer,
            count, base_block_size);
    if (BCOL_FN_COMPLETE != rc) {
        assert(0 != *active_requests);
        PTPCOLL_VERBOSE(10, ("Not done. Return %d", rc));
        return rc;
    }
    PTPCOLL_VERBOSE(10, ("PR_GATHER done"));

    /* it the process is proxy , it has to send full
       message to remote peer */
    if ((PTPCOLL_PROXY & ptpcoll_module->pow_2type) &&
            ! CHECK_IF_ROOT_OR_VROOT(ptpcoll_module, buffer_index)) {
        *status = PTPCOLL_EXTRA_SEND_STARTED;
        rc = bcol_ptpcoll_bcast_binomial_scatter_gatther_send_extra(
                ptpcoll_module,
                data_buffer, count, tag - 1,
                ptpcoll_module->proxy_extra_index, comm,
                active_requests, requests);
        if (BCOL_FN_COMPLETE != rc) {
            return rc;
        }
    }

    /* return */
    return BCOL_FN_COMPLETE;
}

int bcol_ptpcoll_bcast_binomial_scatter_gatther_known_root(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int tag;
    int rc;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int group_src, comm_root;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    int pow2_distance;
    void *curr_data_buffer;
    int recv_count;
    uint64_t sequence_number = input_args->sequence_num;
    uint32_t buffer_index = input_args->buffer_index;

    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *iteration =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration);
    int *radix_mask_pow =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask_pow);
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    size_t base_block_size = (count +  ptpcoll_module->pow_2num - 1) /
        ptpcoll_module->pow_2num;
    int root_pow2 = ptpcoll_module->pow_2 - 1;
    int *status =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].status);

    PTPCOLL_VERBOSE(3, ("BCAST Anyroot, index_this_type %d, num_of_this_type %d",
                    const_args->index_of_this_type_in_collective + 1,
                    const_args->n_of_this_type_in_collective));

    /* keep tag within the limit support by the pml */
    tag = (PTPCOLL_TAG_OFFSET + sequence_number * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag = tag = -tag;
    /* reset active requests */
    *active_requests = 0;
    /* reset iteration counter */
    *iteration = -1;
    /* set initial status */
    *status = PTPCOLL_NOT_STARTED;

    PTPCOLL_VERBOSE(8, ("bcol_ptpcoll_bcast_binomial_scatter_gatther_known, buffer index: %d \n"
                         "tag: %d "
                         "tag_mask: %d "
                         "sn: %d "
                         "root: %d "
                         "pow_2: %d %d "
                         "buff: %p "
                         "radix: %d"
                         "block_size: %d",
                         buffer_index, tag,
                         ptpcoll_module->tag_mask, sequence_number,
                         input_args->root_flag,
                         ptpcoll_module->pow_2, ptpcoll_module->pow_2num,
                         data_buffer,
                         2,
                         base_block_size));

    /* we have a power 2 group */
    if (input_args->root_flag) {

        PTPCOLL_VERBOSE(10, ("I'm root of the data"));
        /* for proxy we have little bit more work to do */
        if (PTPCOLL_PROXY & ptpcoll_module->pow_2type) {
            /* send the all data to your extra peer */
            rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,
                        group_list[ptpcoll_module->proxy_extra_index], tag - 1,
                        MCA_PML_BASE_SEND_STANDARD, comm,
                        &(requests[*active_requests])));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to send data"));
                return OMPI_ERROR;
            }
            *active_requests = 1;
        }
        /*
         * I'm root of the operation
         * send data to (k - 1) * log base k N neighbors
         */
        K_NOMIAL_ROOT_BCAST_NB_BINOMIAL_SCATTER(root_pow2,
                my_group_index, group_size, group_list,
                data_buffer, base_block_size, count, tag, comm, requests,
                active_requests);

        /* EXIT OR GO TO Gather */
        *iteration = 0;
        *radix_mask_pow = ptpcoll_module->pow_2;
        goto GATHER;
    }

    /* <-- non root flow --> */
    /* prapare and post recv operation */
    group_src = bcol_ptpcoll_binomial_root_to_src(input_args->root_route->rank,
            my_group_index, ptpcoll_module->pow_2num,
            ptpcoll_module->group_size, &pow2_distance);

    assert(group_src >= 0);

    if (0 > pow2_distance) {
        /* the rank is virtual root for this group, receive the data
           and scatter gather as root */
        PTPCOLL_VERBOSE(10, ("Virtual root %d , set mask to %d", my_group_index, ptpcoll_module->pow_2));
        *radix_mask_pow = ptpcoll_module->pow_2;
        curr_data_buffer = data_buffer;
        recv_count = count;
    } else {
        int my_left_boundary_rank;
        recv_count = base_block_size * (1 << pow2_distance); /* we may receive larger data */
        my_left_boundary_rank = my_group_index & ((~(int)0) << pow2_distance );
        curr_data_buffer = (void *)((unsigned char *)data_buffer +
                (size_t) base_block_size * my_left_boundary_rank);
        *radix_mask_pow = pow2_distance;
    }

    comm_root = group_list[group_src];

    PTPCOLL_VERBOSE(10, ("Bcast, receive data from %d[%d], count %d, tag %d, addr %p",
                comm_root, group_src, count, tag, data_buffer));

    rc = MCA_PML_CALL(irecv(curr_data_buffer, recv_count, MPI_BYTE, comm_root,
                tag, comm, &requests[*active_requests]));
    if( OMPI_SUCCESS != rc ) {
        PTPCOLL_VERBOSE(10, ("Failed to receive data"));
        return OMPI_ERROR;
    }

    ++(*active_requests);

    *status = PTPCOLL_WAITING_FOR_DATA;
    rc = bcol_ptpcoll_bcast_binomial_test_and_scatter_known_root(ptpcoll_module,
            buffer_index, data_buffer, count, base_block_size);

    if (BCOL_FN_COMPLETE != rc) {
        PTPCOLL_VERBOSE(10, ("Not done. Return %d", rc));
        return rc;
    }

    /* recv operation is done */

    *iteration = 0;

GATHER:

    *status = PTPCOLL_GATHER_STARTED;
    rc = bcol_ptpcoll_bcast_binomial_gather_anyroot(ptpcoll_module, buffer_index,
            data_buffer, count, base_block_size);

    if (BCOL_FN_COMPLETE != rc) {
        assert(0 != *active_requests);
        PTPCOLL_VERBOSE(10, ("Not done. Return %d", rc));
        return rc;
    }

    ++(*iteration); /* I need it for progress */

    /* proxy case */
    if ((PTPCOLL_PROXY & ptpcoll_module->pow_2type) &&
            ! CHECK_IF_ROOT_OR_VROOT(ptpcoll_module, buffer_index)) {
        *status = PTPCOLL_EXTRA_SEND_STARTED;
        rc = bcol_ptpcoll_bcast_binomial_scatter_gatther_send_extra(
                ptpcoll_module,
                data_buffer, count, tag - 1,
                ptpcoll_module->proxy_extra_index, comm,
                active_requests, requests);
        if (BCOL_FN_COMPLETE != rc) {
            return rc;
        }
    }

    return BCOL_FN_COMPLETE;
}

int bcol_ptpcoll_bcast_binomial_scatter_gatther_known_root_extra(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int tag;
    int rc;
    int completed = 0; /* not completed */
    uint32_t buffer_index = input_args->buffer_index;

    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *iteration =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration);
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;

    PTPCOLL_VERBOSE(3, ("BCAST known root, index_this_type %d, num_of_this_type %d",
                const_args->index_of_this_type_in_collective + 1,
                const_args->n_of_this_type_in_collective));

    /* keep tag within the limit support by the pml */
    tag = (PTPCOLL_TAG_OFFSET + input_args->sequence_num * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;
    /* reset active requests */
    *active_requests = 0;
    /* reset iteration counter */
    *iteration = -1;

    PTPCOLL_VERBOSE(8, ("bcol_ptpcoll_bcast_k_nomial_anyroot extra, buffer index: %d \n"
                "tag: %d "
                "tag_mask: %d "
                "sn: %d "
                "root: %d "
                "pow_k: %d %d "
                "buff: %p "
                "radix: %d" ,
                buffer_index, tag,
                ptpcoll_module->tag_mask, input_args->sequence_num,
                input_args->root_flag,
                ptpcoll_module->pow_k, ptpcoll_module->pow_knum,
                data_buffer,
                2
                ));

    /* we have a power 2 group */
    if (input_args->root_flag) {
        PTPCOLL_VERBOSE(10, ("I'm EXTRA root of the data"));
        /* send the all data to your proxy peer */
        rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,
                    group_list[ptpcoll_module->proxy_extra_index], tag,
                    MCA_PML_BASE_SEND_STANDARD, comm,
                    &(requests[*active_requests])));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to send data"));
            return OMPI_ERROR;
        }
        ++(*active_requests);

        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if (0 == completed) {
            /* we have to store the iteration number somewhere */
            return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
        }
    } else {
        rc = MCA_PML_CALL(irecv(data_buffer, count, MPI_BYTE,
                    group_list[ptpcoll_module->proxy_extra_index],
                    tag - 1, comm, &requests[*active_requests]));
        ++(*active_requests);

        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if (0 == completed) {
            PTPCOLL_VERBOSE(10, ("Test was not matched - %d", rc));
            return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
        }
    }

    return BCOL_FN_COMPLETE;
}

int bcol_ptpcoll_bcast_binomial_scatter_gatther_known_root_extra_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    int rc;
    int completed = 0; /* not completed */
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[input_args->buffer_index].requests;
    uint32_t buffer_index = input_args->buffer_index;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);

    PTPCOLL_VERBOSE(10, ("bcol_ptpcoll_bcast_binomial_known_root_extra_progress extra, was called\n"));

    completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
    if (0 == completed) {
        return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
    }

    return BCOL_FN_COMPLETE;
}

static int bcol_ptpcoll_bcast_narray_knomial_scatter_gatther_known_root_progress(
        bcol_function_args_t *input_args, struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int rc;
    int completed = 0; /* not completed */
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    uint32_t buffer_index = input_args->buffer_index;

    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *iteration =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration);
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int tag = ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag;
    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    int *status =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].status);
    int relative_group_index,
        group_root_index = 0;
    int group_size = ptpcoll_module->full_narray_tree_size;

    PTPCOLL_VERBOSE(8, ("bcol_ptpcoll_bcast_narray_knomial_scatter_gatther_known_root_progress, buffer index: %d "
                         "tag: %d "
                         "tag_mask: %d "
                         "root: %d "
                         "buff: %p "
                         "radix: %d"
                         , buffer_index, tag,
                         ptpcoll_module->tag_mask,
                         input_args->root_flag,
                         data_buffer,
                         ptpcoll_module->narray_knomial_proxy_num
                         ));

    if (input_args->root_flag ||
            /* virtual root case */
            (input_args->root_route->rank >= group_size &&
             my_group_index == (input_args->root_route->rank - group_size) /
             mca_bcol_ptpcoll_component.narray_knomial_radix)) {
        relative_group_index = 0;
        group_root_index = my_group_index;
    } else {
        if (input_args->root_route->rank >= group_size) {
            group_root_index = (input_args->root_route->rank - group_size) /
                mca_bcol_ptpcoll_component.narray_knomial_radix;
        } else {
            group_root_index = input_args->root_route->rank;
        }
        relative_group_index = my_group_index - group_root_index;
        if (relative_group_index < 0) {
            relative_group_index += group_size;
        }
    }

    switch(*status) {
        case PTPCOLL_WAITING_FOR_DATA:
            PTPCOLL_VERBOSE(10, ("Probe for the data"));
            rc = bcol_ptpcoll_bcast_narray_test_and_scatter_known_root(ptpcoll_module,
                    buffer_index, data_buffer, count, group_root_index,
                    relative_group_index);

            if (BCOL_FN_COMPLETE != rc) {
                assert(0 != *active_requests);
                PTPCOLL_VERBOSE(10, ("Not done. Return %d", rc));
                return rc;
            }
            *iteration = 0;
            *status = PTPCOLL_GATHER_STARTED;
            break;
        case PTPCOLL_ROOT_SEND_STARTED:
        case PTPCOLL_GATHER_STARTED:
            completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
            if (0 == completed) {
                PTPCOLL_VERBOSE(10, ("Not done, have to complete %d, Return %d", *active_requests, rc));
                return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
            }
            ++(*iteration); /* start from next iteration */
            PTPCOLL_VERBOSE(10, ("Outstanding operation was comleted, starting next one ! %d", *iteration));
            break;
        case PTPCOLL_EXTRA_SEND_STARTED:
            completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
            if (0 == completed) {
                PTPCOLL_VERBOSE(10, ("Not done, have to complete %d, Return %d", *active_requests, rc));
                return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
            }
            return BCOL_FN_COMPLETE;
        default:
            PTPCOLL_VERBOSE(10, ("Unknown status %d", *status));
            return OMPI_ERROR;
    }

    PTPCOLL_VERBOSE(10, ("Stating PR_GATHER"));
    /* Gather, continue the recoursive doubling iterations */
    rc = bcol_ptpcoll_bcast_narray_knomial_gather(ptpcoll_module,
            buffer_index, data_buffer, count,
            relative_group_index);
    if (BCOL_FN_COMPLETE != rc) {
        assert(0 != *active_requests);
        PTPCOLL_VERBOSE(10, ("Not done. Return %d", rc));
        return rc;
    }
    PTPCOLL_VERBOSE(10, ("PR_GATHER done"));

    /* it the process is proxy , it has to send full
       message to remote peer */
    if ((PTPCOLL_PROXY & ptpcoll_module->narray_type) &&
            !input_args->root_flag) {
        *status = PTPCOLL_EXTRA_SEND_STARTED;
        rc =  bcol_ptpcoll_send_n_extra(
                ptpcoll_module,
                data_buffer, count, tag - 1,
                ptpcoll_module->narray_knomial_proxy_extra_index,
                ptpcoll_module->narray_knomial_proxy_num,
                input_args->root_route->rank,
                comm, active_requests, requests);
        if (BCOL_FN_COMPLETE != rc) {
            return rc;
        }
    }

    /* return */
    return BCOL_FN_COMPLETE;
}


static int bcol_ptpcoll_bcast_narray_knomial_scatter_gatther_known_root(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int tag, rc, i;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int data_src, offset,
        comm_root;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    void *curr_data_buffer;
    int recv_count;
    uint64_t sequence_number = input_args->sequence_num;
    uint32_t buffer_index = input_args->buffer_index;

    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *iteration =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration);
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    size_t base_block_size = 0;
    int *status =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].status);
    int relative_group_index,
        group_root_index;
    int group_size = ptpcoll_module->full_narray_tree_size;
    int completed = 0;
    int virtual_root;
    netpatterns_narray_knomial_tree_node_t *narray_knomial_node = NULL;
    netpatterns_narray_knomial_tree_node_t *narray_node = NULL;

    PTPCOLL_VERBOSE(3, ("BCAST Anyroot, index_this_type %d, num_of_this_type %d",
                    const_args->index_of_this_type_in_collective + 1,
                    const_args->n_of_this_type_in_collective));

    /* keep tag within the limit support by the pml */
    tag = (PTPCOLL_TAG_OFFSET + sequence_number * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].tag = tag = -tag;
    /* reset radix mask, it used to keep last block size */
    ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].radix_mask = 1;
    /* reset active requests */
    *active_requests = 0;
    /* reset iteration counter */
    *iteration = -1;
    /* set initial status */
    *status = PTPCOLL_NOT_STARTED;

    PTPCOLL_VERBOSE(8, ("bcol_ptpcoll_bcast_narray_knomial_scatter_gatther_known_root, buffer index: %d "
                         "tag: %d "
                         "tag_mask: %d "
                         "sn: %d "
                         "root: %d "
                         "buff: %p "
                         "radix: %d"
                         ,buffer_index, tag,
                         ptpcoll_module->tag_mask, sequence_number,
                         input_args->root_flag,
                         data_buffer,
                         ptpcoll_module->narray_knomial_proxy_num
                         ));

    /* we have a power 2 group */
    if (input_args->root_flag) {
        PTPCOLL_VERBOSE(10, ("I'm root of the data"));
        narray_knomial_node = &ptpcoll_module->narray_knomial_node[0];
        relative_group_index = 0;
        group_root_index = my_group_index;

        /* for proxy we have little bit more work to do */
        if (PTPCOLL_PROXY & ptpcoll_module->narray_type) {
            /* send the all data to your extra peer */
            for (i = 0; i < ptpcoll_module->narray_knomial_proxy_num; ++i) {
                PTPCOLL_VERBOSE(9, ("Extra send %d, dst %d, tag %d",
                            i, ptpcoll_module->narray_knomial_proxy_extra_index[i], tag - 1));
                rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,
                            group_list[ptpcoll_module->narray_knomial_proxy_extra_index[i]],
                            tag - 1,
                            MCA_PML_BASE_SEND_STANDARD, comm,
                            &(requests[*active_requests])));
                if( OMPI_SUCCESS != rc ) {
                    PTPCOLL_VERBOSE(10, ("Failed to send data"));
                    return OMPI_ERROR;
                }
                ++(*active_requests);
            }
        }
        /*
         * I'm root of the operation
         * send data to radix_k neighbors
         */
        base_block_size = NARRAY_BLOCK_SIZE(count, ptpcoll_module,
                narray_knomial_node->level_size);

        NARRAY_SCATTER_B(narray_knomial_node, my_group_index,
                group_size, data_buffer,
                base_block_size, count, tag, comm, requests,
                active_requests, completed);
        if (0 == completed) {
            *status = PTPCOLL_ROOT_SEND_STARTED;
            return BCOL_FN_STARTED;
        }
        goto EXIT;
    }

    /* <-- non root flow --> */
    group_root_index = input_args->root_route->rank;

    if (group_root_index >= group_size) {
        /* calculate virtual root */
        virtual_root =
            (group_root_index - group_size) /
            mca_bcol_ptpcoll_component.narray_knomial_radix;
        if (my_group_index == virtual_root) {
            PTPCOLL_VERBOSE(10, ("I'm virtual root of the data"));

            rc = MCA_PML_CALL(irecv(data_buffer, count, MPI_BYTE,
                        group_list[group_root_index],
                        tag, comm, &requests[*active_requests]));
            if( OMPI_SUCCESS != rc ) {
                PTPCOLL_VERBOSE(10, ("Failed to receive data"));
                return OMPI_ERROR;
            }
            ++(*active_requests);
            /* act like a root */
            relative_group_index = 0;
            group_root_index = my_group_index;
            goto SCATTER;
        }
        group_root_index = virtual_root;
    }

    relative_group_index = my_group_index - group_root_index;
    if (relative_group_index < 0) {
        relative_group_index += group_size;
    }

    narray_node = &ptpcoll_module->narray_knomial_node[relative_group_index];

    data_src = narray_node->parent_rank + group_root_index;
    if (data_src >= group_size) {
        data_src -= group_size;
    }

    comm_root = group_list[data_src];

    recv_count = NARRAY_BLOCK_SIZE(count, ptpcoll_module, narray_node->level_size);
    offset = recv_count * narray_node->rank_on_level;
    /* make sure that we do not overun memory */
    if (OPAL_UNLIKELY(offset + recv_count > count)) {
        recv_count = count - offset;
        if (0 >= recv_count) {
            goto GATHER;
        }
    }

    curr_data_buffer = (void *)((unsigned char *)data_buffer + (size_t)offset);
    PTPCOLL_VERBOSE(10, ("Bcast, receive data from %d[%d], count %d, tag %d, addr %p len %d offset %d",
                comm_root, data_src, count, tag, data_buffer, recv_count, offset));

    rc = MCA_PML_CALL(irecv(curr_data_buffer, recv_count, MPI_BYTE, comm_root,
                tag, comm, &requests[*active_requests]));
    if( OMPI_SUCCESS != rc ) {
        PTPCOLL_VERBOSE(10, ("Failed to receive data"));
        return OMPI_ERROR;
    }

    ++(*active_requests);

SCATTER:
    *status = PTPCOLL_WAITING_FOR_DATA;

    rc = bcol_ptpcoll_bcast_narray_test_and_scatter_known_root(ptpcoll_module,
            buffer_index, data_buffer,
            count, group_root_index, relative_group_index);

    if (BCOL_FN_COMPLETE != rc) {
        PTPCOLL_VERBOSE(10, ("Not done. Return %d", rc));
        return rc;
    }

GATHER:
    /* recv operation is done */
    *iteration = 0;
    *status = PTPCOLL_GATHER_STARTED;
    rc = bcol_ptpcoll_bcast_narray_knomial_gather(ptpcoll_module,
            buffer_index, data_buffer, count,
            relative_group_index);
    if (BCOL_FN_COMPLETE != rc) {
        assert(0 != *active_requests);
        PTPCOLL_VERBOSE(10, ("Not done. Return %d", rc));
        return rc;
    }

    ++(*iteration); /* I need it for progress */

    /* proxy case */
    if ((PTPCOLL_PROXY & ptpcoll_module->narray_type) &&
            ! input_args->root_flag) {
        *status = PTPCOLL_EXTRA_SEND_STARTED;
        rc =  bcol_ptpcoll_send_n_extra(
                ptpcoll_module,
                data_buffer, count, tag - 1,
                ptpcoll_module->narray_knomial_proxy_extra_index,
                ptpcoll_module->narray_knomial_proxy_num,
                input_args->root_route->rank,
                comm, active_requests, requests);
            if (BCOL_FN_COMPLETE != rc) {
                return rc;
            }
    }

EXIT:
    return BCOL_FN_COMPLETE;
}

/* Pasha : need to move this code to some common function */
static int bcol_ptpcoll_bcast_narray_knomial_scatter_gatther_known_root_extra(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int tag;
    int rc;
    int completed = 0; /* not completed */
    uint32_t buffer_index = input_args->buffer_index;

    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *iteration =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].iteration);
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;

    PTPCOLL_VERBOSE(3, ("BCAST known root, index_this_type %d, num_of_this_type %d",
                const_args->index_of_this_type_in_collective + 1,
                const_args->n_of_this_type_in_collective));

    /* keep tag within the limit support by the pml */
    tag = (PTPCOLL_TAG_OFFSET + input_args->sequence_num * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;
    /* reset active requests */
    *active_requests = 0;
    /* reset iteration counter */
    *iteration = -1;

    PTPCOLL_VERBOSE(8, ("bcol_ptpcoll_bcast_narray_knomial_scatter_gatther_known_root_extra, buffer index: %d "
                "tag: %d "
                "tag_mask: %d "
                "sn: %d "
                "root: %d "
                "buff: %p "
                ,buffer_index, tag,
                ptpcoll_module->tag_mask, input_args->sequence_num,
                input_args->root_flag,
                data_buffer
                ));

    /* we have a power 2 group */
    if (input_args->root_flag) {
        PTPCOLL_VERBOSE(10, ("I'm EXTRA root of the data"));
        /* send the all data to your proxy peer */
        rc = MCA_PML_CALL(isend(data_buffer, count, MPI_BYTE,
                    group_list[ptpcoll_module->narray_knomial_proxy_extra_index[0]], tag,
                    MCA_PML_BASE_SEND_STANDARD, comm,
                    &(requests[*active_requests])));
        if( OMPI_SUCCESS != rc ) {
            PTPCOLL_VERBOSE(10, ("Failed to send data"));
            return OMPI_ERROR;
        }
        ++(*active_requests);

        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if (0 == completed) {
            /* we have to store the iteration number somewhere */
            return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
        }
    } else {
        PTPCOLL_VERBOSE(9, ("Posting recive from %d tag %d",
                        ptpcoll_module->narray_knomial_proxy_extra_index[0], tag - 1));
        rc = MCA_PML_CALL(irecv(data_buffer, count, MPI_BYTE,
                    group_list[ptpcoll_module->narray_knomial_proxy_extra_index[0]],
                    tag - 1, comm, &requests[*active_requests]));
        ++(*active_requests);

        completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
        if (0 == completed) {
            PTPCOLL_VERBOSE(10, ("Test was not matched - %d", rc));
            return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
        }
    }

    return BCOL_FN_COMPLETE;
}

static int bcol_ptpcoll_bcast_known_root_extra_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    int rc;
    int completed = 0; /* not completed */
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;
    ompi_request_t **requests =
        ptpcoll_module->ml_mem.ml_buf_desc[input_args->buffer_index].requests;
    uint32_t buffer_index = input_args->buffer_index;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);

    PTPCOLL_VERBOSE(10, ("bcol_ptpcoll_bcast_binomial_known_root_extra_progress extra, was called\n"));

    completed = mca_bcol_ptpcoll_test_all_for_match(active_requests, requests, &rc);
    if (0 == completed) {
        return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
    }

    PTPCOLL_VERBOSE(10, ("Test was matched - %d", rc));
    return BCOL_FN_COMPLETE;
}


static int bcol_ptpcoll_bcast_narray_progress(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int tag = -1;
    int rc;
    int group_size = ptpcoll_module->group_size;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    uint32_t buffer_index = input_args->buffer_index;

    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **send_requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    ompi_request_t **recv_request =
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests[0];
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int matched = true;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int relative_group_index = 0;
    netpatterns_tree_node_t *narray_node = NULL;

    PTPCOLL_VERBOSE(3, ("Bcast, Narray tree Progress"));


    PTPCOLL_VERBOSE(8, ("bcol_ptpcoll_bcast_k_nomial_known_root, buffer index: %d "
                         "tag: %d "
                         "tag_mask: %d "
                         "sn: %d "
                         "root: %d [%d]"
                         "buff: %p ",
                         buffer_index, tag,
                         ptpcoll_module->tag_mask, input_args->sequence_num,
                         input_args->root_flag, input_args->root_route->rank,
                         data_buffer));

    if (0 == *active_requests) {
        int group_root_index = input_args->root_route->rank;
        /* If the collective does not have any active requests, it
           means the initial data was not received from parent.
           Check if some data arrived
         */
        if (0 == mca_bcol_ptpcoll_test_for_match(recv_request, &rc)) {
            PTPCOLL_VERBOSE(10, ("Test was not matched - %d", rc));
            /* No data was received, return no match error */
            return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
        }

        /* set all paremetres */
        relative_group_index = my_group_index - group_root_index;
        if (relative_group_index < 0) {
            relative_group_index +=group_size;
        }
        narray_node = &ptpcoll_module->narray_node[relative_group_index];
        /* keep tag within the limit support by the pml */
        tag = (PTPCOLL_TAG_OFFSET + input_args->sequence_num * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
        /* mark this as a collective tag, to avoid conflict with user-level flags */
        tag = -tag;
        /* Bcast the data */
        NARRAY_BCAST_NB(narray_node, group_root_index, group_size,
                data_buffer, count, tag, comm, send_requests, active_requests);
    }

    /* All data was received and sent out.
       Check if the completion arrived */
    matched = mca_bcol_ptpcoll_test_all_for_match
        (active_requests, send_requests, &rc);
    if (OMPI_SUCCESS != rc) {
        return OMPI_ERROR;
    }

    /* If it is last call, we have to recycle memory */
    if(matched) {
        return BCOL_FN_COMPLETE;
    } else {
        PTPCOLL_VERBOSE(10, ("bcast root is started"));
        return BCOL_FN_STARTED;
    }
}

static int bcol_ptpcoll_bcast_narray(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *)const_args->bcol_module;

    int tag;
    int rc;
    int data_src;
    int group_size = ptpcoll_module->group_size;
    int *group_list = ptpcoll_module->super.sbgp_partner_module->group_list;
    uint32_t buffer_index = input_args->buffer_index;

    ompi_communicator_t* comm = ptpcoll_module->super.sbgp_partner_module->group_comm;
    ompi_request_t **send_requests =
        ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests;
    ompi_request_t **recv_request =
        &ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].requests[0];
    void *data_buffer = (void *) (
            (unsigned char *)input_args->sbuf +
            (size_t)input_args->sbuf_offset);
    int count = input_args->count * input_args->dtype->super.size;
    int *active_requests =
        &(ptpcoll_module->ml_mem.ml_buf_desc[buffer_index].active_requests);
    int matched = true;
    int my_group_index = ptpcoll_module->super.sbgp_partner_module->my_index;
    int group_root_index;
    int relative_group_index = 0;
    netpatterns_tree_node_t *narray_node = NULL;

    PTPCOLL_VERBOSE(3, ("Bcast, Narray tree"));

    /* reset active request counter */
    (*active_requests) = 0;
    /* keep tag within the limit support by the pml */
    tag = (PTPCOLL_TAG_OFFSET + input_args->sequence_num * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);
    /* mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;

    PTPCOLL_VERBOSE(8, ("bcol_ptpcoll_bcast_narray, buffer index: %d "
                         "tag: %d "
                         "tag_mask: %d "
                         "sn: %d "
                         "root: %d "
                         "buff: %p ",
                         buffer_index, tag,
                         ptpcoll_module->tag_mask, input_args->sequence_num,
                         input_args->root_flag,
                         data_buffer));


    if (input_args->root_flag) {
        PTPCOLL_VERBOSE(10, ("I'm root of the data"));
        narray_node = &ptpcoll_module->narray_node[0];
        group_root_index = my_group_index;
        /*
         * I'm root of the operation
         * send data to N childrens
         */
        goto NARRAY_BCAST_START;
    }

    /* I'm not root */
    group_root_index = input_args->root_route->rank;

    relative_group_index = my_group_index - group_root_index;
    if (relative_group_index < 0) {
        relative_group_index += group_size;
    }

    data_src =
        ptpcoll_module->narray_node[relative_group_index].parent_rank +
        group_root_index;
    if (data_src >= group_size) {
        data_src -= group_size;
    }

    PTPCOLL_VERBOSE(10, ("Bcast, receive data from %d [%d], count %d, tag %d, addr %p",
                group_list[data_src], data_src,
                count, tag, data_buffer));


    rc = MCA_PML_CALL(irecv(data_buffer, count, MPI_BYTE,
                group_list[data_src],
                tag, comm, recv_request));
    if( OMPI_SUCCESS != rc ) {
        PTPCOLL_VERBOSE(10, ("Failed to receive data"));
        return OMPI_ERROR;
    }

    /* We can not block. So run couple of test for data arrival */
    if (0 == mca_bcol_ptpcoll_test_for_match(recv_request, &rc)) {
        PTPCOLL_VERBOSE(10, ("Test was not matched - %d", rc));
        /* No data was received, return no match error */
        return (OMPI_SUCCESS != rc) ? rc : BCOL_FN_STARTED;
    }

    narray_node = &ptpcoll_module->narray_node[relative_group_index];

NARRAY_BCAST_START:
    /* Bcast the data */
    NARRAY_BCAST_NB(narray_node, group_root_index, group_size,
                    data_buffer, count, tag, comm, send_requests, active_requests);

    matched = mca_bcol_ptpcoll_test_all_for_match
        (active_requests, send_requests, &rc);
    if (OMPI_SUCCESS != rc) {
        return OMPI_ERROR;
    }

    /* If it is last call, we have to recycle memory */
    if(matched) {
        return BCOL_FN_COMPLETE;
    } else {
        PTPCOLL_VERBOSE(10, ("bcast root is started"));
        return BCOL_FN_STARTED;
    }
}

int bcol_ptpcoll_bcast_init(mca_bcol_base_module_t *super)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module =
                    (mca_bcol_ptpcoll_module_t *) super;

    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    comm_attribs.bcoll_type = BCOL_BCAST;
    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 1024 * 1024;
    comm_attribs.waiting_semantics = NON_BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000; /* range 1 */

    inv_attribs.datatype_bitmap = 0xffffffff;
    inv_attribs.op_types_bitmap = 0xffffffff;


    comm_attribs.data_src = DATA_SRC_UNKNOWN;

    if(PTPCOLL_KN_EXTRA == ptpcoll_module->pow_ktype) {
        mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                bcol_ptpcoll_bcast_k_nomial_extra_known_and_anyroot,
                bcol_ptpcoll_bcast_k_nomial_extra_known_and_anyroot_progress);
    } else {
        mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                bcol_ptpcoll_bcast_k_nomial_anyroot,
                bcol_ptpcoll_bcast_k_nomial_anyroot_progress);
    }

    comm_attribs.data_src = DATA_SRC_KNOWN;
    switch(mca_bcol_ptpcoll_component.bcast_small_messages_known_root_alg) {
        case PTPCOLL_KNOMIAL:
            if(PTPCOLL_KN_EXTRA == ptpcoll_module->pow_ktype) {
                mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                        bcol_ptpcoll_bcast_k_nomial_extra_known_and_anyroot,
                        bcol_ptpcoll_bcast_k_nomial_extra_known_and_anyroot_progress);
            } else {
                mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                        bcol_ptpcoll_bcast_k_nomial_known_root,
                        bcol_ptpcoll_bcast_k_nomial_known_root_progress);
            }
            break;
        case PTPCOLL_NARRAY:
            mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                        bcol_ptpcoll_bcast_narray,
                        bcol_ptpcoll_bcast_narray_progress);
            break;
        default:
            PTPCOLL_ERROR(("Unknown algorithm index was selected %",
             mca_bcol_ptpcoll_component.bcast_small_messages_known_root_alg));
            return OMPI_ERROR;
    }

    comm_attribs.data_src = DATA_SRC_UNKNOWN;
    inv_attribs.bcol_msg_min = 10000000;
    inv_attribs.bcol_msg_max = 10485760; /* range 4 */

    /* Anyroot large messages functions registration */

    if (PTPCOLL_EXTRA == ptpcoll_module->pow_2type) {
        mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                bcol_ptpcoll_bcast_binomial_scatter_gatther_anyroot_extra,
                bcol_ptpcoll_bcast_binomial_scatter_gatther_anyroot_extra_progress);
    } else {
        mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                bcol_ptpcoll_bcast_binomial_scatter_gatther_anyroot,
                bcol_ptpcoll_bcast_binomial_scatter_gatther_anyroot_progress);
    }

    /* Known-root large messages functions registration */

    comm_attribs.data_src = DATA_SRC_KNOWN;
    switch(mca_bcol_ptpcoll_component.bcast_large_messages_known_root_alg) {
        case PTPCOLL_BINOMIAL_SG:
            if (PTPCOLL_EXTRA == ptpcoll_module->pow_2type) {
                mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                        bcol_ptpcoll_bcast_binomial_scatter_gatther_known_root_extra,
                        bcol_ptpcoll_bcast_known_root_extra_progress);
                        /* bcol_ptpcoll_bcast_binomial_scatter_gatther_known_root_extra_progress); */
            } else {
                mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                        bcol_ptpcoll_bcast_binomial_scatter_gatther_known_root,
                        bcol_ptpcoll_bcast_binomial_scatter_gatther_known_root_progress);
            }
            break;
        case PTPCOLL_NARRAY_KNOMIAL_SG:
            if (PTPCOLL_EXTRA == ptpcoll_module->narray_type) {
                mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                        bcol_ptpcoll_bcast_narray_knomial_scatter_gatther_known_root_extra,
                        bcol_ptpcoll_bcast_known_root_extra_progress);
            } else {
                mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                        bcol_ptpcoll_bcast_narray_knomial_scatter_gatther_known_root,
                        bcol_ptpcoll_bcast_narray_knomial_scatter_gatther_known_root_progress);
            }
            break;
        default:
            PTPCOLL_ERROR(("Unknown algorithm index was selected %",
                        mca_bcol_ptpcoll_component.bcast_large_messages_known_root_alg));
            return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
