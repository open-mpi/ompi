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

#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <inttypes.h>

#include "bcol_iboffload.h"
#include "bcol_iboffload_bcast.h"
#include "bcol_iboffload_frag.h"
#include "bcol_iboffload_task.h"
#include "bcol_iboffload_collreq.h"
#include "bcol_iboffload_collfrag.h"
#include "bcol_iboffload_endpoint.h"

#include "opal/include/opal/types.h"

static int mca_bcol_iboffload_bcast_init(
                               bcol_function_args_t *fn_arguments,
                               mca_bcol_iboffload_module_t *iboffload_module,
                               mca_bcol_iboffload_collreq_t **coll_request,
                               bool if_bcol_last, int mq_credits,
                               collective_message_progress_function progress_fn)
{
    int rc;

    ompi_free_list_item_t *item;
    mca_bcol_iboffload_collfrag_t *coll_fragment;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    int my_group_index = iboffload_module->super.sbgp_partner_module->my_index;

    OMPI_FREE_LIST_WAIT(&cm->collreqs_free, item, rc);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("Wait for free list failed.\n"));
        return rc;
    }
    /* setup call request */
    (*coll_request) = (mca_bcol_iboffload_collreq_t *) item;

    (*coll_request)->n_fragments  = 0;
    (*coll_request)->n_frags_sent = 0;
    (*coll_request)->n_frag_mpi_complete = 0;
    (*coll_request)->n_frag_net_complete = 0;
    (*coll_request)->if_bcol_last = if_bcol_last;
    (*coll_request)->ml_buffer_index = fn_arguments->buffer_index;
    (*coll_request)->completion_cb_fn = NULL;
    (*coll_request)->buffer_info[SBUF].buf = (void *) (
            (unsigned char *)fn_arguments->sbuf +
            fn_arguments->sbuf_offset);
    (*coll_request)->buffer_info[SBUF].offset = fn_arguments->sbuf_offset;
    (*coll_request)->buffer_info[RBUF].offset = fn_arguments->rbuf_offset;

    (*coll_request)->dtype = fn_arguments->dtype;
    (*coll_request)->count = fn_arguments->count;
    (*coll_request)->module = iboffload_module;
    /* TODO Pasha: we need it for pending quque. Set it later. */
    (*coll_request)->progress_fn = progress_fn;
    /* TODO Pasha: fix it  later */
    (*coll_request)->qp_index = MCA_BCOL_IBOFFLOAD_QP_REGULAR;

    (*coll_request)->order_info = &fn_arguments->order_info;

    coll_fragment = &((*coll_request)->first_collfrag);
    mca_bcol_iboffload_collfrag_init(coll_fragment);

    /** Vasily ????? */
    /* mq_credits = (*coll_request)->total_tasks_num; */
    coll_fragment->mq_credits = mq_credits;
    coll_fragment->mq_index = COLL_MQ;
    /* Pasha: just set it to zero */
    coll_fragment->last_wait_num = 0;
    coll_fragment->alg = -2; /* used only for debug */
    /*
    if (my_rank == algthm_ptr->root) {
        coll_fragment->last_wait_num = 0;
    } else {
        coll_fragment->last_wait_num = algth_lst->last_wait_num;
    }
    */
    /* Pasha: we have nothing to unpack */
    coll_fragment->unpack_size = 0;
    /* coll_fragment->unpack_size = pack_len; */
    /* coll_fragment->alg = RECURSIVE_DOUBLING_TREE_BCAST; */

    /* set pointers for (coll frag) <-> (coll full request) */
    (*coll_request)->user_handle_freed = false;

    fn_arguments->bcol_opaque_data = (void *) (*coll_request);

    if (true == fn_arguments->root_flag) {
        (*coll_request)->root = my_group_index;
    } else {
        (*coll_request)->root = fn_arguments->root_route->rank;
    }

    MCA_BCOL_IBOFFLOAD_SET_COLL_REQ_LINKS((*coll_request), coll_fragment);
    return OMPI_SUCCESS;
}
static inline __opal_attribute_always_inline__ int
binomial_scatter_smsg(
        mca_bcol_iboffload_module_t *iboffload_module,
        mca_bcol_iboffload_collfrag_t *coll_fragment,
        struct mqe_task **last_send,
        int radix_mask_pow,
        uint32_t my_group_index,
        size_t send_size
        )
{
    int rc, dst;
    int radix_mask = radix_mask_pow >= 0 ? 1 << radix_mask_pow : 0;

    while(radix_mask > 0) {
        /* For each level of tree, do sends */
        dst = my_group_index ^ radix_mask;
        rc = mca_bcol_iboffload_send_small_buff_setup(
                last_send, send_size, dst,
                iboffload_module, coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to isend data"));
            return rc;
        }

        radix_mask >>= 1;
    }

    return OMPI_SUCCESS;
}

#define BINOMIAL_SMALL_SCATTER(                                                                                     \
        iboffload_module, coll_fragment,                                                                            \
        last_wait, last_send,                                                                                       \
        distance,                                                                                                   \
        my_group_index,                                                                                             \
        segment_size                                                                                                \
        )                                                                                                           \
do {                                                                                                                \
    int rc = OMPI_SUCCESS;                                                                                          \
    int dst;                                                                                                        \
    int send_size;                                                                                                  \
    int dst_boundary_rank;                                                                                          \
    int radix_mask_pow = distance;                                                                                  \
    int radix_mask = (distance) >= 0 ? 1 << (distance) : 0;                                                         \
    IBOFFLOAD_VERBOSE(10, ("BCAST SCATTER %d %d", radix_mask, distance));                                           \
                                                                                                                    \
    while(radix_mask > 0) {                                                                                         \
        /* For each level of tree, do sends */                                                                      \
        dst = my_group_index ^ radix_mask;                                                                          \
        dst_boundary_rank = dst & ((~(int)0) << (radix_mask_pow));                                                  \
                                                                                                                    \
        IBOFFLOAD_VERBOSE(10, ("Scatter data to %d , len %d offset %d", dst, send_size, send_offset));              \
                                                                                                                    \
        rc = mca_bcol_iboffload_send_small_buff_setup(                                                              \
                &last_send, send_size, dst,                                                                         \
                iboffload_module, coll_fragment);                                                                   \
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {                                                                    \
            IBOFFLOAD_VERBOSE(10, ("Failed to isend data"));                                                        \
            return rc;                                                                                              \
        }                                                                                                           \
        radix_mask >>= 1;                                                                                           \
        /* radix_mask_pow--; */                                                                                     \
    }                                                                                                               \
} while(0)


int mca_bcol_iboffload_small_msg_bcast_progress(
                        bcol_function_args_t *input_args,
                        struct coll_ml_function_t *const_args)
{
    mca_bcol_iboffload_collreq_t *coll_request =
                 (mca_bcol_iboffload_collreq_t *)
                                   input_args->bcol_opaque_data;

    IBOFFLOAD_VERBOSE(10, ("Run progress.\n"));

    /* We should send the data to our children in the tree before 
       the upper layer will start with buffers recycling */
    if (BCOL_AND_NET_ARE_COMPLETED(coll_request)) {
        coll_request->user_handle_freed = true;
        if (COLLREQ_IS_DONE(coll_request)) {
            IBOFFLOAD_VERBOSE(10, ("Coll request already done.\n"));
            RELEASE_COLLREQ(coll_request);
        }

        IBOFFLOAD_VERBOSE(10, ("New bcast done !!!"));
        return BCOL_FN_COMPLETE;
    }

    return BCOL_FN_STARTED;
}

static int mca_bcol_iboffload_small_msg_bcast_exec(mca_bcol_iboffload_module_t *iboffload_module,
                                                   mca_bcol_iboffload_collreq_t *coll_request)
{
    netpatterns_pair_exchange_node_t *recursive_doubling_tree =
        &iboffload_module->recursive_doubling_tree;

    int rc,
        distance_mask_pow , dst,
        group_src, power_of_2_distance;

    uint32_t pack_len;
    int my_group_index = iboffload_module->super.sbgp_partner_module->my_index;

    struct mqe_task *last_send = NULL,
                    *last_wait = NULL;
    mca_bcol_iboffload_collfrag_t *coll_fragment = &coll_request->first_collfrag;

    IBOFFLOAD_VERBOSE(10,("Entering small msg iboffload bcast"));

    if (OPAL_UNLIKELY(!iboffload_module->connection_status[RECURSIVE_DOUBLING_TREE_BCAST])) {
        IBOFFLOAD_VERBOSE(10,("Bcast open new connection "));
        bcol_iboffload_setup_binomial_connection(iboffload_module);
    }

    pack_len = coll_request->count * coll_request->dtype->super.size;
    IBOFFLOAD_VERBOSE(10,("My packet length %d pack_len frag_count %d dtype size %d ",
                            pack_len,
                            coll_request->count,
                            coll_request->dtype->super.size));

    /* it is estimated mq consumption... */
    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
                 iboffload_module, coll_fragment->mq_index, coll_fragment->mq_credits))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));
        goto out_of_resources;
    }

    coll_fragment->tail_next = &coll_fragment->to_post;
    coll_request->buffer_info[SBUF].lkey = iboffload_module->rdma_block.ib_info.lkey;

    if (coll_request->root == my_group_index) {
        IBOFFLOAD_VERBOSE(10, ("I'm root of the data"));

        /* Send data to the extra peer */
        if (recursive_doubling_tree->n_extra_sources > 0) {
            /* send the all data to your extra peer */
            dst = recursive_doubling_tree->rank_extra_source;
            IBOFFLOAD_VERBOSE(10,("Sending the dat to Dst %d",dst));
            rc = mca_bcol_iboffload_send_small_buff_setup(
                    &last_send, pack_len, dst,
                    iboffload_module, coll_fragment);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to"
                            " mca_bcol_iboffload_send_large_buff_setup"));
                goto out_of_resources;
            }
        }

        distance_mask_pow =
            iboffload_module->power_of_2 - 1;

       rc = binomial_scatter_smsg(iboffload_module, coll_fragment,
                &last_send, distance_mask_pow,
                my_group_index, pack_len);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to binomial_scatter_smsg"));
            goto out_of_resources;
        }

        goto finalize;
    }

    /* prepare and post recv operation */
    group_src = bcol_iboffload_binomial_root_to_src(coll_request->root,
            my_group_index, iboffload_module->power_of_2_ranks,
            iboffload_module->group_size, &power_of_2_distance);
    assert(group_src >= 0);

    if (0 > power_of_2_distance) {
        /* the rank is virtual root for this group, receive the data
           and scatter gather as root */
        IBOFFLOAD_VERBOSE(10,("Virtual root distance_mask_pow %d ",iboffload_module->power_of_2));
        distance_mask_pow = iboffload_module->power_of_2 - 1;
    } else {
        distance_mask_pow = power_of_2_distance - 1;
    }

    IBOFFLOAD_VERBOSE(10, ("Bcast, receive data from %d[%d], count %d, offset %d",
                group_src));

    rc = mca_bcol_iboffload_recv_small_buff_setup(&last_wait,
                                pack_len, group_src,
                                iboffload_module, coll_fragment);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to setup data receive"));
        goto out_of_resources;
    }

    rc = binomial_scatter_smsg(iboffload_module, coll_fragment,
            &last_send, distance_mask_pow,
            my_group_index, pack_len);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to binomial_scatter_smsg"));
        goto out_of_resources;
    }

    if (recursive_doubling_tree->n_extra_sources > 0 &&
            iboffload_module->power_of_2 - 1 != distance_mask_pow) {
/*

    if ((recursive_doubling_tree->n_extra_sources > 0) &&
            ((my_group_index + iboffload_module->power_of_2_ranks ) <
            iboffload_module->group_size) ) {
  */
          dst = recursive_doubling_tree->rank_extra_source;
        /*
        dst = my_group_index + iboffload_module->power_of_2_ranks;
        */

        rc = mca_bcol_iboffload_send_small_buff_setup(
                &last_send, pack_len, dst,
                iboffload_module, coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to"
                        " mca_bcol_iboffload_send_small_buff_setup"));
            goto out_of_resources;
        }
    }

finalize:
    /* end of list */
    *coll_fragment->tail_next = NULL;

    /* finish initializing full message descriptor */
    (coll_request)->n_fragments  += 1;
    (coll_request)->n_frags_sent += 1;

    if (NULL != last_wait) {
        last_wait->flags |= MQE_WR_FLAG_SIGNAL;
        coll_fragment->signal_task_wr_id = last_wait->wr_id;
        last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;
    } else {
        last_send->flags |= MQE_WR_FLAG_SIGNAL;
        coll_fragment->signal_task_wr_id = last_send->wr_id;
        last_send->wr_id = (uint64_t) (uintptr_t) coll_fragment;
    }
    /* post the mwr */
    rc = mca_bcol_iboffload_post_mqe_tasks(iboffload_module, coll_fragment->to_post);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload_module->super, coll_request->order_info);

    IBOFFLOAD_VERBOSE(10, ("Return success.\n"));
    return BCOL_FN_STARTED;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Barrier, adding collfrag to collfrag_pending.\n"));
    rc =
        mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload_module);
    return (OMPI_SUCCESS != rc) ? BCOL_FN_NOT_STARTED : BCOL_FN_STARTED;
}

int mca_bcol_iboffload_small_msg_bcast_intra(bcol_function_args_t *fn_arguments,
                                                   struct coll_ml_function_t *const_args)
{
    mca_bcol_iboffload_module_t *iboffload_module =
        (mca_bcol_iboffload_module_t *) const_args->bcol_module;

    int rc;
    int mq_credits = iboffload_module->power_of_2 + 2;
    bool if_bcol_last = BCOL_IBOFFLOAD_IS_LAST_CALL(const_args);
    mca_bcol_iboffload_collreq_t *coll_request;

    MCA_BCOL_CHECK_ORDER(const_args->bcol_module, fn_arguments);

    rc = mca_bcol_iboffload_bcast_init(fn_arguments, iboffload_module,
            &coll_request, if_bcol_last, mq_credits,
            mca_bcol_iboffload_small_msg_bcast_exec);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    rc = coll_request->progress_fn(iboffload_module, coll_request);

    IBOFFLOAD_VERBOSE(10, ("mca_bcol_iboffload_small_msg_bcast_intra was started [%d]\n", rc));
    return rc;
}

static int mca_bcol_iboffload_small_msg_bcast_extra_exec(mca_bcol_iboffload_module_t *iboffload_module,
                                                   mca_bcol_iboffload_collreq_t *coll_request)
{
    netpatterns_pair_exchange_node_t *recursive_doubling_tree =
        &iboffload_module->recursive_doubling_tree;

    int rc,
        dst;
    int my_group_index = iboffload_module->super.sbgp_partner_module->my_index;
    uint32_t pack_len;

    struct mqe_task *last_send = NULL,
                    *last_wait = NULL;
    mca_bcol_iboffload_collfrag_t *coll_fragment = &coll_request->first_collfrag;

    IBOFFLOAD_VERBOSE(10,("Entering small msg extra iboffload bcast"));

    if (OPAL_UNLIKELY(!iboffload_module->connection_status[RECURSIVE_DOUBLING_TREE_BCAST])) {
        IBOFFLOAD_VERBOSE(10,("Bcast open new connection "));
        bcol_iboffload_setup_binomial_connection(iboffload_module);
    }


    pack_len = coll_request->count * coll_request->dtype->super.size;
    coll_request->buffer_info[SBUF].lkey = iboffload_module->rdma_block.ib_info.lkey;

    IBOFFLOAD_VERBOSE(10,("My packet length %d pack_len frag_count %d dtype size %d ",
                            pack_len,
                            coll_request->count,
                            coll_request->dtype->super.size));

    /* it is estimated mq consumption... */
    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
                 iboffload_module,
                 coll_fragment->mq_index, coll_fragment->mq_credits))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));
        goto out_of_resources;
    }

    coll_fragment->tail_next = &coll_fragment->to_post;


    if (coll_request->root == my_group_index) {
        IBOFFLOAD_VERBOSE(10, ("I'm root of the data %d", iboffload_module->power_of_2));
        /* send the all data to your extra peer */

        dst = recursive_doubling_tree->rank_extra_source;
        IBOFFLOAD_VERBOSE(10,("Im extra root sending data to %d \n",dst));
        rc = mca_bcol_iboffload_send_small_buff_setup(
                &last_send, pack_len, dst,
                iboffload_module, coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to"
                        " mca_bcol_iboffload_send_small_buff_setup"));
            goto out_of_resources;
        }
    } else {
        /* Not root case */
        dst = recursive_doubling_tree->rank_extra_source;
        rc = mca_bcol_iboffload_recv_small_buff_setup(&last_wait,
                pack_len, dst,
                iboffload_module, coll_fragment);

        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to setup data receive"));
            return OMPI_ERROR;
        }
    }

    /* end of list */
    *coll_fragment->tail_next = NULL;

    /* finish initializing full message descriptor */
    (coll_request)->n_fragments  = 1;
    (coll_request)->n_frags_sent = 1;

    if (NULL != last_wait) {
        last_wait->flags |= MQE_WR_FLAG_SIGNAL;
        coll_fragment->signal_task_wr_id = last_wait->wr_id;
        last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;
    } else {
        last_send->flags |= MQE_WR_FLAG_SIGNAL;
        coll_fragment->signal_task_wr_id = last_send->wr_id;
        last_send->wr_id = (uint64_t) (uintptr_t) coll_fragment;
    }
    /* post the mwr */
    rc = mca_bcol_iboffload_post_mqe_tasks(iboffload_module, coll_fragment->to_post);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload_module->super, coll_request->order_info);

    IBOFFLOAD_VERBOSE(10, ("Return success.\n"));
    return BCOL_FN_STARTED;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Barrier, adding collfrag to collfrag_pending.\n"));
    rc =
        mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload_module);
    return (OMPI_SUCCESS != rc) ? BCOL_FN_NOT_STARTED : BCOL_FN_STARTED;
}

int mca_bcol_iboffload_small_msg_bcast_extra_intra(bcol_function_args_t *fn_arguments,
                                                   struct coll_ml_function_t *const_args)
{
    mca_bcol_iboffload_module_t *iboffload_module =
        (mca_bcol_iboffload_module_t *)const_args->bcol_module;

    int rc;
    int mq_credits = 2;
    bool if_bcol_last = BCOL_IBOFFLOAD_IS_LAST_CALL(const_args);
    mca_bcol_iboffload_collreq_t *coll_request;

    MCA_BCOL_CHECK_ORDER(const_args->bcol_module, fn_arguments);

    rc = mca_bcol_iboffload_bcast_init(fn_arguments, iboffload_module,
            &coll_request, if_bcol_last, mq_credits,
            mca_bcol_iboffload_small_msg_bcast_extra_exec);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    rc = coll_request->progress_fn(iboffload_module, coll_request);

    IBOFFLOAD_VERBOSE(10, ("mca_bcol_iboffload_small_msg_bcast_extra_exec was started [%d]\n", rc));
    return rc;
}

/* Large message scatter-allgather with zero copy */
int mca_bcol_iboffload_zero_copy_progress(bcol_function_args_t *fn_arguments,
                                                   struct coll_ml_function_t *const_args)
{
    int i;
    mca_bcol_iboffload_collreq_t *coll_request =
                 (mca_bcol_iboffload_collreq_t *)fn_arguments->bcol_opaque_data;

    /* IBOFFLOAD_VERBOSE(10, ("Run general progress. %d == %d *  %d == %d",
                coll_request->n_frag_mpi_complete, coll_request->n_fragments,
                coll_request->n_frag_net_complete, coll_request->n_fragments)); */

    /* Complete the bcast - progress releases full request descriptors */
    for (i = 0; i < mca_bcol_iboffload_component.max_progress_pull; i++) {
        if (coll_request->n_frag_mpi_complete == coll_request->n_fragments &&
            coll_request->n_frag_net_complete == coll_request->n_fragments) {

            IBOFFLOAD_VERBOSE(10, ("Deregister user buff.\n"));
            coll_request->module->device->mpool->mpool_deregister(
                    coll_request->module->device->mpool,
                    (mca_mpool_base_registration_t *) coll_request->buffer_info[SBUF].iboffload_reg);
            coll_request->buffer_info[SBUF].iboffload_reg = NULL;

            RELEASE_COLLREQ(coll_request);
            IBOFFLOAD_VERBOSE(10, ("New bcast done !!!"));
            return BCOL_FN_COMPLETE;
        }
    }

    /* IBOFFLOAD_VERBOSE(10, ("Bcast general progress done")); */

    /* done */
    return BCOL_FN_STARTED;
}
/* Pasha: I have to move it to static inline later, it looks too ugly for macro */
#define BINOMIAL_SCATTER(                                                                                           \
        iboffload_module, coll_fragment,                                                                            \
        last_wait, last_send,                                                                                       \
        distance,                                                                                                   \
        my_group_index,                                                                                             \
        segment_size, count                                                                                         \
        )                                                                                                           \
do {                                                                                                                \
    int rc = OMPI_SUCCESS;                                                                                          \
    int dst;                                                                                                        \
    int send_size;                                                                                                  \
    int send_offset;                                                                                                \
    int delta;                                                                                                      \
    int dst_boundary_rank;                                                                                          \
    int radix_mask_pow = distance;                                                                                  \
    int radix_mask = (distance) >= 0 ? 1 << (distance) : 0;                                                         \
    IBOFFLOAD_VERBOSE(10, ("BCAST SCATTER %d %d", radix_mask, distance));                                           \
                                                                                                                    \
    while(radix_mask > 0) {                                                                                         \
        /* For each level of tree, do sends */                                                                      \
        dst = my_group_index ^ radix_mask;                                                                          \
        dst_boundary_rank = dst & ((~(int)0) << (radix_mask_pow));                                                  \
        send_offset = segment_size * dst_boundary_rank;                                                             \
        /* Pasha: make sure that we handle the corner cases */                                                      \
        delta = count - send_offset;                                                                                \
        if (OPAL_UNLIKELY(delta <= 0)) {                                                                            \
            radix_mask >>= 1;                                                                                       \
            radix_mask_pow--;                                                                                       \
            continue; /* we have to send something, other way it will hang */                                       \
        } else  {                                                                                                   \
            /* the tail case */                                                                                     \
            send_size = (int)                                                                                       \
            (delta - (int)segment_size * radix_mask) < 0 ? delta :                                                  \
            (int)segment_size * radix_mask;                                                                         \
        }                                                                                                           \
        IBOFFLOAD_VERBOSE(10, ("Scatter data to %d , len %d offset %d", dst, send_size, send_offset));              \
        rc = mca_bcol_iboffload_recv_rtr_setup(                                                                     \
                &last_wait, dst, iboffload_module, coll_fragment);                                                  \
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {                                                                                  \
            IBOFFLOAD_VERBOSE(10, ("Failed to isend data"));                                                        \
            return OMPI_ERROR;                                                                                      \
        }                                                                                                           \
        rc = mca_bcol_iboffload_send_large_buff_setup(                                                              \
                &last_send, SBUF, send_offset, send_size, dst,                                                      \
                iboffload_module, coll_fragment);                                                                   \
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {                                                                                  \
            IBOFFLOAD_VERBOSE(10, ("Failed to isend data"));                                                        \
            return OMPI_ERROR;                                                                                      \
        }                                                                                                           \
        radix_mask >>= 1;                                                                                           \
        radix_mask_pow--;                                                                                           \
    }                                                                                                               \
} while(0)

static int mca_bcol_iboffload_bcast_scatter_allgather_exec(mca_bcol_iboffload_module_t *iboffload_module,
        mca_bcol_iboffload_collreq_t *coll_request)
{
    netpatterns_pair_exchange_node_t *recursive_doubling_tree =
        &iboffload_module->recursive_doubling_tree;

    int rc,
        dst,
        group_src, power_of_2_distance,
        recv_count;
    size_t offset;
    int count = coll_request->count * coll_request->dtype->super.size;
    int my_group_index = iboffload_module->ibnet->super.my_index;
    size_t base_block_size =
        (count +  iboffload_module->power_of_2_ranks - 1) /
        iboffload_module->power_of_2_ranks;

    struct mqe_task *last_send = NULL,
                    *last_wait = NULL;
    mca_bcol_iboffload_collfrag_t *coll_fragment = &coll_request->first_collfrag;

    if (OPAL_UNLIKELY(!iboffload_module->connection_status[RECURSIVE_DOUBLING_TREE_BCAST])) {
        bcol_iboffload_setup_binomial_connection(iboffload_module);
    }

    /* register memory in mpool/rcache */
    rc = mca_bcol_iboffload_prepare_buffer(coll_request->buffer_info[SBUF].buf, count,
            &coll_request->buffer_info[SBUF].iboffload_reg, iboffload_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("Cannot register memory: "
                         "addr - %p, %d bytes.\n",
                          coll_request->buffer_info[SBUF].buf, count));
        return OMPI_ERROR;
    }

    coll_request->buffer_info[SBUF].lkey = coll_request->buffer_info[SBUF].iboffload_reg->mr->lkey;

    /* it is estimated mq consumption... */
    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
                 iboffload_module, coll_fragment->mq_index, coll_fragment->mq_credits) ||
                 false == opal_list_is_empty(&iboffload_module->collfrag_pending))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));
        goto out_of_resources;
    }

    coll_fragment->tail_next = &coll_fragment->to_post;

    if (coll_request->root == my_group_index) {
        IBOFFLOAD_VERBOSE(10, ("I'm root of the data %d %d",
                    iboffload_module->power_of_2, recursive_doubling_tree->n_extra_sources ));
        /* for proxy we have little bit more work to do */
        if (recursive_doubling_tree->n_extra_sources > 0) {
            /* send the all data to your extra peer */
            dst = recursive_doubling_tree->rank_extra_source;
            rc = mca_bcol_iboffload_recv_rtr_setup(
                    &last_wait, dst, iboffload_module, coll_fragment);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to"
                            " mca_bcol_iboffload_recv_rtr_setup"));
                return OMPI_ERROR;
            }
            rc = mca_bcol_iboffload_send_large_buff_setup(
                    &last_send, SBUF, 0, count, dst,
                    iboffload_module, coll_fragment);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                IBOFFLOAD_VERBOSE(10, ("Failed to"
                            " mca_bcol_iboffload_send_large_buff_setup"));
                return OMPI_ERROR;
            }
        }
        power_of_2_distance = iboffload_module->power_of_2;

        BINOMIAL_SCATTER(iboffload_module, coll_fragment,
                last_wait, last_send,  power_of_2_distance - 1,
                my_group_index, base_block_size, count
                );
        /* EXIT OR GO TO Gather */
        goto GATHER;
    }

    /* prepare and post recv operation */
    group_src = bcol_iboffload_binomial_root_to_src(coll_request->root,
            my_group_index, iboffload_module->power_of_2_ranks,
            iboffload_module->group_size, &power_of_2_distance);

    IBOFFLOAD_VERBOSE(10, ("SRC %d DIST %d ranks %d gsize %d root %d my rank %d",
                group_src, power_of_2_distance, iboffload_module->power_of_2_ranks,
                iboffload_module->group_size,
                coll_request->root, my_group_index));
    assert(group_src >= 0);

    if (0 > power_of_2_distance) {
        /* the rank is virtual root for this group, receive the data
           and scatter gather as root */
        power_of_2_distance =
            iboffload_module->power_of_2;
        offset = 0;
        recv_count = count;
        IBOFFLOAD_VERBOSE(10, ("Virtual root %d , set mask to %d",
                    my_group_index, power_of_2_distance));
    } else {
        int my_left_boundary_rank;
        int delta;
        recv_count = base_block_size * (1 << power_of_2_distance); /* we may receive larger data */
        my_left_boundary_rank = my_group_index & ((~(int)0) << power_of_2_distance );
        offset = (size_t) (base_block_size * my_left_boundary_rank);
        delta = count - offset;
        if (OPAL_UNLIKELY(delta <= 0)) {
            /* no data to recv */
            goto GATHER;
        } else {
            recv_count = (delta < recv_count) ? delta : recv_count;
        }

        IBOFFLOAD_VERBOSE(10, ("Recv data set mask to %d",
                    power_of_2_distance));
    }

    IBOFFLOAD_VERBOSE(10, ("Bcast, receive data from %d[%d], count %d, offset %d",
                group_src, recv_count, offset));

    /* Receive data to user buffer */
    rc = mca_bcol_iboffload_send_rtr_setup(&last_send,
                                group_src, iboffload_module,
                                coll_fragment);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to setup send rtr"));
        return OMPI_ERROR;
    }

    rc = mca_bcol_iboffload_recv_large_buff_setup(&last_wait,
                                SBUF, offset, recv_count, group_src,
                                iboffload_module, coll_fragment);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to setup data receive"));
        return OMPI_ERROR;
    }

    BINOMIAL_SCATTER(iboffload_module, coll_fragment,
            last_wait, last_send, power_of_2_distance - 1,
            my_group_index, base_block_size, count);

GATHER:
    rc = bcol_iboffload_bcast_binomial_gather(iboffload_module,
            &last_send, &last_wait, coll_fragment,
            count, base_block_size, power_of_2_distance);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to setup gather. Return %d", rc));
        return rc;
    }

    if (recursive_doubling_tree->n_extra_sources > 0 &&
            iboffload_module->power_of_2 != power_of_2_distance) {
        dst = recursive_doubling_tree->rank_extra_source;

        rc = mca_bcol_iboffload_recv_rtr_setup(
                &last_wait, dst, iboffload_module, coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to"
                        " mca_bcol_iboffload_recv_rtr_setup"));
            return OMPI_ERROR;
        }

        rc = mca_bcol_iboffload_send_large_buff_setup(
                &last_send, SBUF, 0, count, dst,
                iboffload_module, coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to"
                        " mca_bcol_iboffload_send_large_buff_setup"));
            return OMPI_ERROR;
        }
    }

    IBOFFLOAD_VERBOSE(10, ("Fill in the the rest of the coll_fragment.\n"));

    /* end of list */
    *coll_fragment->tail_next = NULL;

    /* finish initializing full message descriptor */
    coll_request->n_fragments  += 1;
    coll_request->n_frags_sent += 1;

    if (NULL != last_wait) {
        last_wait->flags |= MQE_WR_FLAG_SIGNAL;
        coll_fragment->signal_task_wr_id = last_wait->wr_id;
        last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;
    } else {
        last_send->flags |= MQE_WR_FLAG_SIGNAL;
        coll_fragment->signal_task_wr_id = last_send->wr_id;
        last_send->wr_id = (uint64_t) (uintptr_t) coll_fragment;
    }

    /* post the mwr */
    rc = mca_bcol_iboffload_post_mqe_tasks(iboffload_module, coll_fragment->to_post);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload_module->super, coll_request->order_info);

    IBOFFLOAD_VERBOSE(10, ("Return success.\n"));

    return BCOL_FN_STARTED;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Barrier, adding collfrag to collfrag_pending.\n"));
    rc =
        mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload_module);
    return (OMPI_SUCCESS != rc) ? BCOL_FN_NOT_STARTED : BCOL_FN_STARTED;
}

int mca_bcol_iboffload_bcast_scatter_allgather_intra(bcol_function_args_t *fn_arguments,
                                                   struct coll_ml_function_t *const_args)
{
    mca_bcol_iboffload_module_t *iboffload_module =
        (mca_bcol_iboffload_module_t *) const_args->bcol_module;

    int rc;
    int mq_credits = iboffload_module->power_of_2 * 3  + 4;
    bool if_bcol_last = BCOL_IBOFFLOAD_IS_LAST_CALL(const_args);
    mca_bcol_iboffload_collreq_t *coll_request;

    MCA_BCOL_CHECK_ORDER(const_args->bcol_module, fn_arguments);

    rc = mca_bcol_iboffload_bcast_init(fn_arguments, iboffload_module,
            &coll_request, if_bcol_last, mq_credits,
            mca_bcol_iboffload_bcast_scatter_allgather_exec);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    rc = coll_request->progress_fn(iboffload_module, coll_request);

    IBOFFLOAD_VERBOSE(10, ("mca_bcol_iboffload_bcast_scatter_allgather_intra was started [%d]\n", rc));
    return rc;
}

static int mca_bcol_iboffload_bcast_scatter_allgather_extra_exec(mca_bcol_iboffload_module_t *iboffload_module,
        mca_bcol_iboffload_collreq_t *coll_request)
{
    netpatterns_pair_exchange_node_t *recursive_doubling_tree =
        &iboffload_module->recursive_doubling_tree;

    int rc, dst;
    int count = coll_request->count * coll_request->dtype->super.size;
    int my_group_index = iboffload_module->ibnet->super.my_index;
    struct mqe_task *last_send = NULL,
                    *last_wait = NULL;
    mca_bcol_iboffload_collfrag_t *coll_fragment = &coll_request->first_collfrag;

    if (OPAL_UNLIKELY(!iboffload_module->connection_status[RECURSIVE_DOUBLING_TREE_BCAST])) {
        bcol_iboffload_setup_binomial_connection(iboffload_module);
    }

    /* register memory in mpool/rcache */
    rc = mca_bcol_iboffload_prepare_buffer(coll_request->buffer_info[SBUF].buf, count,
            &coll_request->buffer_info[SBUF].iboffload_reg, iboffload_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("Cannot register memory: "
                         "addr - %p, %d bytes.\n",
                          coll_request->buffer_info[SBUF].buf, count));
        return OMPI_ERROR;
    }

    coll_request->buffer_info[SBUF].lkey = coll_request->buffer_info[SBUF].iboffload_reg->mr->lkey;

    /* it is estimated mq consumption... */
    if (OPAL_UNLIKELY(false == BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(
                 iboffload_module, coll_fragment->mq_index, coll_fragment->mq_credits) ||
                 false == opal_list_is_empty(&iboffload_module->collfrag_pending))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));
        goto out_of_resources;
    }

    coll_fragment->tail_next = &coll_fragment->to_post;

    /* send or recv the data */

    if (coll_request->root == my_group_index) {
        IBOFFLOAD_VERBOSE(10, ("I'm root of the data %d", iboffload_module->power_of_2));
        /* send the all data to your extra peer */
        dst = recursive_doubling_tree->rank_extra_source;
        rc = mca_bcol_iboffload_recv_rtr_setup(
                &last_wait, dst, iboffload_module, coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to"
                        " mca_bcol_iboffload_recv_rtr_setup"));
            return OMPI_ERROR;
        }
        rc = mca_bcol_iboffload_send_large_buff_setup(
                &last_send, SBUF, 0, count, dst,
                iboffload_module, coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to"
                        " mca_bcol_iboffload_send_large_buff_setup"));
            return OMPI_ERROR;
        }
    } else {
        /* Not root case */
        dst = recursive_doubling_tree->rank_extra_source;
        rc = mca_bcol_iboffload_send_rtr_setup(&last_send,
                dst, iboffload_module,
                coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to setup send rtr"));
            return OMPI_ERROR;
        }

        rc = mca_bcol_iboffload_recv_large_buff_setup(&last_wait,
                SBUF, 0, count, dst,
                iboffload_module, coll_fragment);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to setup data receive"));
            return OMPI_ERROR;
        }
    }

    IBOFFLOAD_VERBOSE(10, ("Fill in the the rest of the coll_fragment.\n"));

    /* end of list */
    *coll_fragment->tail_next = NULL;

    /* finish initializing full message descriptor */
    coll_request->n_fragments  += 1;
    coll_request->n_frags_sent += 1;

    if (NULL != last_wait) {
        last_wait->flags |= MQE_WR_FLAG_SIGNAL;
        coll_fragment->signal_task_wr_id = last_wait->wr_id;
        last_wait->wr_id = (uint64_t) (uintptr_t) coll_fragment;
    } else {
        last_send->flags |= MQE_WR_FLAG_SIGNAL;
        coll_fragment->signal_task_wr_id = last_send->wr_id;
        last_send->wr_id = (uint64_t) (uintptr_t) coll_fragment;
    }

    /* post the mwr */
    rc = mca_bcol_iboffload_post_mqe_tasks(iboffload_module, coll_fragment->to_post);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    MCA_BCOL_UPDATE_ORDER_COUNTER(&iboffload_module->super, coll_request->order_info);

    IBOFFLOAD_VERBOSE(10, ("Return success.\n"));

    return BCOL_FN_STARTED;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("Barrier, adding collfrag to collfrag_pending.\n"));
    rc =
        mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, iboffload_module);
    return (OMPI_SUCCESS != rc) ? BCOL_FN_NOT_STARTED : BCOL_FN_STARTED;
}

int mca_bcol_iboffload_bcast_scatter_allgather_extra_intra(bcol_function_args_t *fn_arguments,
                                                   struct coll_ml_function_t *const_args)
{
    mca_bcol_iboffload_module_t *iboffload_module =
        (mca_bcol_iboffload_module_t *) const_args->bcol_module;

    int rc;
    int mq_credits = iboffload_module->power_of_2 * 3  + 4;
    bool if_bcol_last = BCOL_IBOFFLOAD_IS_LAST_CALL(const_args);
    mca_bcol_iboffload_collreq_t *coll_request;

    MCA_BCOL_CHECK_ORDER(const_args->bcol_module, fn_arguments);

    rc = mca_bcol_iboffload_bcast_init(fn_arguments, iboffload_module,
            &coll_request, if_bcol_last, mq_credits,
            mca_bcol_iboffload_bcast_scatter_allgather_extra_exec);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    rc = coll_request->progress_fn(iboffload_module, coll_request);

    IBOFFLOAD_VERBOSE(10, ("mca_bcol_iboffload_bcast_scatter_allgather_extra_intra was started [%d]\n", rc));
    return rc;
}

int mca_bcol_iboffload_bcast_register(mca_bcol_base_module_t *super)
{
    mca_bcol_iboffload_module_t *iboffload_module =
                            (mca_bcol_iboffload_module_t *) super;

    int my_group_index = iboffload_module->ibnet->super.my_index;

    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    IBOFFLOAD_VERBOSE(10, ("Register iboffload Bcast.\n"));

    comm_attribs.bcoll_type = BCOL_BCAST;

    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 1024 * 1024;
    comm_attribs.waiting_semantics = NON_BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000; /* range 1 */

    inv_attribs.datatype_bitmap = 0xffffffff;
    inv_attribs.op_types_bitmap = 0xffffffff;

    comm_attribs.data_src = DATA_SRC_KNOWN;

    if (my_group_index < iboffload_module->power_of_2_ranks) {
        mca_bcol_base_set_attributes(super,
            &comm_attribs, &inv_attribs,
            mca_bcol_iboffload_small_msg_bcast_intra,
            mca_bcol_iboffload_small_msg_bcast_progress);

        inv_attribs.bcol_msg_min = 10000000;
        inv_attribs.bcol_msg_max = 10485760; /* range 4 */

        mca_bcol_base_set_attributes(super,
                &comm_attribs, &inv_attribs,
                mca_bcol_iboffload_bcast_scatter_allgather_intra,
                mca_bcol_iboffload_zero_copy_progress);

    } else {
        mca_bcol_base_set_attributes(super,
            &comm_attribs, &inv_attribs,
            mca_bcol_iboffload_small_msg_bcast_extra_intra,
            mca_bcol_iboffload_small_msg_bcast_progress);

        inv_attribs.bcol_msg_min = 10000000;
        inv_attribs.bcol_msg_max = 10485760; /* range 4 */

        mca_bcol_base_set_attributes(super,
                &comm_attribs, &inv_attribs,
                mca_bcol_iboffload_bcast_scatter_allgather_extra_intra,
                mca_bcol_iboffload_zero_copy_progress);

    }

    return OMPI_SUCCESS;
}
