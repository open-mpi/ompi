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

#include "ompi_config.h"

#include "ompi/include/ompi/constants.h"
#include "bcol_ptpcoll.h"
#include "bcol_ptpcoll_utils.h"

/*
 * Fanin routines - no user data
 */

/********************************************* New Barrier *********************************************/
/*******************************************************************************************************/
/*******************************************************************************************************/

/*************************************** K-nominal ***************************************/
/*****************************************************************************************/
static int bcol_ptpcoll_barrier_recurs_knomial_new(
                bcol_function_args_t *input_args,
                struct mca_bcol_base_function_t *const_args)
{
    /* local variable */
    uint64_t sequence_number;
    mca_bcol_ptpcoll_module_t *ptpcoll_module =
                        (mca_bcol_ptpcoll_module_t *) const_args->bcol_module;

    netpatterns_k_exchange_node_t *my_exchange_node =
                                       &ptpcoll_module->knomial_exchange_tree;

    int rc, k, pair_comm_rank, exchange, completed,
        tree_order = my_exchange_node->tree_order, tag,
        n_extra_sources = my_exchange_node->n_extra_sources,
        n_exchange = my_exchange_node->n_exchanges, num_reqs;

    ompi_communicator_t *comm =
            ptpcoll_module->super.sbgp_partner_module->group_comm;

    int *extra_sources_array = NULL,
        **rank_exchanges = my_exchange_node->rank_exchanges;

    ompi_request_t **requests;
    ompi_free_list_item_t *item;

    mca_bcol_ptpcoll_collreq_t *collreq;

    OMPI_FREE_LIST_WAIT_MT(&ptpcoll_module->collreqs_free, item);
    if (OPAL_UNLIKELY(NULL == item)) {
        PTPCOLL_ERROR(("Free list waiting failed."));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    collreq = (mca_bcol_ptpcoll_collreq_t *) item;
    input_args->bcol_opaque_data = (void *) collreq;

    requests = collreq->requests;

    /* TAG Calculation */
    sequence_number = input_args->sequence_num;

    /* Keep tag within the limit supportd by the pml */
    tag = (PTPCOLL_TAG_OFFSET + sequence_number * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);

    /* Mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;

    if (0 < n_extra_sources) { /* EXCHANGE_NODE case */
        collreq->need_toserv_extra = 1;
        extra_sources_array = my_exchange_node->rank_extra_sources_array;

        /* I will participate in the exchange (of the algorithm) -
         * wait for signal from extra process */
        for (k = 0; k < n_extra_sources; ++k) {
            pair_comm_rank =
                    ptpcoll_module->super.sbgp_partner_module->group_list[extra_sources_array[k]];

            rc = MCA_PML_CALL(irecv(
                        NULL, 0, MPI_INT,
                        pair_comm_rank, tag,
                        comm, &(requests[k])));
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                PTPCOLL_ERROR(("IRecv failed."));
                return rc;
            }
        }

        num_reqs = n_extra_sources;

        /* Test for completion */
        completed =
            mca_bcol_ptpcoll_test_all_for_match(&num_reqs, requests, &rc);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("Test for all failed."));
            return rc;
        }

        if (!completed) {
            collreq->tag = tag;
            collreq->num_reqs = num_reqs;
            collreq->exchange = 0;

            return BCOL_FN_STARTED;
        }
    } else {
        collreq->need_toserv_extra = 0;
    }

    /* loop over exchange send/recv pairs */
    for (exchange = 0; exchange < n_exchange; ++exchange) {
        for (k = 0; k < tree_order - 1; ++k) {
            /* rank of exchange partner within the group */
            pair_comm_rank =
                ptpcoll_module->super.sbgp_partner_module->group_list[rank_exchanges[exchange][k]];

            assert(2 * ptpcoll_module->k_nomial_radix > (k * 2 + 1));

            /* send to partner - we will wait for completion, as send
             *   completion is at the MPI level, and will not
             *   incur network level completion costs
             */
            rc = MCA_PML_CALL(isend(
                        NULL, 0, MPI_INT,
                        pair_comm_rank, tag,
                        MCA_PML_BASE_SEND_STANDARD,
                        comm, &(requests[k * 2 + 1])));
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                PTPCOLL_ERROR(("ISend failed."));
                return rc;
            }

            PTPCOLL_VERBOSE(10, ("Ex %d, K %d send to %d[%d]", exchange, k,
                                  pair_comm_rank, rank_exchanges[exchange][k]));

            /* recive from partner */
            rc = MCA_PML_CALL(irecv(
                        NULL, 0, MPI_INT,
                        pair_comm_rank, tag,
                        comm, &(requests[k * 2])));
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                PTPCOLL_ERROR(("IRecv failed."));
                return rc;
            }

            PTPCOLL_VERBOSE(10, ("Ex %d, K %d irecv from %d[%d]", exchange, k,
                                  pair_comm_rank, rank_exchanges[exchange][k]));
        }

        num_reqs = 2 * (tree_order - 1);

        /* Test for completion */
        completed =
            mca_bcol_ptpcoll_test_all_for_match(&num_reqs, requests, &rc);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("Test for all failed."));
            return rc;
        }

        if (!completed) {
            collreq->tag = tag;
            collreq->num_reqs = num_reqs;
            collreq->exchange = exchange + 1;

            return BCOL_FN_STARTED;
        }
    }

    /* If non power of 2, may need to send message to "extra" proc */
    if (0 < n_extra_sources)  {  /* EXCHANGE_NODE case */
        for (k = 0; k < n_extra_sources; ++k) {
            pair_comm_rank =
                ptpcoll_module->super.sbgp_partner_module->group_list[extra_sources_array[k]];

            rc = MCA_PML_CALL(isend(
                        NULL, 0, MPI_INT,
                        pair_comm_rank, tag,
                        MCA_PML_BASE_SEND_STANDARD,
                        comm, &(requests[k])));
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                PTPCOLL_ERROR(("ISend failed."));
                return rc;
            }
        }

        num_reqs = n_extra_sources;

        /* Test for completion */
        completed =
            mca_bcol_ptpcoll_test_all_for_match(&num_reqs, requests, &rc);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("Test for all failed."));
            return rc;
        }

        if (!completed) {
            collreq->tag = tag;
            collreq->num_reqs = num_reqs;

            collreq->exchange = n_exchange;
            collreq->need_toserv_extra = 0;

            return BCOL_FN_STARTED;
        }
    }

    OMPI_FREE_LIST_RETURN_MT(&ptpcoll_module->collreqs_free, (ompi_free_list_item_t *) collreq);
    return BCOL_FN_COMPLETE;
}

static int bcol_ptpcoll_barrier_recurs_knomial_new_progress(
                                bcol_function_args_t *input_args,
                                struct mca_bcol_base_function_t *const_args)
{
    /* local variable */
    mca_bcol_ptpcoll_module_t *ptpcoll_module =
                        (mca_bcol_ptpcoll_module_t *) const_args->bcol_module;

    netpatterns_k_exchange_node_t *my_exchange_node =
                                       &ptpcoll_module->knomial_exchange_tree;

    int rc, k, tag, pair_comm_rank, exchange,
        tree_order = my_exchange_node->tree_order, num_reqs,
        n_exchange = my_exchange_node->n_exchanges, completed,
        n_extra_sources = my_exchange_node->n_extra_sources;

    ompi_communicator_t *comm =
            ptpcoll_module->super.sbgp_partner_module->group_comm;

    int *extra_sources_array,
        **rank_exchanges = my_exchange_node->rank_exchanges;

    mca_bcol_ptpcoll_collreq_t *collreq =
                    (mca_bcol_ptpcoll_collreq_t *) input_args->bcol_opaque_data;

    ompi_request_t **requests = collreq->requests;

    num_reqs = collreq->num_reqs;

    /* Test for completion */
    completed =
        mca_bcol_ptpcoll_test_all_for_match(&num_reqs, requests, &rc);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        PTPCOLL_ERROR(("Test for all failed."));
        return rc;
    }

    if (!completed) {
        return BCOL_FN_STARTED;
    }

    /* Continue loop over exchange send/recv pairs */
    tag = collreq->tag;

    for (exchange = collreq->exchange; exchange < n_exchange; ++exchange) {
        for (k = 0; k < tree_order - 1; ++k) {
            /* rank of exchange partner within the group */
            pair_comm_rank =
                ptpcoll_module->super.sbgp_partner_module->group_list[rank_exchanges[exchange][k]];

            assert(2 * ptpcoll_module->k_nomial_radix > (k * 2 + 1));

            /* send to partner - we will wait for completion, as send
             *   completion is at the MPI level, and will not
             *   incur network level completion costs
             */
            rc = MCA_PML_CALL(isend(
                        NULL, 0, MPI_INT,
                        pair_comm_rank, tag,
                        MCA_PML_BASE_SEND_STANDARD,
                        comm, &(requests[k * 2 + 1])));
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                PTPCOLL_ERROR(("ISend failed."));
                return rc;
            }

            PTPCOLL_VERBOSE(10, ("Ex %d, K %d send to %d[%d]", exchange, k,
                                  pair_comm_rank, rank_exchanges[exchange][k]));

            /* recive from partner */
            rc = MCA_PML_CALL(irecv(
                        NULL, 0, MPI_INT,
                        pair_comm_rank, tag,
                        comm, &(requests[k * 2])));
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                PTPCOLL_ERROR(("IRecv failed."));
                return rc;
            }

            PTPCOLL_VERBOSE(10, ("Ex %d, K %d irecv from %d[%d]", exchange, k,
                                  pair_comm_rank, rank_exchanges[exchange][k]));
        }

        num_reqs = 2 * (tree_order - 1);

        /* Test for completion */
        completed =
            mca_bcol_ptpcoll_test_all_for_match(&num_reqs, requests, &rc);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("Test for all failed."));
            return rc;
        }

        if (!completed) {
            collreq->num_reqs = num_reqs;
            collreq->exchange = exchange + 1;

            return BCOL_FN_STARTED;
        }
    }

    /* If non power of 2, may need to send message to "extra" proc */
    if (collreq->need_toserv_extra)  {  /* EXCHANGE_NODE case */
        extra_sources_array = my_exchange_node->rank_extra_sources_array;

        for (k = 0; k < n_extra_sources; ++k) {
            pair_comm_rank =
                ptpcoll_module->super.sbgp_partner_module->group_list[extra_sources_array[k]];

            rc = MCA_PML_CALL(isend(
                        NULL, 0, MPI_INT,
                        pair_comm_rank, tag,
                        MCA_PML_BASE_SEND_STANDARD,
                        comm, &(requests[k])));
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                PTPCOLL_ERROR(("ISend failed."));
                return rc;
            }
        }

        num_reqs = n_extra_sources;

        /* Test for completion */
        completed =
            mca_bcol_ptpcoll_test_all_for_match(&num_reqs, requests, &rc);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("Test for all failed."));
            return rc;
        }

        if (!completed) {
            collreq->num_reqs = num_reqs;
            collreq->exchange = n_exchange;
            collreq->need_toserv_extra = 0;

            return BCOL_FN_STARTED;
        }
    }

    return BCOL_FN_COMPLETE;
}

/****************************************** Extra node Barrier ******************************************/

static int bcol_ptpcoll_barrier_recurs_knomial_extra_new(
                                bcol_function_args_t *input_args,
                                struct mca_bcol_base_function_t *const_args)
{
    /* local variable */
    uint64_t sequence_number;
    int rc, tag, pair_comm_rank,
        completed, num_reqs = 2;

    mca_bcol_ptpcoll_module_t *ptpcoll_module =
                    (mca_bcol_ptpcoll_module_t *) const_args->bcol_module;

    netpatterns_k_exchange_node_t *my_exchange_node =
                                   &ptpcoll_module->knomial_exchange_tree;

    ompi_communicator_t *comm =
                    ptpcoll_module->super.sbgp_partner_module->group_comm;

    int *extra_sources_array = my_exchange_node->rank_extra_sources_array;

    ompi_request_t **requests;
    ompi_free_list_item_t *item;

    mca_bcol_ptpcoll_collreq_t *collreq;

    OMPI_FREE_LIST_WAIT_MT(&ptpcoll_module->collreqs_free, item);
    if (OPAL_UNLIKELY(NULL == item)) {
        PTPCOLL_ERROR(("Free list waiting failed."));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    collreq = (mca_bcol_ptpcoll_collreq_t *) item;
    input_args->bcol_opaque_data = (void *) collreq;

    requests = collreq->requests;

    /* TAG Calculation */
    sequence_number = input_args->sequence_num;

    /* Keep tag within the limit supportd by the pml */
    tag = (PTPCOLL_TAG_OFFSET + sequence_number * PTPCOLL_TAG_FACTOR) & (ptpcoll_module->tag_mask);

    /* Mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;

    pair_comm_rank =
            ptpcoll_module->super.sbgp_partner_module->group_list[extra_sources_array[0]];

    rc = MCA_PML_CALL(isend(
                NULL, 0, MPI_INT,
                pair_comm_rank, tag,
                MCA_PML_BASE_SEND_STANDARD,
                comm, &(requests[0])));
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        PTPCOLL_ERROR(("ISend failed."));
        return rc;
    }

    rc = MCA_PML_CALL(irecv(
                NULL, 0, MPI_INT,
                pair_comm_rank, tag,
                comm, &(requests[1])));
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        PTPCOLL_ERROR(("IRecv failed."));
        return rc;
    }

    /* Test for completion */
    completed =
        mca_bcol_ptpcoll_test_all_for_match(&num_reqs, requests, &rc);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        PTPCOLL_ERROR(("Test for all failed."));
        return rc;
    }

    if (!completed) {
        return BCOL_FN_STARTED;
    }

    OMPI_FREE_LIST_RETURN_MT(&ptpcoll_module->collreqs_free, (ompi_free_list_item_t *) collreq);
    return BCOL_FN_COMPLETE;
}

/*************************************** Recursive-Doubling ***************************************/
/**************************************************************************************************/

static int bcol_ptpcoll_barrier_recurs_dbl_new(
                                bcol_function_args_t *input_args,
                                struct mca_bcol_base_function_t *const_args)
{
   /* local variable */
    uint64_t sequence_number;
    mca_bcol_ptpcoll_module_t *ptp_module =
                         (mca_bcol_ptpcoll_module_t *) const_args->bcol_module;

    ompi_communicator_t *comm = ptp_module->super.sbgp_partner_module->group_comm;

    int rc, my_extra_partner_comm_rank = 0, exchange, completed,
        pair_comm_rank, pair_rank, delta, tag, num_reqs = 0,
        my_rank = ptp_module->super.sbgp_partner_module->my_index,
        n_exchange = ptp_module->super.sbgp_partner_module->n_levels_pow2;

    ompi_request_t **requests;
    ompi_free_list_item_t *item;

    mca_bcol_ptpcoll_collreq_t *collreq;

    OMPI_FREE_LIST_WAIT_MT(&ptp_module->collreqs_free, item);
    if (OPAL_UNLIKELY(NULL == item)) {
        PTPCOLL_ERROR(("Free list waiting failed."));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    collreq = (mca_bcol_ptpcoll_collreq_t *) item;
    input_args->bcol_opaque_data = (void *) collreq;

    assert(PTPCOLL_EXTRA != ptp_module->pow_2type);

    requests = collreq->requests;

    /* TAG Calculation */
    sequence_number = input_args->sequence_num;

    /* keep tag within the limit supportd by the pml */
    tag = (PTPCOLL_TAG_OFFSET + sequence_number * PTPCOLL_TAG_FACTOR) & (ptp_module->tag_mask);

    /* mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;

    if (PTPCOLL_PROXY == ptp_module->pow_2type) {
        /* I will participate in the exchange - wait for signal from extra
         ** process */
        /*
         * recv from extra rank - my_extra_partner_comm_rank
         *  can use blocking recv, as no other communications
         *  need to take place.
         */
        my_extra_partner_comm_rank =
                       ptp_module->super.sbgp_partner_module->group_list[ptp_module->proxy_extra_index];

        collreq->need_toserv_extra = 1;
        collreq->extra_partner_rank = my_extra_partner_comm_rank;

        rc = MCA_PML_CALL(irecv(NULL, 0, MPI_INT,
                    my_extra_partner_comm_rank, tag, comm,
                    &(requests[0])));
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("IRecv failed."));
            return rc;
        }

        completed = mca_bcol_ptpcoll_test_for_match(&requests[0], &rc);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("Test for irecv failed."));
            return rc;
        }

        if (!completed) {
            collreq->tag = tag;
            collreq->num_reqs = 1;
            collreq->exchange = 0;

            return BCOL_FN_STARTED;
        }
    } else {
        collreq->need_toserv_extra = 0;
    }

    /* Loop over exchange send/recv pairs */
    delta = 1;
    for (exchange = 0; exchange < n_exchange; ++exchange) {

        /* rank of exchange partner within the group */
        pair_rank = my_rank ^ delta;

        /* rank within the communicator */
        pair_comm_rank =
            ptp_module->super.sbgp_partner_module->group_list[pair_rank];

        /* send to partner - we will wait for completion, as send
         *   completion is at the MPI level, and will not
         *   incur network level completion costs
         */
        rc = MCA_PML_CALL(isend(NULL, 0, MPI_INT,
                    pair_comm_rank, tag,
                    MCA_PML_BASE_SEND_STANDARD, comm,
                    &(requests[0])));
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("ISend failed."));
            return rc;
        }

        ++num_reqs;

        /* recive from partner */
        rc = MCA_PML_CALL(irecv(NULL, 0, MPI_INT,
                    pair_comm_rank, tag, comm,
                    &(requests[1])));
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("IRecv failed."));
            return rc;
        }

        ++num_reqs;

        PTPCOLL_VERBOSE(5, ("exchange - %d, pair_rank - %d, pair_comm_rank - %d",
                             exchange, pair_rank, pair_comm_rank));

        /* test for completion */
        completed =
            mca_bcol_ptpcoll_test_all_for_match(&num_reqs, requests, &rc);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("Test for all failed."));
            return rc;
        }

        if (!completed) {
            collreq->tag = tag;
            collreq->num_reqs = num_reqs;

            collreq->exchange = exchange + 1;
            assert(collreq->exchange >= 0);

            return BCOL_FN_STARTED;
        }

        delta <<= 1; /* delta *= 2 */
    }

    if (PTPCOLL_PROXY == ptp_module->pow_2type) {
        /* send - let the extra rank know that we are done */
        rc = MCA_PML_CALL(isend(NULL, 0, MPI_INT,
                    my_extra_partner_comm_rank, tag,
                    MCA_PML_BASE_SEND_STANDARD, comm,
                    &(requests[0])));
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("ISend failed."));
            return rc;
        }

        completed = mca_bcol_ptpcoll_test_for_match(&requests[0], &rc);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("Test for isend failed."));
            return rc;
        }

        if (!completed) {
            collreq->tag = tag;
            collreq->num_reqs = 1;

            collreq->need_toserv_extra = 0;
            collreq->exchange = n_exchange;

            return BCOL_FN_STARTED;
        }
    }

    OMPI_FREE_LIST_RETURN_MT(&ptp_module->collreqs_free, (ompi_free_list_item_t *) collreq);
    return BCOL_FN_COMPLETE;
}

static int bcol_ptpcoll_barrier_recurs_dbl_new_progress(
                                bcol_function_args_t *input_args,
                                struct mca_bcol_base_function_t *const_args)
{
   /* local variable */
    mca_bcol_ptpcoll_module_t *ptp_module =
                         (mca_bcol_ptpcoll_module_t *) const_args->bcol_module;

    ompi_communicator_t *comm = ptp_module->super.sbgp_partner_module->group_comm;

    int rc, exchange, pair_comm_rank, tag,
        pair_rank, delta, num_reqs, completed,
        my_rank = ptp_module->super.sbgp_partner_module->my_index,
        n_exchange = ptp_module->super.sbgp_partner_module->n_levels_pow2;

    ompi_request_t **requests;
    mca_bcol_ptpcoll_collreq_t *collreq =
                    (mca_bcol_ptpcoll_collreq_t *) input_args->bcol_opaque_data;

    num_reqs = collreq->num_reqs;
    requests = collreq->requests;

    /* test for completion */
    completed =
        mca_bcol_ptpcoll_test_all_for_match(&num_reqs, requests, &rc);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        PTPCOLL_ERROR(("Test for all failed."));
        return rc;
    }

    if (!completed) {
          return BCOL_FN_STARTED;
    }

    assert(PTPCOLL_EXTRA != ptp_module->pow_2type);

    /* Continue loop over exchange send/recv pairs */
    num_reqs = 0;
    tag = collreq->tag;

    exchange = collreq->exchange;
    assert(exchange >= 0);

    delta = 1 << exchange;
    for (; exchange < n_exchange; ++exchange) {

        /* rank of exchange partner within the group */
        pair_rank = my_rank ^ delta;

        /* rank within the communicator */
        pair_comm_rank =
            ptp_module->super.sbgp_partner_module->group_list[pair_rank];

        /* send to partner - we will wait for completion, as send
         *   completion is at the MPI level, and will not
         *   incur network level completion costs
         */
        rc = MCA_PML_CALL(isend(NULL, 0, MPI_INT,
                    pair_comm_rank, tag,
                    MCA_PML_BASE_SEND_STANDARD, comm,
                    &(requests[0])));
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("ISend failed."));
            return rc;
        }

        ++num_reqs;

        /* recive from partner */
        rc = MCA_PML_CALL(irecv(NULL, 0, MPI_INT,
                    pair_comm_rank, tag, comm,
                    &(requests[1])));
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("IRecv failed."));
            return rc;
        }

        ++num_reqs;

        PTPCOLL_VERBOSE(5, ("exchange - %d, pair_rank - %d, pair_comm_rank - %d",
                             exchange, pair_rank, pair_comm_rank));

        /* test for completion */
        completed =
            mca_bcol_ptpcoll_test_all_for_match(&num_reqs, requests, &rc);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("Test for all failed."));
            return rc;
        }

        if (!completed) {
            collreq->num_reqs = num_reqs;
            collreq->exchange = exchange + 1;
            assert(collreq->exchange >= 0);

            return BCOL_FN_STARTED;
        }

        delta <<= 1; /* delta *= 2 */
    }

    /* if non power of 2, may need to send message to "extra" proc */
    if (collreq->need_toserv_extra) {
        /* send - let the extra rank know that we are done */
        rc = MCA_PML_CALL(isend(NULL, 0, MPI_INT,
                    collreq->extra_partner_rank, tag,
                    MCA_PML_BASE_SEND_STANDARD, comm,
                    &(requests[0])));
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("ISend failed."));
            return rc;
        }

        completed = mca_bcol_ptpcoll_test_for_match(&requests[0], &rc);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            PTPCOLL_ERROR(("Test for isend failed."));
            return rc;
        }

        if (!completed) {
            collreq->num_reqs = 1;
            collreq->need_toserv_extra = 0;
            collreq->exchange = n_exchange;

            return BCOL_FN_STARTED;
        }
    }

    return BCOL_FN_COMPLETE;
}

/****************************************** Extra node Barrier ******************************************/

static int bcol_ptpcoll_barrier_recurs_dbl_extra_new(
                                bcol_function_args_t *input_args,
                                struct mca_bcol_base_function_t *const_args)
{
   /* local variable */
    uint64_t sequence_number;
    int rc, completed, num_reqs = 2,
        tag, my_extra_partner_comm_rank;

    ompi_request_t **requests;
    ompi_free_list_item_t *item;

    mca_bcol_ptpcoll_collreq_t *collreq;

    mca_bcol_ptpcoll_module_t *ptp_module =
                         (mca_bcol_ptpcoll_module_t *) const_args->bcol_module;
    ompi_communicator_t *comm = ptp_module->super.sbgp_partner_module->group_comm;

    OMPI_FREE_LIST_WAIT_MT(&ptp_module->collreqs_free, item);
    if (OPAL_UNLIKELY(NULL == item)) {
        PTPCOLL_ERROR(("Free list waiting failed."));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    collreq = (mca_bcol_ptpcoll_collreq_t *) item;
    input_args->bcol_opaque_data = (void *) collreq;

    requests = collreq->requests;

    /* TAG Calculation */
    sequence_number = input_args->sequence_num;

    /* Keep tag within the limit supportd by the pml */
    tag = (PTPCOLL_TAG_OFFSET + sequence_number * PTPCOLL_TAG_FACTOR) & (ptp_module->tag_mask);

    /* mark this as a collective tag, to avoid conflict with user-level flags */
    tag = -tag;

    /* I will not participate in the exchange - so just "register" as here,
     * signal the extra rank that I am here */

    my_extra_partner_comm_rank =
                 ptp_module->super.sbgp_partner_module->group_list[ptp_module->proxy_extra_index];

    rc = MCA_PML_CALL(isend(NULL, 0, MPI_INT,
                my_extra_partner_comm_rank, tag,
                MCA_PML_BASE_SEND_STANDARD, comm,
                &(requests[0])));
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        PTPCOLL_ERROR(("Send failed."));
        return rc;
    }

    /* Recv signal that the rest are done - my_extra_partner_comm_rank */
    rc = MCA_PML_CALL(irecv(NULL, 0, MPI_INT,
                my_extra_partner_comm_rank, tag, comm,
                &(requests[1])));
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        PTPCOLL_ERROR(("IRecv failed."));
        return rc;
    }

    /* Test for completion */
    completed =
        mca_bcol_ptpcoll_test_all_for_match(&num_reqs, requests, &rc);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        PTPCOLL_ERROR(("Test for all failed."));
        return rc;
    }

    if (!completed) {
        return BCOL_FN_STARTED;
    }

    OMPI_FREE_LIST_RETURN_MT(&ptp_module->collreqs_free, (ompi_free_list_item_t *) collreq);
    return BCOL_FN_COMPLETE;
}

/* We have the same progress func for both cases (R-D and K-Nominal) */
static int bcol_ptpcoll_barrier_extra_node_progress(
                            bcol_function_args_t *input_args,
                            struct mca_bcol_base_function_t *const_args)
{
   /* local variable */
    ompi_request_t **requests;
    int rc, completed, num_reqs = 2;

    mca_bcol_ptpcoll_collreq_t *collreq =
                    (mca_bcol_ptpcoll_collreq_t *) input_args->bcol_opaque_data;

    requests = collreq->requests;

    /* test for completion */
    completed =
        mca_bcol_ptpcoll_test_all_for_match(&num_reqs, requests, &rc);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        PTPCOLL_ERROR(("Test for all failed."));
        return rc;
    }

    if (!completed) {
        return BCOL_FN_STARTED;
    }

    return BCOL_FN_COMPLETE;
}

static int mca_bcol_ptpcoll_barrier_setup(mca_bcol_base_module_t *super, int bcoll_type)
{
    netpatterns_k_exchange_node_t *my_exchange_node;
    mca_bcol_ptpcoll_module_t * ptpcoll_module =
                           (mca_bcol_ptpcoll_module_t *) super;

    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    comm_attribs.bcoll_type = bcoll_type;

    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 1024 * 1024;
    comm_attribs.waiting_semantics = NON_BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000; /* range 1 */

    inv_attribs.datatype_bitmap = 0xffffffff;
    inv_attribs.op_types_bitmap = 0xffffffff;

    comm_attribs.data_src = DATA_SRC_KNOWN;

    switch(mca_bcol_ptpcoll_component.barrier_alg) {
        case 1:
            if (PTPCOLL_EXTRA == ptpcoll_module->pow_2type) {
                mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                    bcol_ptpcoll_barrier_recurs_dbl_extra_new,
                    bcol_ptpcoll_barrier_extra_node_progress);
                break;
            }

            mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                bcol_ptpcoll_barrier_recurs_dbl_new,
                bcol_ptpcoll_barrier_recurs_dbl_new_progress);
            break;
        case 2:
            my_exchange_node = &ptpcoll_module->knomial_exchange_tree;
            if (my_exchange_node->n_extra_sources > 0 &&
                           EXTRA_NODE == my_exchange_node->node_type) {
                mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                    bcol_ptpcoll_barrier_recurs_knomial_extra_new,
                    bcol_ptpcoll_barrier_extra_node_progress);
                break;
            }

            mca_bcol_base_set_attributes(super, &comm_attribs, &inv_attribs,
                bcol_ptpcoll_barrier_recurs_knomial_new,
                bcol_ptpcoll_barrier_recurs_knomial_new_progress);
            break;
        default:
            PTPCOLL_ERROR(("Wrong barrier_alg flag value."));
    }

    return OMPI_SUCCESS;
}

int mca_bcol_ptpcoll_memsync_init(mca_bcol_base_module_t *super)
{
    return mca_bcol_ptpcoll_barrier_setup(super, BCOL_SYNC);
}

int bcol_ptpcoll_barrier_init(mca_bcol_base_module_t *super)
{
    return mca_bcol_ptpcoll_barrier_setup(super, BCOL_BARRIER);
}
