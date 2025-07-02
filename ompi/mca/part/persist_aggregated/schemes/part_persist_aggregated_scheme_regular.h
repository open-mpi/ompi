/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024      High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

/**
 * @file
 * This file defines a simple message aggregation scheme:
 * A user-provided partitioning into n partitions is mapped
 * to an internal partitioning of ceil(n/k) partitions where
 * each internal partition corresponds to k public ones
 * (with the last partition potentially having a lower size).
 * The factor k can be defined to optimize the internal
 * number/size of internal partitions.
 */

#ifndef PART_PERSIST_AGGREGATED_SCHEME_REGULAR_H
#define PART_PERSIST_AGGREGATED_SCHEME_REGULAR_H

#include "ompi_config.h"

#include "opal/include/opal/sys/atomic.h"

/**
 * @brief tracks the number of pready calls corresponding to internal partitions
 *
 */
struct part_persist_aggregation_state {
    // counters for each internal partition
    opal_atomic_int32_t *public_parts_ready;

    // number of public partitions
    int public_partition_count;

    // number of internal partitions
    int internal_partition_count;

    // how many public partitions are aggregated into an internal one
    int aggregation_count;
    // number of public partitions corresponding to last internal partition
    int last_internal_partition_size;
};

/**
 * @brief initializes the aggregation state
 *
 * @param[out] state                        pointer to aggregation state object
 * @param[in] internal_partition_count      number of internal partitions (i.e. number of messages
 *                                          per partitioned transfer)
 * @param[in] factor                        number of public partitions corresponding to each
 *                                          internal one other than the last
 * @param[in] last_internal_partition_size  number of public partitions corresponding to the last
 *                                          internal partition internal partition
 */
OMPI_DECLSPEC void part_persist_aggregate_regular_init(struct part_persist_aggregation_state *state,
                                                       int internal_partition_count, int factor,
                                                       int last_internal_partition_size);

/**
 * @brief resets the aggregation state
 *
 * @param[out] state                pointer to aggregation state object
 */
OMPI_DECLSPEC void
part_persist_aggregate_regular_reset(struct part_persist_aggregation_state *state);

/**
 * @brief marks a public partition as ready and optionally outputs an internal partition that can be
 * sent.
 *
 * @param[in,out] state             pointer to aggregation state object
 * @param[in] partition             index of the public partition to mark ready
 * @param[out] available_partition  index of the corresponding internal partition if it is ready,
 *                                  otherwise -1
 */
OMPI_DECLSPEC void
part_persist_aggregate_regular_pready(struct part_persist_aggregation_state *state, int partition,
                                      int *available_partition);

/**
 * @brief destroys the aggregation scheme
 *
 * @param[in,out] state             pointer to aggregation state object
 */
OMPI_DECLSPEC void
part_persist_aggregate_regular_free(struct part_persist_aggregation_state *state);

#endif