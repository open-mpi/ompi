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
 * A user-provided partitioning of n partitions of size b can be mapped
 * to an internal partitioning of n/k partitions of size k*b,
 * where k can be selected to optimize internal partition size.
 */

#ifndef AGGREGATION_SCHEME_REGULAR_H
#define AGGREGATION_SCHEME_REGULAR_H

#include "ompi_config.h"

#include "opal/include/opal/sys/atomic.h"


/**
 * @brief tracks the number of pready calls corresponding to internal partitions
 *
 */
struct part_persist_aggregation_state {
    // counters for each internal partition
    opal_atomic_uint32_t *public_parts_ready;

    int internal_partition_count;

    // parameters for message aggregation
    int factor; // how many public partitions may be aggregated into an internal one

    int public_partition_count;
    int last_internal_partition_size; // number of public partitions corresponding to last internal
                                      // one
};

/**
 * @brief initializes the aggregation state for the sending side
 *
 * @param[out] state                        pointer to aggregation state object
 * @param[in] internal_partition_count      number of internal partitions (i.e. number of messages
 * per partitioned transfer)
 * @param[in] factor                        number of public partitions corresponding to each internal one other than the last
 * @param[in] last_internal_partition_size  number of public partitions corresponding to last
 * internal partition
 */
void aggregation_scheme_regular_psend_init(struct part_persist_aggregation_state *state,
                                           int internal_partition_count,
                                           int factor,
                                           int last_internal_partition_size);

/**
 * @brief resets the aggregation state
 *
 * @param[out] state                pointer to aggregation state object
 */
void aggregation_scheme_regular_reset(struct part_persist_aggregation_state *state);

/**
 * @brief marks a public partition as ready
 *
 * @param[in,out] state             pointer to aggregation state object
 * @param[in] partition             index of the public partition to mark ready
 * @param[out] available_partition  index of the internal partition if it is ready, otherwise -1
 */
void aggregation_scheme_regular_pready(struct part_persist_aggregation_state *state,
                                       int partition, int* available_partition);

/**
 * @brief 
 *
 * @param[in,out] state             pointer to aggregation state object
 * @param[in] partition             index of the public partition
 * @return the internal partition number corresponding to the given 
 */
int aggregation_scheme_regular_internal_part(struct part_persist_aggregation_state *state,
                                             int partition);

/**
 * @brief destroys the aggregation scheme
 *
 * @param[in,out] state             pointer to aggregation state object
 */
void aggregation_scheme_regular_free(struct part_persist_aggregation_state *state);

#endif
