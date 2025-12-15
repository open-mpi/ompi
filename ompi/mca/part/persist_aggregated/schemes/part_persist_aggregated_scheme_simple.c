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

#include "part_persist_aggregated_scheme_simple.h"

#include <stdlib.h>
#include <string.h>

// converts the index of a public partition to the index of its corresponding internal partition
static int internal_partition(struct part_persist_aggregation_state *state, int public_part)
{
    return public_part / state->aggregation_count;
}

void part_persist_aggregate_simple_init(struct part_persist_aggregation_state *state,
                                        int internal_partition_count, int factor, int last_internal_partition_size)
{
    state->public_partition_count = (internal_partition_count - 1) * factor + last_internal_partition_size;
    state->internal_partition_count = internal_partition_count;

    // number of user-partitions per internal partition (except for the last one)
    state->aggregation_count = factor;
    // number of user-partitions corresponding to the last internal partition
    state->last_internal_partition_size = last_internal_partition_size;

    // initialize counters
    state->public_parts_ready = (opal_atomic_uint32_t *) calloc(state->internal_partition_count,
                                                                 sizeof(opal_atomic_uint32_t));
}

void part_persist_aggregate_simple_reset(struct part_persist_aggregation_state *state)
{
    // reset flags
    if (NULL != state->public_parts_ready) {
        memset((void*)state->public_parts_ready, 0, state->internal_partition_count * sizeof(opal_atomic_uint32_t));
    }
}

static inline int is_last_partition(struct part_persist_aggregation_state *state, int partition) {
    return (partition == state->internal_partition_count - 1);
}

static inline int num_public_parts(struct part_persist_aggregation_state *state, int partition) {
    return is_last_partition(state, partition) ? state->last_internal_partition_size : state->aggregation_count;
}

void part_persist_aggregate_simple_pready(struct part_persist_aggregation_state *state, int partition, int* available_partition)
{
    int internal_part = internal_partition(state, partition);

    // this is the new value (after adding)
    int count = opal_atomic_add_fetch_32(&state->public_parts_ready[internal_part], 1);

    // push to buffer if internal partition is ready
    if (count == num_public_parts(state, internal_part)) {
        *available_partition = internal_part;
    } else {
        *available_partition = -1;
    }
}

void part_persist_aggregate_simple_free(struct part_persist_aggregation_state *state)
{
    if (state->public_parts_ready != NULL)
        free((void*)state->public_parts_ready);
    state->public_parts_ready = NULL;
}
