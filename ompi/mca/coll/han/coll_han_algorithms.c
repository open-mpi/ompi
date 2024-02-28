/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020-2022 Bull S.A.S. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * @file
 * This files contains helps for algorithm selection
 *
 */

#include "coll_han.h"
#include "coll_han_algorithms.h"

/* default algo names, formatted for MCA var_enum_create */
mca_base_var_enum_value_t han_default_algorithms_enum[] = {
    { 0, "default" }, // algo #0 is called "defaut"
    { 0, NULL }
};

/**
 * all available algorithms must be added here to be selectable in
 * the component.
 *
 * note: algorithm 'default' / #0 is automatically added to each
 * implemented collective, it is the default behaviour in
 * coll_han_dynamic.c
 */
mca_coll_han_algorithm_value_t*  mca_coll_han_available_algorithms[COLLCOUNT] = {
    [BARRIER] = (mca_coll_han_algorithm_value_t[]) {
        {"simple", (fnptr_t) &mca_coll_han_barrier_intra_simple}, // 2-level
        { 0 }
    },
    [BCAST] = (mca_coll_han_algorithm_value_t[]){
        {"intra", (fnptr_t) &mca_coll_han_bcast_intra}, // 2-level
        {"simple", (fnptr_t) &mca_coll_han_bcast_intra_simple}, // 2-level
        { 0 }
    },
    [REDUCE] = (mca_coll_han_algorithm_value_t[]){
        {"intra", (fnptr_t) &mca_coll_han_reduce_intra}, // 2-level
        {"simple", (fnptr_t) &mca_coll_han_reduce_intra_simple}, // 2-level
        {"reproducible", (fnptr_t) &mca_coll_han_reduce_reproducible}, // fallback
        { 0 }
    },
    [ALLREDUCE] = (mca_coll_han_algorithm_value_t[]){
        {"intra", (fnptr_t) &mca_coll_han_allreduce_intra}, // 2-level
        {"simple", (fnptr_t) &mca_coll_han_allreduce_intra_simple}, // 2-level
        {"reproducible", (fnptr_t) &mca_coll_han_allreduce_reproducible}, // fallback
        { 0 }
    },
    [SCATTER] = (mca_coll_han_algorithm_value_t[]){
        {"intra", (fnptr_t) &mca_coll_han_scatter_intra}, // 2-level
        {"simple", (fnptr_t) &mca_coll_han_scatter_intra_simple}, // 2-level
        { 0 }
    },
    [SCATTERV] = (mca_coll_han_algorithm_value_t[]){
        {"intra", (fnptr_t) &mca_coll_han_scatterv_intra}, // 2-level
        { 0 }
    },
    [GATHER] = (mca_coll_han_algorithm_value_t[]){
        {"intra", (fnptr_t) &mca_coll_han_gather_intra}, // 2-level
        {"simple", (fnptr_t) &mca_coll_han_gather_intra_simple}, // 2-level
        { 0 }
    },
    [GATHERV] = (mca_coll_han_algorithm_value_t[]){
        {"intra", (fnptr_t) &mca_coll_han_gatherv_intra}, // 2-level
        { 0 }
    },
    [ALLGATHER] = (mca_coll_han_algorithm_value_t[]){
        {"intra", (fnptr_t)&mca_coll_han_allgather_intra}, // 2-level
        {"simple", (fnptr_t)&mca_coll_han_allgather_intra_simple}, // 2-level
        { 0 }
    },
};

int
mca_coll_han_algorithm_id_is_valid(int coll_id, int algorithm_id)
{
    if (!mca_coll_han_is_coll_dynamic_implemented(coll_id)) {
        return false;
    }
    if (0 > algorithm_id) {
        return false;
    }

    /*
     * user provided algorithms + 'default' algorithm #0
     */
    return algorithm_id < mca_coll_han_component.num_available_algorithms[coll_id] + 1;
}

fnptr_t
mca_coll_han_algorithm_id_to_fn(int coll_id, int algorithm_id)
{
    if (algorithm_id == 0 || !mca_coll_han_algorithm_id_is_valid(coll_id, algorithm_id)) {
        return NULL;
    }
    /* algorithm 0 is not included here */
    return mca_coll_han_available_algorithms[coll_id][algorithm_id - 1].fn;
}

char*
mca_coll_han_algorithm_id_to_name(int coll_id, int algorithm_id)
{
    if (!mca_coll_han_algorithm_id_is_valid(coll_id, algorithm_id)) {
        return NULL;
    }
    if (0 == algorithm_id) {
        return "default";
    }
    /* algorithm 0 is not included here */
    return mca_coll_han_available_algorithms[coll_id][algorithm_id - 1].name;
}

int
mca_coll_han_algorithm_name_to_id(COLLTYPE_T coll_id, const char* algorithm_name)
{
    // shortcut for default
    if (0 == strcmp(algorithm_name, "default")) {
        return 0;
    }
    if (0 > mca_coll_han_component.num_available_algorithms[coll_id]) {
        return -1;
    }

    const mca_base_var_enum_value_t* algorithms_values
                = mca_coll_han_component.algorithm_enumerator[coll_id];
    for (int i = 0; algorithms_values[i].string != NULL; i++) {
        if (0 == strcmp(algorithm_name, algorithms_values[i].string))
            return i;
    }
    return -1;
}

/**
 * count algorithms for this collective (other than default)
 */
static int
mca_han_algorithm_count(mca_coll_han_algorithm_value_t* algorithm_values)
{
    int n = 0;
    if (NULL != algorithm_values) {
        while (algorithm_values[n].name != NULL) {
            n++;
        }
    }
    return n;
}

/**
 * Initializes algorithm_enumerator, used to show algorithm id/name in ompi_info
 */
static int
mca_han_algorithm_enumerator_create(mca_base_var_enum_value_t** algorithm_enumerator,
                                    mca_coll_han_algorithm_value_t* algorithm_values)
{
    *algorithm_enumerator = NULL;

    int n_algorithm_values = mca_han_algorithm_count(algorithm_values);
    if (0 == n_algorithm_values) {
        return OMPI_SUCCESS;
    }

    // n_algorithm_values+2 because of 'default' and termination by 0
    mca_base_var_enum_value_t* enum_values =
        malloc((n_algorithm_values + 2) * sizeof(mca_base_var_enum_value_t));
    if (NULL == enum_values) {
        goto alloc_error;
    }

    // always add "default" algorithm
    enum_values[0].value = 0;
    enum_values[0].string = "default";
    // fill data for other algorithms
    for (int i = 0; i < n_algorithm_values; i++) {
        enum_values[i + 1].value = i + 1;
        enum_values[i + 1].string = algorithm_values[i].name;
    }
    // last value of enum must be zeroed
    enum_values[n_algorithm_values + 1] = (mca_base_var_enum_value_t){ 0 };
    *algorithm_enumerator = enum_values;
    return OMPI_SUCCESS;

alloc_error:
    opal_output(0, "coll/han failed to initialize available algorithms "
                   "(allocation error)");
    return OMPI_ERROR;
}


/**
 * initializes componnent algorithm_info
 */
int
mca_coll_han_init_algorithms(void)
{
    memset(mca_coll_han_component.num_available_algorithms, 0,
           COLLCOUNT * sizeof(int));
    memset(mca_coll_han_component.algorithm_enumerator, 0,
           COLLCOUNT * sizeof(mca_base_var_enum_value_t*));
    for (int coll=0; coll<COLLCOUNT; coll++) {
        mca_coll_han_component.num_available_algorithms[coll] =
            mca_han_algorithm_count(mca_coll_han_available_algorithms[coll]);
        if (0 == mca_coll_han_component.num_available_algorithms[coll]
            || !mca_coll_han_is_coll_dynamic_implemented(coll)) {
            continue;
        }
        if (OMPI_SUCCESS !=
            mca_han_algorithm_enumerator_create(&mca_coll_han_component.algorithm_enumerator[coll],
                                                mca_coll_han_available_algorithms[coll])) {
            goto init_failed;
        }
    }
    return OMPI_SUCCESS;
 init_failed:
    mca_coll_han_free_algorithms();
    return OMPI_ERROR;
}

int
mca_coll_han_free_algorithms(void)
{
    for (int coll_id = 0; coll_id < COLLCOUNT; coll_id++) {
        if (NULL != mca_coll_han_component.algorithm_enumerator[coll_id]) {
            free(mca_coll_han_component.algorithm_enumerator[coll_id]);
        }
    }
    return OPAL_SUCCESS;
}
