/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Copyright (c) 2026       Hewlett Packard Enterprise Development LP. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hook_hwpc_cxi_constants.h"

static int failures = 0;

#define EXPECT_TRUE(EXPR, MSG)                                                   \
    do {                                                                         \
        if (!(EXPR)) {                                                           \
            fprintf(stderr, "FAIL: %s (%s:%d)\n", MSG, __FILE__, __LINE__);   \
            failures++;                                                          \
        }                                                                        \
    } while (0)

#define EXPECT_EQ_INT(ACTUAL, EXPECTED, MSG)                                     \
    do {                                                                         \
        int _actual = (ACTUAL);                                                  \
        int _expected = (EXPECTED);                                              \
        if (_actual != _expected) {                                              \
            fprintf(stderr,                                                      \
                    "FAIL: %s (%s:%d): got %d expected %d\n",                 \
                    MSG, __FILE__, __LINE__, _actual, _expected);                \
            failures++;                                                          \
        }                                                                        \
    } while (0)

static void test_group_lookup(void)
{
    const hwpc_cxi_predefined_counter_group_obj_t *group_obj = NULL;
    hwpc_cxi_predefined_counter_group_id_t group_id;
    int counter_count = 0;

    EXPECT_EQ_INT(hwpc_cxi_get_counter_group_obj_by_name(&group_obj, "CxiPerfStats"),
                  HWPC_CXI_SUCCESS,
                  "lookup by pretty group name should succeed");
    EXPECT_TRUE(NULL != group_obj, "group object should be returned");
    EXPECT_TRUE(NULL != group_obj->counter_group_name, "group name should be populated");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_group_id_by_obj(&group_id, group_obj),
                  HWPC_CXI_SUCCESS,
                  "lookup by group object should succeed");
    EXPECT_EQ_INT(group_id, CXI_PERFSTATS,
                  "group object should map back to its id");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_group_obj_by_name(&group_obj, "cxi_perfstats"),
                  HWPC_CXI_SUCCESS,
                  "lookup by enum-like group name should be case-insensitive");

    EXPECT_EQ_INT(hwpc_cxi_get_num_counters_in_counter_group_by_name(&counter_count, "CxiPerfStats"),
                  HWPC_CXI_SUCCESS,
                  "counter count by group should succeed");
    EXPECT_TRUE(counter_count > 0, "group should contain at least one counter");
}

static void test_mnemonic_lookup(void)
{
    const hwpc_cxi_predefined_counter_mnemonic_obj_t *mnemonic_obj = NULL;
    hwpc_cxi_predefined_counter_mnemonic_id_t mnemonic_id;
    int counter_count = 0;

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_by_id(&mnemonic_obj, ATU_CACHE_MISS),
                  HWPC_CXI_SUCCESS,
                  "lookup by mnemonic id should succeed");
    EXPECT_TRUE(NULL != mnemonic_obj, "mnemonic object should be returned");
    EXPECT_TRUE(NULL != mnemonic_obj->counter_name, "mnemonic name should be populated");

    EXPECT_EQ_INT(strcmp(mnemonic_obj->counter_name, "ATU_CACHE_MISS"),
                  0,
                  "mnemonic name should match expected id");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_id_by_obj(&mnemonic_id, mnemonic_obj),
                  HWPC_CXI_SUCCESS,
                  "lookup by mnemonic object should succeed");
    EXPECT_EQ_INT(mnemonic_id, ATU_CACHE_MISS,
                  "mnemonic object should map back to its id");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_by_name(&mnemonic_obj, "atu_cache_miss"),
                  HWPC_CXI_SUCCESS,
                  "lookup by mnemonic name should be case-insensitive");

    EXPECT_EQ_INT(hwpc_cxi_get_num_counters_in_counter_mnemonic_by_name(&counter_count, "ATU_CACHE_MISS"),
                  HWPC_CXI_SUCCESS,
                  "counter count by mnemonic should succeed");
    EXPECT_EQ_INT(counter_count, 4, "ATU_CACHE_MISS should expose 4 categories");
}

static void test_mnemonic_object_lookup_errors(void)
{
    const hwpc_cxi_predefined_counter_mnemonic_obj_t *mnemonic_obj = NULL;
    hwpc_cxi_predefined_counter_mnemonic_id_t mnemonic_id = ATU_CACHE_MISS;
    hwpc_cxi_predefined_counter_mnemonic_obj_t mnemonic_copy;

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_id_by_obj(NULL, mnemonic_obj),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "NULL mnemonic id output should be rejected");
    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_id_by_obj(&mnemonic_id, NULL),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "NULL mnemonic object should be rejected");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_by_id(&mnemonic_obj, ATU_CACHE_MISS),
                  HWPC_CXI_SUCCESS,
                  "mnemonic object setup should succeed");
    memcpy(&mnemonic_copy, mnemonic_obj, sizeof(mnemonic_copy));
    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_id_by_obj(&mnemonic_id, &mnemonic_copy),
                  HWPC_CXI_COUNTER_MNEMONIC_NOT_FOUND,
                  "a copy is not a predefined mnemonic object");
    EXPECT_EQ_INT(mnemonic_id, HWPC_CXI_NUM_PREDEFINED_COUNTER_MNEMONICS,
                  "failed object lookup should return the invalid sentinel id");
}

static void test_lowlevel_counter_lookup(void)
{
    const hwpc_cxi_predefined_counter_mnemonic_obj_t *mnemonic_obj = NULL;

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_for_lowlevel_counter_name(&mnemonic_obj,
                                                                    "atu_cache_evictions"),
                  HWPC_CXI_SUCCESS,
                  "standalone low-level counter should resolve");
    EXPECT_TRUE(NULL != mnemonic_obj, "standalone counter should return a mnemonic");
    EXPECT_EQ_INT(strcmp(mnemonic_obj->counter_name, "ATU_CACHE_EVICTIONS"), 0,
                  "standalone counter should resolve to the expected mnemonic");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_for_lowlevel_counter_name(&mnemonic_obj,
                                                                    "atu_cache_miss_3"),
                  HWPC_CXI_SUCCESS,
                  "categorized low-level counter should resolve");
    EXPECT_EQ_INT(strcmp(mnemonic_obj->counter_name, "ATU_CACHE_MISS"), 0,
                  "categorized counter should resolve to its base mnemonic");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_for_lowlevel_counter_name(&mnemonic_obj,
                                                                    "rh:sct_timeouts"),
                  HWPC_CXI_SUCCESS,
                  "retry-handler low-level counter should resolve");
    EXPECT_EQ_INT(strcmp(mnemonic_obj->counter_name, "SCT_TIMEOUTS"), 0,
                  "retry-handler counter should resolve to its mnemonic");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_for_lowlevel_counter_name(&mnemonic_obj,
                                                                    "atu_cache_miss_4"),
                  HWPC_CXI_COUNTER_MNEMONIC_NOT_FOUND,
                  "out-of-range category should not resolve");
    EXPECT_TRUE(NULL == mnemonic_obj, "failed lookup should clear the mnemonic output");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_for_lowlevel_counter_name(&mnemonic_obj,
                                                                    "atu_cache_miss_bad"),
                  HWPC_CXI_COUNTER_MNEMONIC_NOT_FOUND,
                  "non-numeric category should not resolve");
    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_for_lowlevel_counter_name(&mnemonic_obj,
                                                                    "sct_timeouts"),
                  HWPC_CXI_COUNTER_MNEMONIC_NOT_FOUND,
                  "retry-handler counter without rh prefix should not resolve");
}

static void test_missing_entries(void)
{
    const hwpc_cxi_predefined_counter_group_obj_t *group_obj = NULL;
    const hwpc_cxi_predefined_counter_mnemonic_obj_t *mnemonic_obj = NULL;
    int counter_count = 123;

    EXPECT_EQ_INT(hwpc_cxi_get_counter_group_obj_by_name(&group_obj, "NOT_A_GROUP"),
                  HWPC_CXI_COUNTER_GROUP_NOT_FOUND,
                  "unknown group should return not-found");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_by_name(&mnemonic_obj, "NOT_A_MNEMONIC"),
                  HWPC_CXI_COUNTER_MNEMONIC_NOT_FOUND,
                  "unknown mnemonic should return not-found");

    EXPECT_EQ_INT(hwpc_cxi_get_num_counters_in_counter_group_by_name(&counter_count, "NOT_A_GROUP"),
                  HWPC_CXI_COUNTER_GROUP_NOT_FOUND,
                  "unknown group count should return not-found");
    EXPECT_EQ_INT(counter_count, 0, "unknown group count should be reset to 0");

    counter_count = 123;
    EXPECT_EQ_INT(hwpc_cxi_get_num_counters_in_counter_mnemonic_by_name(&counter_count, "NOT_A_MNEMONIC"),
                  HWPC_CXI_COUNTER_MNEMONIC_NOT_FOUND,
                  "unknown mnemonic count should return not-found");
    EXPECT_EQ_INT(counter_count, 0, "unknown mnemonic count should be reset to 0");
}

int main(void)
{
    test_group_lookup();
    test_mnemonic_lookup();
    test_mnemonic_object_lookup_errors();
    test_lowlevel_counter_lookup();
    test_missing_entries();

    if (0 != failures) {
        fprintf(stderr, "hwpc_cxi_constants_lookup_test: %d failure(s)\n", failures);
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
