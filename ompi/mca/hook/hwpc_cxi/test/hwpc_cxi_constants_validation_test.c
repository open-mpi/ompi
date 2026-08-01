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

static void test_argument_validation(void)
{
    const hwpc_cxi_predefined_counter_group_obj_t *group_obj = NULL;
    const hwpc_cxi_predefined_counter_mnemonic_obj_t *mnemonic_obj = NULL;
    int counter_count = 0;

    EXPECT_EQ_INT(hwpc_cxi_get_counter_group_obj_by_name(NULL, "CxiPerfStats"),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "group lookup should reject NULL output pointer");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_group_obj_by_name(&group_obj, NULL),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "group lookup should reject NULL name");

    EXPECT_EQ_INT(hwpc_cxi_get_num_counters_in_counter_group_by_name(NULL, "CxiPerfStats"),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "group count should reject NULL output pointer");

    EXPECT_EQ_INT(hwpc_cxi_get_num_counters_in_counter_group_by_name(&counter_count, NULL),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "group count should reject NULL name");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_by_id(NULL, ATU_CACHE_MISS),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "mnemonic-by-id lookup should reject NULL output pointer");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_by_id(&mnemonic_obj, HWPC_CXI_NUM_PREDEFINED_COUNTER_MNEMONICS),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "mnemonic-by-id lookup should reject invalid id");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_by_name(NULL, "ATU_CACHE_MISS"),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "mnemonic-by-name lookup should reject NULL output pointer");

    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_by_name(&mnemonic_obj, NULL),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "mnemonic-by-name lookup should reject NULL name");

    EXPECT_EQ_INT(hwpc_cxi_get_num_counters_in_counter_mnemonic_by_name(NULL, "ATU_CACHE_MISS"),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "mnemonic count should reject NULL output pointer");

    EXPECT_EQ_INT(hwpc_cxi_get_num_counters_in_counter_mnemonic_by_name(&counter_count, NULL),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "mnemonic count should reject NULL name");
}

static void test_string_helpers(void)
{
    EXPECT_TRUE(0 == strcmp(hwpc_cxi_error_to_string(HWPC_CXI_SUCCESS), "Success"),
                "error_to_string should map success");
    EXPECT_TRUE(0 == strcmp(hwpc_cxi_error_to_string(9999), "Unknown error code"),
                "error_to_string should map unknown values");

    EXPECT_TRUE(0 == strcmp(hwpc_cxi_counter_type_to_string(HWPC_CXI_COUNTER_GROUP_TYPE), "Counter Group"),
                "counter_type_to_string should map group type");
    EXPECT_TRUE(0 == strcmp(hwpc_cxi_counter_type_to_string((hwpc_cxi_counter_type_t) 9999), "Unknown counter type"),
                "counter_type_to_string should map unknown values");
}

static void test_print_helpers(void)
{
    const hwpc_cxi_predefined_counter_group_obj_t *group_obj = NULL;
    const hwpc_cxi_predefined_counter_mnemonic_obj_t *mnemonic_obj = NULL;
    FILE *tmp = NULL;

    EXPECT_EQ_INT(hwpc_cxi_get_counter_group_obj_by_name(&group_obj, "CxiPerfStats"),
                  HWPC_CXI_SUCCESS,
                  "group lookup for print helpers should succeed");
    EXPECT_EQ_INT(hwpc_cxi_get_counter_mnemonic_obj_by_name(&mnemonic_obj, "ATU_CACHE_MISS"),
                  HWPC_CXI_SUCCESS,
                  "mnemonic lookup for print helpers should succeed");

    tmp = tmpfile();
    EXPECT_TRUE(NULL != tmp, "tmpfile should be created for print tests");
    if (NULL == tmp) {
        return;
    }

    EXPECT_EQ_INT(hwpc_cxi_print_counter_group_description(tmp, group_obj),
                  HWPC_CXI_SUCCESS,
                  "group print helper should succeed");
    EXPECT_EQ_INT(hwpc_cxi_print_counter_mnemonic_description(tmp, mnemonic_obj),
                  HWPC_CXI_SUCCESS,
                  "mnemonic print helper should succeed");
    EXPECT_EQ_INT(hwpc_cxi_print_full_counter_group_description(tmp, group_obj),
                  HWPC_CXI_SUCCESS,
                  "full group print helper should succeed");

    fclose(tmp);

    EXPECT_EQ_INT(hwpc_cxi_print_counter_group_description(NULL, group_obj),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "group print helper should reject NULL file");
    EXPECT_EQ_INT(hwpc_cxi_print_counter_mnemonic_description(NULL, mnemonic_obj),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "mnemonic print helper should reject NULL file");
    EXPECT_EQ_INT(hwpc_cxi_print_full_counter_group_description(NULL, group_obj),
                  HWPC_CXI_ERROR_INVALID_ARGUMENTS,
                  "full group print helper should reject NULL file");
}

int main(void)
{
    test_argument_validation();
    test_string_helpers();
    test_print_helpers();

    if (0 != failures) {
        fprintf(stderr, "hwpc_cxi_constants_validation_test: %d failure(s)\n", failures);
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
