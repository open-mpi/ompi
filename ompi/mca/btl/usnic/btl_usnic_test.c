/*
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>

#include "opal/class/opal_list.h"

#include "btl_usnic.h"
#include "btl_usnic_test.h"

int ompi_btl_usnic_num_tests_run = 0;
int ompi_btl_usnic_num_tests_passed = 0;
int ompi_btl_usnic_num_tests_failed = 0;
int ompi_btl_usnic_num_tests_skipped = 0;

struct test_info {
    opal_list_item_t li;
    char *name;
    ompi_btl_usnic_test_fn_t test_fn;
    void *ctx;
};

#if OMPI_BTL_USNIC_UNIT_TESTS
static bool initialized = false;
static opal_list_t all_tests;

void ompi_btl_usnic_cleanup_tests(void)
{
    opal_list_item_t *li;
    struct test_info *info;

    if (initialized) {
        while (NULL != (li = opal_list_remove_first(&all_tests))) {
            info = container_of(li, struct test_info, li);
            free(info);
        }
        OBJ_DESTRUCT(&all_tests);
    }
    initialized = false;
}

static void init_test_infra(void)
{
    if (!initialized) {
        OBJ_CONSTRUCT(&all_tests, opal_list_t);
        initialized = true;
    }
}

void ompi_btl_usnic_register_test(const char *name,
                                  ompi_btl_usnic_test_fn_t test_fn,
                                  void *ctx)
{
    struct test_info *info = malloc(sizeof(*info));
    assert(info != NULL);

    OBJ_CONSTRUCT(&info->li, opal_list_item_t);

    init_test_infra();

    info->name = strdup(name);
    info->test_fn = test_fn;
    info->ctx = ctx;

    opal_list_append(&all_tests, &info->li);
}

void ompi_btl_usnic_run_tests(void)
{
    struct test_info *info;
    enum test_result result;

    if (!OMPI_BTL_USNIC_UNIT_TESTS) {
        test_out("unit tests disabled in this build, doing nothing!\n");
        return;
    }
    test_out("STARTING TESTS\n");

    OPAL_LIST_FOREACH(info, &all_tests, struct test_info) {
        test_out("running test '%s'... ", info->name);
        result = info->test_fn(info->ctx);

        ++ompi_btl_usnic_num_tests_run;
        switch (result) {
            case TEST_PASSED:
                ++ompi_btl_usnic_num_tests_passed;
                test_out("PASSED\n");
                break;
            case TEST_FAILED:
                ++ompi_btl_usnic_num_tests_failed;
                test_out("FAILED\n");
                break;
            case TEST_SKIPPED:
                ++ompi_btl_usnic_num_tests_skipped;
                test_out("SKIPPED\n");
                break;
        }
    }

    test_out("FINISHED TESTS (%d passed, %d failed, %d skipped)\n",
             ompi_btl_usnic_num_tests_passed,
             ompi_btl_usnic_num_tests_failed,
             ompi_btl_usnic_num_tests_skipped);
}

#else /* !OMPI_BTL_USNIC_UNIT_TESTS */

void ompi_btl_usnic_register_test(const char *name,
                                  ompi_btl_usnic_test_fn_t test_fn,
                                  void *ctx)
{
    abort(); /* never should be called */
}

void ompi_btl_usnic_run_tests(void)
{
    test_out("unit tests disabled in this build, doing nothing!\n");
    return;
}

#endif /* !OMPI_BTL_USNIC_UNIT_TESTS */
