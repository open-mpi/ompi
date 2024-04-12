/*
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "src/include/pmix_config.h"

#include <stdlib.h>
#include <stdio.h>

#include "src/include/pmix_globals.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_argv.h"

int main(int argc, char *argv[])
{
    char **test_array;
    char **new_array;
    const char *tmp;
    pmix_status_t ret;
    int ret_int;
    PMIX_HIDE_UNUSED_PARAMS(argc, argv);

    /*
     * mess with environ
     */

    /* first test: does overwrite work? */
    ret = PMIx_Setenv("pmix_environ_test_string_3", "three", true, &environ);
    if (ret != PMIX_SUCCESS) {
        printf("PMIx_Setenv returned %d\n", ret);
        return 1;
    }
    ret = PMIx_Setenv("pmix_environ_test_string_3", "four", false, &environ);
    if (ret != PMIX_SUCCESS) {
        printf("PMIx_Setenv returned %d\n", ret);
        return 1;
    }
    if (strcmp(getenv("pmix_environ_test_string_3"), "three") != 0) {
        printf("PMIx_Setenv overwrote.  Found %s expected %s\n",
               getenv("pmix_environ_test_string_3"), "three");
        return 1;
    }
    ret = PMIx_Setenv("pmix_environ_test_string_3", "four", true, &environ);
    if (ret != PMIX_SUCCESS) {
        printf("PMIx_Setenv returned %d\n", ret);
        return 1;
    }
    tmp = getenv("pmix_environ_test_string_3");
    if (NULL == tmp) {
        printf("could not getenv(\"pmix_environ_test_string_3\")\n");
        return 1;
    }
    if (strcmp(tmp, "four") != 0) {
        printf("PMIx_Setenv overwrote.  Found %s expected %s\n",
               tmp, "four");
        return 1;
    }

    /* second test: do we handle environ properly in unsetenv */
    ret_int = putenv("pmix_environ_test_string_1=one");
    if (ret_int != 0) {
        printf("putenv() returned %d\n", ret_int);
        return 1;
    }
    ret_int = setenv("pmix_environ_test_string_2", "two", true);
    if (ret_int != 0) {
        printf("setenv() returned %d\n", ret_int);
        return 1;
    }
    ret = PMIx_Setenv("pmix_environ_test_string_3", "three", true, &environ);
    if (ret != PMIX_SUCCESS) {
        printf("PMIx_Setenv returned %d\n", ret);
        return 1;
    }
    ret = pmix_unsetenv("pmix_environ_test_string_1", &environ);
    if (ret != PMIX_SUCCESS) {
        printf("pmix_unsetenv returned %d\n", ret);
        return 1;
    }
    ret = pmix_unsetenv("pmix_environ_test_string_2", &environ);
    if (ret != PMIX_SUCCESS) {
        printf("pmix_unsetenv returned %d\n", ret);
        return 1;
    }
    ret = pmix_unsetenv("pmix_environ_test_string_3", &environ);
    if (ret != PMIX_SUCCESS) {
        printf("pmix_unsetenv returned %d\n", ret);
        return 1;
    }

    /* third test: can we merge environ into a new env.  Note that the
     * new env is empty, so we can compare environ and newenv later
     * and they should match.  We'll test the actual merging later
     * without environ as the additions, so we have more control over
     * the situation */
    ret_int = putenv("pmix_environ_test_string_1=one");
    if (ret_int != 0) {
        printf("putenv() returned %d\n", ret_int);
        return 1;
    }
    ret_int = setenv("pmix_environ_test_string_2", "two", true);
    if (ret_int != 0) {
        printf("setenv() returned %d\n", ret_int);
        return 1;
    }
    ret = PMIx_Setenv("pmix_environ_test_string_3", "three", true, &environ);
    if (ret != PMIX_SUCCESS) {
        printf("PMIx_Setenv returned %d\n", ret);
        return 1;
    }
    test_array = NULL;
    ret = pmix_environ_merge_inplace(&test_array, environ);
    if (ret != PMIX_SUCCESS) {
        printf("pmix_environ_merge returned %d\n", ret);
        return 1;
    }
    PMIx_Argv_free(test_array);

    /* fourth test: make sure environ_merge_inplace does. */
    test_array = NULL;
    PMIx_Argv_append_nosize(&test_array, "pmix_environ_test_string_1=old_one");
    PMIx_Argv_append_nosize(&test_array, "pmix_environ_test_string_2=old_two");
    PMIx_Argv_append_nosize(&test_array, "pmix_environ_test_string_3=old_three");
    PMIx_Argv_append_nosize(&test_array, "pmix_environ_test_string_4=old_four");

    new_array = NULL;
    PMIx_Argv_append_nosize(&new_array, "pmix_environ_test_string_1=new_one");
    PMIx_Argv_append_nosize(&new_array, "pmix_environ_test_string_5=new_five");

    ret = pmix_environ_merge_inplace(&test_array, new_array);
    if (ret != PMIX_SUCCESS) {
        printf("pmix_environ_merge returned %d\n", ret);
        return 1;
    }

    if (NULL == pmix_getenv("pmix_environ_test_string_1", test_array)) {
        printf("new env missing string 1\n");
        return 1;
    }
    if (NULL == pmix_getenv("pmix_environ_test_string_2", test_array)) {
        printf("new env missing string 2\n");
        return 1;
    }
    if (NULL == pmix_getenv("pmix_environ_test_string_3", test_array)) {
        printf("new env missing string 3\n");
        return 1;
    }
    if (NULL == pmix_getenv("pmix_environ_test_string_4", test_array)) {
        printf("new env missing string 4\n");
        return 1;
    }
    if (NULL == pmix_getenv("pmix_environ_test_string_5", test_array)) {
        printf("new env missing string 5\n");
        return 1;
    }

    tmp = pmix_getenv("pmix_environ_test_string_1", test_array);
    if (NULL == tmp) {
        printf("could not getenv(\"pmix_environ_test_string_1\")\n");
        return 1;
    }
    if (strcmp("old_one", tmp) != 0) {
        printf("wrong value.  expected %s, found %s\n",
               "old_one", tmp);
        return 1;
    }
    tmp = pmix_getenv("pmix_environ_test_string_2", test_array);
    if (NULL == tmp) {
        printf("could not getenv(\"pmix_environ_test_string_2\")\n");
        return 1;
    }
    if (strcmp("old_two", tmp) != 0) {
        printf("wrong value.  expected %s, found %s\n",
               "old_two", tmp);
        return 1;
    }
    tmp = pmix_getenv("pmix_environ_test_string_3", test_array);
    if (NULL == tmp) {
        printf("could not getenv(\"pmix_environ_test_string_3\")\n");
        return 1;
    }
    if (strcmp("old_three", tmp) != 0) {
        printf("wrong value.  expected %s, found %s\n",
               "old_three", tmp);
        return 1;
    }
    tmp = pmix_getenv("pmix_environ_test_string_4", test_array);
    if (NULL == tmp) {
        printf("could not getenv(\"pmix_environ_test_string_4\")\n");
        return 1;
    }
    if (strcmp("old_four", tmp) != 0) {
        printf("wrong value.  expected %s, found %s\n",
               "old_four", tmp);
        return 1;
    }
    tmp = pmix_getenv("pmix_environ_test_string_5", test_array);
    if (NULL == tmp) {
        printf("could not getenv(\"pmix_environ_test_string_5\")\n");
        return 1;
    }
    if (strcmp("new_five", tmp) != 0) {
        printf("wrong value.  expected %s, found %s\n",
               "new_five", tmp);
        return 1;
    }

    if (5 != PMIx_Argv_count(test_array)) {
        printf("test array has wrong length.  found %d, expected %d\n",
               PMIx_Argv_count(test_array), 5);
        return 1;
    }

    PMIx_Argv_free(test_array);

    return 0;
}
