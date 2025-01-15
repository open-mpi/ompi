/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include <assert.h>

#include "opal/class/opal_include_list.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/util/argv.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"
#include "support.h"

/*
 * Data type used for testing
 */
typedef struct test_data {
    /* link list data structure */
    opal_list_item_t ll_element;
    /* test data */
    size_t data;
} test_data_t;

OBJ_CLASS_INSTANCE(test_data_t, opal_list_item_t, NULL, NULL);

#define TEST_ASSERT(cond, failure_string)       \
    if (!(cond)) {                              \
        test_failure(failure_string);           \
        return test_finalize();                 \
    }

#define TEST_EXPECT(cond, failure_string)       \
    if (cond) {                                 \
        test_pass();                            \
    } else {                                    \
        test_failure(failure_string);           \
    }

static void test_pass(void) {
    fprintf (stderr, "PASS\n");
    test_success();
}

static char *string_var;
static opal_include_list_t include_list_var;

static int test_string_variable(void)
{
    test_init("mca_var string");

    unsetenv("OMPI_MCA_test_component_test_string");

    fprintf (stderr, "Testing string MCA variables....\n");

    fprintf (stderr, "  Testing string variable creation: ");

    const char *kProjectName = "openmpi";
    const char *kFrameworkName = "test";
    const char *kComponentName = "component";
    const char *kTestVarName = "test_string";

    char *test_string1 = "test_string1";
    string_var = test_string1;
    int ret = mca_base_var_register(kProjectName, kFrameworkName, kComponentName, kTestVarName,
                                    "String variable for test", MCA_BASE_VAR_TYPE_STRING,
                                    /*enumerator=*/NULL, /*bind=*/0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_1, MCA_BASE_VAR_SCOPE_LOCAL, &string_var);
    TEST_ASSERT(ret >= 0, "could not create variable");

    int vari = mca_base_var_find(kProjectName, kFrameworkName, kComponentName, kTestVarName);
    TEST_ASSERT(vari >= 0, "variable register but not found");
    TEST_ASSERT(ret == vari, "mca_base_var_register did not return the correct variable index");

    test_pass();

    fprintf (stderr, "  Testing string variable storage: ");
    TEST_ASSERT(string_var != test_string1, "set value did not re-allocate string");
    ret = test_verify_str(test_string1, string_var);
    if (1 == ret) {
        test_pass();
    }

    fprintf (stderr, "  Testing mca_base_var_set_value: ");

    const char *kTestString2 = "test_string2";
    ret = mca_base_var_set_value(vari, kTestString2, /*size=*/0, MCA_BASE_VAR_SOURCE_SET,
                                 /*source_file=*/NULL);
    ret = test_verify_int(OPAL_SUCCESS, ret);
    if (ret == 1) {
        fprintf (stderr, "PASS\n");
    }

    fprintf (stderr, "  Retesting string variable storage: ");
    TEST_ASSERT(string_var != kTestString2, "set value did not re-allocate string");
    ret = test_verify_str(kTestString2, string_var);
    if (1 == ret) {
        fprintf (stderr, "PASS\n");
    }

    fprintf (stderr, "  Testing de-registration: ");
    ret = mca_base_var_deregister(vari);
    TEST_ASSERT(OPAL_SUCCESS == ret, "deregistration failed");
    fprintf (stderr, "PASS\n");
    TEST_ASSERT(string_var == NULL, "deregistration did not nullify the storage");

    return test_finalize();
}

static void check_include_list_contents(const char *check_str, const char *value, const opal_include_list_t *include_list)
{
    bool is_exclude = false;

    if ('^' == value[0]) {
        is_exclude = true;
        ++value;
    }

    fprintf (stderr, "    Checking %s list contents: ", check_str);
    char **list = opal_argv_split(value, ',');
    bool contents_match = true;
    for (int i = 0 ; list[i] != NULL ; ++i) {
        if ((NULL == include_list->items[i] || 0 != strcasecmp(list[i], include_list->items[i])) ||
            (list[i+1] == NULL && include_list->items[i+1] != NULL)) {
            contents_match = false;
            break;
        }
    }
    TEST_EXPECT(contents_match, "contents do not match");
  
    fprintf (stderr, "    Checking %s include/exclude: ", check_str);
    TEST_EXPECT(is_exclude == include_list->is_exclude, "list type not correctly identified");
}

static int test_serialized_variable(void)
{
    test_init("serialized MCA variables...");

    unsetenv("OMPI_MCA_test_component_test_include_list");

    fprintf (stderr, "Testing include list MCA variables....\n");

    const char *kProjectName = "openmpi";
    const char *kFrameworkName = "test";
    const char *kComponentName = "component";
    const char *kTestVarName = "test_list";

    OBJ_CONSTRUCT(&include_list_var, opal_include_list_t);
    char *exclude_list = "^foo,baz";
    int ret = include_list_var.super.deserialize(&include_list_var.super, exclude_list);
    if (OPAL_SUCCESS != ret) {
        test_failure("error setting initial value");
        return test_finalize();
    }

    fprintf (stderr, "  Testing include list variable creation: ");

    ret = mca_base_var_register(kProjectName, kFrameworkName, kComponentName, kTestVarName,
                                "String variable for test", MCA_BASE_VAR_TYPE_SERIALIZABLE,
                                /*enumerator=*/NULL, /*bind=*/0, MCA_BASE_VAR_FLAG_SETTABLE,
                                OPAL_INFO_LVL_1, MCA_BASE_VAR_SCOPE_LOCAL, &include_list_var);
    TEST_ASSERT(ret >= 0, "could not create variable");

    int vari = mca_base_var_find(kProjectName, kFrameworkName, kComponentName, kTestVarName);
    TEST_ASSERT(vari >= 0, "variable register but not found");
    TEST_ASSERT(ret == vari, "mca_base_var_register did not return the correct variable index");

    test_pass();

    check_include_list_contents("exclude", exclude_list, &include_list_var);

    fprintf (stderr, "  Testing mca_base_var_set_value: ");
    
    char *include_list = "foo,bar,baz";
    ret = mca_base_var_set_value(vari, include_list, /*size=*/0, MCA_BASE_VAR_SOURCE_SET,
                                 /*source_file=*/NULL);
    ret = test_verify_int(OPAL_SUCCESS, ret);
    if (ret == 1) {
        fprintf (stderr, "PASS\n");
    }

    check_include_list_contents("include", include_list, &include_list_var);

    fprintf (stderr, "  Testing de-registration: ");
    ret = mca_base_var_deregister(vari);
    TEST_ASSERT(OPAL_SUCCESS == ret, "deregistration failed");
    test_pass();

    fprintf (stderr, "  Testing post-deregistration state: ");
    TEST_ASSERT(include_list_var.items == NULL, "deregistration did not nullify the storage");
    test_pass();

    return test_finalize();
}

int main(int argc, char *argv[])
{
    int ret = opal_init_util(&argc, &argv);
    if (OPAL_SUCCESS != ret) {
        fprintf (stderr, "could not initialize opal");
        exit(1);
    }

    test_string_variable();
    test_serialized_variable();

    opal_finalize_util();

    return ret;
}
