/*
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <assert.h>

#include "opal/class/opal_cstring.h"
#include <stdio.h>
#include <stdlib.h>

#define CHECK_INT(actual, expected)                   \
    if (expected != actual) {                         \
        printf("%s:%d: expected %d, got %d\n",        \
               __FILE__, __LINE__, expected, actual); \
        exit(1);                                      \
    }

#define CHECK_BOOL(actual, expected)                  \
    if (expected != actual) {                         \
        printf("%s:%d: expected %s, got %s\n",        \
               __FILE__, __LINE__,                    \
               expected ? "true" : "false",           \
               actual ? "true" : "false");            \
        exit(1);                                      \
    }

int main(int argc, char *argv[])
{
    int ret, int_val;
    bool bool_val;
    opal_cstring_t *cstr;

    /*
     * bool tests
     */
    cstr = opal_cstring_create("true");
    ret = opal_cstring_to_bool(cstr, &bool_val);
    CHECK_INT(ret, OPAL_SUCCESS);
    CHECK_BOOL(bool_val, true);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("yes");
    ret = opal_cstring_to_bool(cstr, &bool_val);
    CHECK_INT(ret, OPAL_SUCCESS);
    CHECK_BOOL(bool_val, true);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("false");
    ret = opal_cstring_to_bool(cstr, &bool_val);
    CHECK_INT(ret, OPAL_SUCCESS);
    CHECK_BOOL(bool_val, false);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("no");
    ret = opal_cstring_to_bool(cstr, &bool_val);
    CHECK_INT(ret, OPAL_SUCCESS);
    CHECK_BOOL(bool_val, false);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("1");
    ret = opal_cstring_to_bool(cstr, &bool_val);
    CHECK_INT(ret, OPAL_SUCCESS);
    CHECK_BOOL(bool_val, true);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("0");
    ret = opal_cstring_to_bool(cstr, &bool_val);
    CHECK_INT(ret, OPAL_SUCCESS);
    CHECK_BOOL(bool_val, false);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("1.0");
    ret = opal_cstring_to_bool(cstr, &bool_val);
    CHECK_INT(ret, OPAL_SUCCESS);
    CHECK_BOOL(bool_val, true);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("0.0");
    ret = opal_cstring_to_bool(cstr, &bool_val);
    CHECK_INT(ret, OPAL_SUCCESS);
    CHECK_BOOL(bool_val, false);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("foobar");
    ret = opal_cstring_to_bool(cstr, &bool_val);
    CHECK_INT(ret, OPAL_ERR_BAD_PARAM);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create(NULL);
    ret = opal_cstring_to_bool(cstr, &bool_val);
    CHECK_INT(ret, OPAL_ERR_BAD_PARAM);
    OBJ_RELEASE(cstr);


    /*
     * bool string tests
     */
    bool_val = opal_str_to_bool("true");
    CHECK_BOOL(bool_val, true);

    bool_val = opal_str_to_bool("yes");
    CHECK_BOOL(bool_val, true);

    bool_val = opal_str_to_bool("false");
    CHECK_BOOL(bool_val, false);

    bool_val = opal_str_to_bool("no");
    CHECK_BOOL(bool_val, false);

    bool_val = opal_str_to_bool("1");
    CHECK_BOOL(bool_val, true);

    bool_val = opal_str_to_bool("0");
    CHECK_BOOL(bool_val, false);

    bool_val = opal_str_to_bool("1.0");
    CHECK_BOOL(bool_val, true);

    bool_val = opal_str_to_bool("0.0");
    CHECK_BOOL(bool_val, false);

    bool_val = opal_str_to_bool("foobar");
    CHECK_BOOL(bool_val, false);

    bool_val = opal_str_to_bool(NULL);
    CHECK_BOOL(bool_val, false);

    /*
     * integer tests
     */
    cstr = opal_cstring_create("1");
    ret = opal_cstring_to_int(cstr, &int_val);
    CHECK_INT(ret, OPAL_SUCCESS);
    CHECK_INT(int_val, 1);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("0");
    ret = opal_cstring_to_int(cstr, &int_val);
    CHECK_INT(ret, OPAL_SUCCESS);
    CHECK_INT(int_val, 0);
    OBJ_RELEASE(cstr);

    /* test intentional overflow cases */
    if (sizeof(long) > sizeof(int)) {
        long input = (long)INT_MAX + 1L;
        char *input_str;

        ret = asprintf(&input_str, "%ld", input);
        if (ret < 0) {
            printf("%s:%d: asprintf()", __FILE__, __LINE__);
            exit(1);
        }

        cstr = opal_cstring_create(input_str);
        ret = opal_cstring_to_int(cstr, &int_val);
        CHECK_INT(ret, OPAL_ERR_BAD_PARAM);
        OBJ_RELEASE(cstr);
        free(input_str);
    }

    /* yes, this may be redundant from above, but always try both
       cases for simplicity */
    if (sizeof(long long) > sizeof(int)) {
        long long input = (long long)INT_MAX + 1L;
        char *input_str;

        ret = asprintf(&input_str, "%lld", input);
        if (ret < 0) {
            printf("%s:%d: asprintf()", __FILE__, __LINE__);
            exit(1);
        }

        cstr = opal_cstring_create(input_str);
        ret = opal_cstring_to_int(cstr, &int_val);
        CHECK_INT(ret, OPAL_ERR_BAD_PARAM);
        OBJ_RELEASE(cstr);
        free(input_str);
    }

    {
        long input = (long)LONG_MAX;
        char *input_str;

        ret = asprintf(&input_str, "%ld0", input);
        if (ret < 0) {
            printf("%s:%d: asprintf()", __FILE__, __LINE__);
            exit(1);
        }

        cstr = opal_cstring_create(input_str);
        ret = opal_cstring_to_int(cstr, &int_val);
        CHECK_INT(ret, OPAL_ERR_BAD_PARAM);
        OBJ_RELEASE(cstr);
        free(input_str);
    }


    cstr = opal_cstring_create("1.0");
    ret = opal_cstring_to_int(cstr, &int_val);
    CHECK_INT(ret, OPAL_ERR_BAD_PARAM);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("0.0");
    ret = opal_cstring_to_int(cstr, &int_val);
    CHECK_INT(ret, OPAL_ERR_BAD_PARAM);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("foobar");
    ret = opal_cstring_to_int(cstr, &int_val);
    CHECK_INT(ret, OPAL_ERR_BAD_PARAM);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create(NULL);
    ret = opal_cstring_to_int(cstr, &int_val);
    CHECK_INT(ret, OPAL_ERR_BAD_PARAM);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("true");
    ret = opal_cstring_to_int(cstr, &int_val);
    CHECK_INT(ret, OPAL_ERR_BAD_PARAM);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("yes");
    ret = opal_cstring_to_int(cstr, &int_val);
    CHECK_INT(ret, OPAL_ERR_BAD_PARAM);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("false");
    ret = opal_cstring_to_int(cstr, &int_val);
    CHECK_INT(ret, OPAL_ERR_BAD_PARAM);
    OBJ_RELEASE(cstr);

    cstr = opal_cstring_create("no");
    ret = opal_cstring_to_int(cstr, &int_val);
    CHECK_INT(ret, OPAL_ERR_BAD_PARAM);
    OBJ_RELEASE(cstr);

    return 0;
}
