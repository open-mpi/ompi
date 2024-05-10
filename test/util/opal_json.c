/*
 * Copyright (c) 2024      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/util/json/opal_json.h"
#include "opal/runtime/opal.h"
#include "support.h"
#include <stdio.h>
#include <string.h>

enum INPUT_TYPE {
    JSON_STRING,
    JSON_FILE,
    INPUT_TYPE_COUNT,
};

#define FREE_JSON(obj)          \
    do {                        \
        opal_json_free(&(obj)); \
        obj = NULL;             \
    } while (0)

static int load_json(const char *string, enum INPUT_TYPE input_type, const opal_json_t **json)
{
    FILE *fp = NULL;
    char filename[] = "XXXXXXXXXX";
    int ret = 0, fd = -1;

    switch (input_type) {
    case JSON_STRING:
        /* Load the input string */
        ret = opal_json_load(string, strlen(string), json);
        break;
    case JSON_FILE:
        /* Write the input string to a temporary file */
        fd = mkstemp(filename);
        if (fd < 0) {
            test_failure("Failed to create JSON file");
            ret = 1;
            break;
        }
        fp = fdopen(fd, "w");
        if (fp == NULL) {
            test_failure("Failed to open JSON file");
            ret = 1;
            break;
        }
        fputs(string, fp);
        fclose(fp);
        close(fd);
        /* Load the input string from the temporary file */
        ret = opal_json_load_file(filename, json);
        /* Remember to delete the file */
        (void) remove(filename);
        break;
    default:
        test_failure("Unknown input type!");
        ret = 1;
    }
    return ret;
}

static void test_json_integer_val(const opal_json_t *json, int64_t expected_value)
{
    int ret = 0;
    int64_t val;
    ret = opal_json_read_integer(json, &val);
    if (ret) {
        test_failure("Failed to read an integer value");
    } else if (test_verify_int64_t(expected_value, val)) {
        test_success();
    } else {
        test_failure("Got a wrong integer value");
    }
}

static void test_json_double_val(const opal_json_t *json, double expected_value)
{
    int ret = 0;
    double val;
    ret = opal_json_read_double(json, &val);
    if (ret) {
        test_failure("Failed to read a double value");
    } else if (test_verify_double(expected_value, val)) {
        test_success();
    } else {
        test_failure("Got a wrong double value");
    }
}

static void test_json_bool_val(const opal_json_t *json, bool expected_value)
{
    int ret = 0;
    bool val;
    ret = opal_json_read_bool(json, &val);
    if (ret) {
        test_failure("Failed to read a boolean value");
    } else if (test_verify_int(expected_value, val)) {
        test_success();
    } else {
        test_failure("Got a wrong boolean value");
    }
}

static void test_json_string_val(const opal_json_t *json, const char *expected_value)
{
    int ret;
    size_t len;
    char *val = NULL;
    ret = opal_json_read_string(json, (const char **) &val, &len);
    if (ret) {
        test_failure("Failed to read a string value");
    } else if (test_verify_str(expected_value, val) && test_verify_size_t(strlen(val), len)) {
        test_success();
    } else {
        test_failure("Got a wrong string value");
    }
}

static void test_valid_json(void)
{
    int ret = 0;
    size_t size;
    const opal_json_t *json = NULL, *a = NULL, *b = NULL, *b0 = NULL, *b1 = NULL, *c = NULL,
                      *d = NULL;
    /**
     * Human readable form:
     * {
     *   "a": 1,
     *   "b": [
     *     2,
     *     true
     *   ],
     *   "c": "this is a string",
     *   "d": 3.456
     * }
     */
    static const char *s
        = "{\"a\": 1, \"b\": [2, true], \"c\": \"this is a string\", \"d\": 3.456}";

    for (int input_type = JSON_STRING; input_type < INPUT_TYPE_COUNT; ++input_type) {
        ret = load_json(s, input_type, &json);
        if (ret) {
            test_failure("Failed to load JSON");
            continue;
        } else {
            test_success();
        }

        ret = opal_json_get_container_size(json, &size);
        if (ret) {
            test_failure("Failed to get object size");
        } else {
            test_verify_size_t(4, size);
        }

        ret = opal_json_get_key(json, "a", &a);
        if (ret) {
            test_failure("Failed to find a valid key");
        } else {
            test_json_integer_val(a, 1);
        }

        ret = opal_json_get_key(json, "b", &b);
        if (ret) {
            test_failure("Failed to find a valid key");
        } else {
            ret = opal_json_get_container_size(b, &size);
            if (ret) {
                test_failure("Failed to get array length");
            } else {
                test_verify_size_t(2, size);
            }

            ret = opal_json_get_index(b, 0, &b0);
            if (ret) {
                test_failure("Failed to get value at an index");
            } else {
                test_json_integer_val(b0, 2);
            }

            ret = opal_json_get_index(b, 1, &b1);
            if (ret) {
                test_failure("Failed to get value at an index");
            } else {
                test_json_bool_val(b1, true);
            }
        }

        ret = opal_json_get_key(json, "c", &c);
        if (ret) {
            test_failure("Failed to find a valid key");
        } else {
            test_json_string_val(c, "this is a string");
        }

        ret = opal_json_get_key(json, "d", &d);
        if (ret) {
            test_failure("Failed to find a valid key");
        } else {
            test_json_double_val(d, 3.456);
        }

        /* JSON objects can be released in any order */
        FREE_JSON(a);
        FREE_JSON(b);
        FREE_JSON(b0);
        FREE_JSON(b1);
        FREE_JSON(c);
        FREE_JSON(json);
    }
}

static void test_invalid_json_string(void)
{
    int ret = 0;
    static const int cnt = 3;
    char *test_cases[cnt];

    test_cases[0] = "1,2,3";
    test_cases[1] = "[1,2,3,]";
    test_cases[2] = "{a: 1}";

    for (int i = 0; i < 1; ++i) {
        const opal_json_t *json = NULL;
        char *test_case = test_cases[i];
        for (int input_type = JSON_STRING; input_type < INPUT_TYPE_COUNT; ++input_type) {
            ret = load_json(test_case, input_type, &json);
            if (ret) {
                test_success();
            } else {
                test_failure("Failed to return error for an invalid JSON string");
            }
            FREE_JSON(json);
        }
    }
}

int main(int argc, char **argv)
{

    opal_init(&argc, &argv);
    test_init("opal_util_json");
    test_valid_json();
    test_invalid_json_string();
    opal_finalize();
    return test_finalize();
}
