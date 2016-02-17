/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "opal/runtime/opal.h"

#include "opal/dss/dss.h"

#define NUM_ITERS 3
#define NUM_ELEMS 10

static bool test1(void);        /* verify different buffer inits */
static bool test2(void);        /* verify int16 */
static bool test3(void);      /* verify int */
static bool test4(void);        /* verify int32 */
static bool test5(void);      /* verify int64 */
static bool test6(void);        /* verify string */
static bool test7(void);        /* verify BOOL */
static bool test8(void);        /* verify OBJECT */
static bool test9(void);        /* verify composite (multiple types and element counts) */
static bool test11(void);        /* verify size_t */
static bool test12(void);        /* verify pid_t */
static bool test13(void);        /* verify pid_t */

static FILE *test_out;


int main (int argc, char* argv[])
{
    int ret = 0;

    opal_init(&argc, &argv);

    test_out = stderr;

    /* run the tests */

    fprintf(test_out, "executing test1\n");
    if (test1()) {
        fprintf(test_out, "Test1 succeeded\n");
    } else {
        fprintf(test_out, "Test1 failed\n");
        ret = 1;
    }

    fprintf(test_out, "executing test2\n");
    if (test2()) {
        fprintf(test_out, "Test2 succeeded\n");
    } else {
        fprintf(test_out, "Test2 failed\n");
        ret = 1;
    }

    fprintf(test_out, "executing test3\n");
    if (test3()) {
        fprintf(test_out, "Test3 succeeded\n");
    } else {
        fprintf(test_out, "Test3 failed\n");
        ret = 3;
    }

    fprintf(test_out, "executing test4\n");
    if (test4()) {
        fprintf(test_out, "Test4 succeeded\n");
    } else {
        fprintf(test_out, "Test4 failed\n");
        ret = 4;
    }

    fprintf(test_out, "executing test5\n");
    if (test5()) {
        fprintf(test_out, "Test5 succeeded\n");
    } else {
        fprintf(test_out, "Test5 failed\n");
        ret = 5;
    }

    fprintf(test_out, "executing test6\n");
    if (test6()) {
        fprintf(test_out, "Test6 succeeded\n");
    } else {
        fprintf(test_out, "Test6 failed\n");
        ret = 6;
    }

    fprintf(test_out, "executing test7\n");
    if (test7()) {
        fprintf(test_out, "Test7 succeeded\n");
    } else {
        fprintf(test_out, "Test7 failed\n");
        ret = 7;
    }

    fprintf(test_out, "executing test8\n");
    if (test8()) {
        fprintf(test_out, "Test8 succeeded\n");
    } else {
        fprintf(test_out, "Test8 failed\n");
        ret = 8;
    }

    fprintf(test_out, "executing test9\n");
    if (test9()) {
        fprintf(test_out, "Test9 succeeded\n");
    } else {
        fprintf(test_out, "opal_dss test9 failed\n");
        ret = 9;
    }

    fprintf(test_out, "executing test11\n");
    if (test11()) {
        fprintf(test_out, "Test11 succeeded\n");
    } else {
        fprintf(test_out, "opal_dss test11 failed\n");
        ret = 11;
    }

    fprintf(test_out, "executing test12\n");
    if (test12()) {
        fprintf(test_out, "Test12 succeeded\n");
    } else {
        fprintf(test_out, "opal_dss test12 failed\n");
        ret = 12;
    }

    fprintf(test_out, "executing test13\n");
    if (test13()) {
        fprintf(test_out, "Test13 succeeded\n");
    } else {
        fprintf(test_out, "opal_dss test13 failed\n");
        ret = 13;
    }

    fclose(test_out);

    opal_finalize();

    return ret;
}

/*
 * INT8
 */
static bool test1(void)
{
    int8_t v2=-100;
    uint8_t u2=150;
    opal_data_type_t type=OPAL_INT8, utype=OPAL_UINT8;
    char *output;

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &v2, type)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed for signed value\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for signed value: %s\n", output);
    free(output);

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &u2, utype)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed for unsigned value\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for unsigned value: %s\n", output);
    free(output);

    return (true);
}

/*
 * INT16
 */
static bool test2(void)
{
    int16_t v2=-100;
    uint16_t u2=150;
    opal_data_type_t type=OPAL_INT16, utype=OPAL_UINT16;
    char *output;

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &v2, type)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed for signed value\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for signed value: %s\n", output);
    free(output);

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &u2, utype)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed for unsigned value\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for unsigned value: %s\n", output);
    free(output);

    return (true);
}

/*
 * INT32
 */
static bool test3(void)
{
    int32_t v2=-100;
    uint32_t u2=150;
    opal_data_type_t type=OPAL_INT32, utype=OPAL_UINT32;
    char *output;

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &v2, type)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed for signed value\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for signed value: %s\n", output);
    free(output);

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &u2, utype)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed for unsigned value\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for unsigned value: %s\n", output);
    free(output);

    return (true);
}

/*
 * INT64
 */
static bool test4(void)
{
    int64_t v2=-100;
    uint64_t u2=150;
    opal_data_type_t type=OPAL_INT64, utype=OPAL_UINT64;
    char *output;

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &v2, type)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed for signed value\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for signed value: %s\n", output);
    free(output);

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &u2, utype)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed for unsigned value\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for unsigned value: %s\n", output);
    free(output);

    return (true);
}

/*
 * INT
 */
static bool test5(void)
{
    int v2=-100;
    uint u2=150;
    opal_data_type_t type=OPAL_INT, utype=OPAL_UINT;
    char *output;

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &v2, type)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed for signed value\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for signed value: %s\n", output);
    free(output);

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &u2, utype)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed for unsigned value\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for unsigned value: %s\n", output);
    free(output);

    return (true);
}

/*
 * STRING
 */
static bool test6(void)
{
    char *string1="This is a short string";
    char *output;

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, string1, OPAL_STRING)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for string: %s\n", output);
    free(output);

    return (true);
}

/*
 * BOOL
 */
static bool test7(void)
{
    bool v2=true;
    char *output;

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &v2, OPAL_BOOL)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for bool: %s\n", output);
    free(output);

    return (true);
}


/*
 * SIZE
 */
static bool test8(void)
{
    size_t v2=100;
    opal_data_type_t type=OPAL_SIZE;
    char *output;

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &v2, type)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for size: %s\n", output);
    free(output);

    return (true);
}

/*
 * PID
 */
static bool test9(void)
{
    pid_t v2=100;
    opal_data_type_t type=OPAL_PID;
    char *output;

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &v2, type)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for pid: %s\n", output);
    free(output);

    return (true);
}

/*
 * DATA TYPE
 */
static bool test11(void)
{
    opal_data_type_t v2=100;
    opal_data_type_t type=OPAL_DATA_TYPE;
    char *output;

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &v2, type)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for data type: %s\n", output);
    free(output);

    return (true);
}

/**
 * OPAL_BYTE_OBJECT
 */

static bool test12(void)
{
    int i;
    opal_byte_object_t v2;
    opal_data_type_t type=OPAL_BYTE_OBJECT;
    char *output;

    v2.size = 20;
    v2.bytes = (uint8_t*)malloc(v2.size);
    for (i=0; i<v2.size; i++) v2.bytes[i] = i;

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &v2, type)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for byte object: %s\n", output);
    free(output);

    return (true);
}


/* OPAL_DATA_VALUE */
static bool test13(void)
{
    int dat2=200;
    opal_value_t v2;
    opal_data_type_t type=OPAL_VALUE;
    char *output;

    v2.type = OPAL_INT;
    v2.data.integer = dat2;
    v2.key = "key";

    if (OPAL_SUCCESS != opal_dss.print(&output, NULL, &v2, type)) {
        fprintf(test_out, "opal_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(test_out, "opal_dss.print failed\n");
        return(false);
    }
    fprintf(test_out, "opal_dss.print output for data value: %s\n", output);
    free(output);

    return (true);
}

