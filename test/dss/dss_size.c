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

FILE *test_out;


int main (int argc, char* argv[])
{
    opal_init(&argc, &argv);

    test_out = stderr;

    /* run the tests */

    fprintf(test_out, "executing test1\n");
    if (test1()) {
        fprintf(test_out, "Test1 succeeded\n");
    }
    else {
      fprintf(test_out, "Test1 failed\n");
    }

    fprintf(test_out, "executing test2\n");
    if (test2()) {
        fprintf(test_out, "Test2 succeeded\n");
    }
    else {
      fprintf(test_out, "Test2 failed\n");
    }

    fprintf(test_out, "executing test3\n");
    if (test3()) {
        fprintf(test_out, "Test3 succeeded\n");
    }
    else {
      fprintf(test_out, "Test3 failed\n");
    }

    fprintf(test_out, "executing test4\n");
    if (test4()) {
        fprintf(test_out, "Test4 succeeded\n");
    }
    else {
      fprintf(test_out, "Test4 failed\n");
    }

    fprintf(test_out, "executing test5\n");
    if (test5()) {
        fprintf(test_out, "Test5 succeeded\n");
    }
    else {
      fprintf(test_out, "Test5 failed\n");
    }

    fprintf(test_out, "executing test6\n");
    if (test6()) {
        fprintf(test_out, "Test6 succeeded\n");
    }
    else {
      fprintf(test_out, "Test6 failed\n");
    }

    fprintf(test_out, "executing test7\n");
    if (test7()) {
        fprintf(test_out, "Test7 succeeded\n");
    }
    else {
      fprintf(test_out, "Test7 failed\n");
    }

    fprintf(test_out, "executing test8\n");
    if (test8()) {
        fprintf(test_out, "Test8 succeeded\n");
    }
    else {
      fprintf(test_out, "Test8 failed\n");
    }

    fprintf(test_out, "executing test9\n");
    if (test9()) {
        fprintf(test_out, "Test9 succeeded\n");
    }
    else {
      fprintf(test_out, "opal_dss test9 failed\n");
    }

    fprintf(test_out, "executing test11\n");
    if (test11()) {
        fprintf(test_out, "Test11 succeeded\n");
    }
    else {
      fprintf(test_out, "opal_dss test11 failed\n");
    }

    fprintf(test_out, "executing test12\n");
    if (test12()) {
        fprintf(test_out, "Test12 succeeded\n");
    }
    else {
      fprintf(test_out, "opal_dss test12 failed\n");
    }

    fprintf(test_out, "executing test13\n");
    if (test13()) {
        fprintf(test_out, "Test13 succeeded\n");
    }
    else {
        fprintf(test_out, "opal_dss test13 failed\n");
    }

    fclose(test_out);

    opal_finalize();

    return(0);
}

/*
 * INT8
 */
static bool test1(void)
{
    int8_t v1;
    uint8_t u1;
    size_t s;
    opal_data_type_t type=OPAL_INT8, utype=OPAL_UINT8;

    if (OPAL_SUCCESS != opal_dss.size(&s, &v1, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for signed value\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for signed value with NULL\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, &u1, utype)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(u1)) {
        fprintf(test_out, "opal_dss.size failed for unsigned value\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, utype)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for unsigned value with NULL\n");
        return(false);
    }

    return (true);
}

/*
 * INT16
 */
static bool test2(void)
{
    int16_t v1;
    uint16_t u1;
    opal_data_type_t type=OPAL_INT16, utype=OPAL_UINT16;
    size_t s;

    if (OPAL_SUCCESS != opal_dss.size(&s, &v1, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for signed value\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for signed value with NULL\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, &u1, utype)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(u1)) {
        fprintf(test_out, "opal_dss.size failed for unsigned value\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, utype)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for unsigned value with NULL\n");
        return(false);
    }

    return (true);
}

/*
 * INT32
 */
static bool test3(void)
{
    int32_t v1;
    uint32_t u1;
    opal_data_type_t type=OPAL_INT32, utype=OPAL_UINT32;
    size_t s;

    if (OPAL_SUCCESS != opal_dss.size(&s, &v1, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for signed value\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for signed value with NULL\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, &u1, utype)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(u1)) {
        fprintf(test_out, "opal_dss.size failed for unsigned value\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, utype)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for unsigned value with NULL\n");
        return(false);
    }

    return (true);
}

/*
 * INT64
 */
static bool test4(void)
{
    int64_t v1;
    uint64_t u1;
    opal_data_type_t type=OPAL_INT64, utype=OPAL_UINT64;
    size_t s;

    if (OPAL_SUCCESS != opal_dss.size(&s, &v1, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for signed value\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for signed value with NULL\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, &u1, utype)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(u1)) {
        fprintf(test_out, "opal_dss.size failed for unsigned value\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, utype)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for unsigned value with NULL\n");
        return(false);
    }

    return (true);
}

/*
 * INT
 */
static bool test5(void)
{
    int v1;
    uint u1;
    opal_data_type_t type=OPAL_INT, utype=OPAL_UINT;
    size_t s;

    if (OPAL_SUCCESS != opal_dss.size(&s, &v1, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for signed value\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for signed value with NULL\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, &u1, utype)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(u1)) {
        fprintf(test_out, "opal_dss.size failed for unsigned value\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, utype)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for unsigned value with NULL\n");
        return(false);
    }

    return (true);
}

/*
 * STRING
 */
static bool test6(void)
{
    char *string1="This is a short string";
    size_t s;

    if (OPAL_SUCCESS != opal_dss.size(&s, string1, OPAL_STRING)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != strlen(string1)+1) {
        fprintf(test_out, "opal_dss.size failed for string\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, OPAL_STRING)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(string1)) {
        fprintf(test_out, "opal_dss.size failed for string with NULL\n");
        return(false);
    }

    return (true);
}

/*
 * BOOL
 */
static bool test7(void)
{
    bool v1;
    size_t s;

    if (OPAL_SUCCESS != opal_dss.size(&s, &v1, OPAL_BOOL)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for bool");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, OPAL_BOOL)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for bool with NULL\n");
        return(false);
    }

    return (true);
}


/*
 * SIZE
 */
static bool test8(void)
{
    size_t v1;
    opal_data_type_t type=OPAL_SIZE;
    size_t s;

    if (OPAL_SUCCESS != opal_dss.size(&s, &v1, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for size\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for size with NULL\n");
        return(false);
    }

    return (true);
}

/*
 * PID
 */
static bool test9(void)
{
    pid_t v1;
    opal_data_type_t type=OPAL_PID;
    size_t s;

    if (OPAL_SUCCESS != opal_dss.size(&s, &v1, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for pid\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for pid with NULL\n");
        return(false);
    }

    return (true);
}

/*
 * DATA TYPE
 */
static bool test11(void)
{
    opal_data_type_t v1;
    opal_data_type_t type=OPAL_DATA_TYPE;
    size_t s;

    if (OPAL_SUCCESS != opal_dss.size(&s, &v1, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for data type\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v1)) {
        fprintf(test_out, "opal_dss.size failed for data type with NULL\n");
        return(false);
    }

    return (true);
}

/**
 * OPAL_BYTE_OBJECT
 */

static bool test12(void)
{
    size_t i, ts;
    opal_byte_object_t v2;
    opal_data_type_t type=OPAL_BYTE_OBJECT;
    size_t s;

    v2.size = 20;
    v2.bytes = (uint8_t*)malloc(v2.size);
    for (i=0; i<v2.size; i++) v2.bytes[i] = i;

    ts = sizeof(v2) + 20*sizeof(uint8_t);

    if (OPAL_SUCCESS != opal_dss.size(&s, &v2, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != ts) {
        fprintf(test_out, "opal_dss.size failed for byte object\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v2)) {
        fprintf(test_out, "opal_dss.size failed for byte object with NULL\n");
        return(false);
    }

    return (true);
}


/* OPAL_DATA_VALUE */
static bool test13(void)
{
    int dat2=200;
    opal_dss_value_t v2;
    opal_data_type_t type=OPAL_DATA_VALUE;
    size_t s, ts;

    v2.type = OPAL_INT;
    v2.data = &dat2;

    ts = sizeof(v2) + sizeof(int);

    if (OPAL_SUCCESS != opal_dss.size(&s, &v2, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != ts) {
        fprintf(test_out, "opal_dss.size failed for data value\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_dss.size(&s, NULL, type)) {
        fprintf(test_out, "opal_dss.size returned error\n");
        return(false);
    }
    if (s != sizeof(v2)) {
        fprintf(test_out, "opal_dss.size failed for data value with NULL\n");
        return(false);
    }

    return (true);
}

