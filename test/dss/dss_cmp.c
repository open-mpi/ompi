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


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

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
      ret = 2;
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
    int8_t v1, v2=100;
    uint8_t u1, u2=150;
    opal_data_type_t type=OPAL_INT8, utype=OPAL_UINT8;

    v1 = v2;
    if (OPAL_EQUAL != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed equality\n");
        return(false);
    }

    v1 = v2 + 1;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed value1 greater\n");
        return(false);
    }

    v1 = v2 - 1;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed value2 greater\n");
        return(false);
    }

    u1 = u2;
    if (OPAL_EQUAL != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned equality\n");
        return(false);
    }

    u1 = u2 + 1;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned value1 greater\n");
        return(false);
    }

    u1 = u2 - 1;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned value2 greater\n");
        return(false);
    }

    return (true);
}

/*
 * INT16
 */
static bool test2(void)
{
    int16_t v1, v2=100;
    uint16_t u1, u2=150;
    opal_data_type_t type=OPAL_INT16, utype=OPAL_UINT16;

    v1 = v2;
    if (OPAL_EQUAL != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed equality\n");
        return(false);
    }

    v1 = v2 + 1;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed value1 greater\n");
        return(false);
    }

    v1 = v2 - 1;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed value2 greater\n");
        return(false);
    }

    u1 = u2;
    if (OPAL_EQUAL != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned equality\n");
        return(false);
    }

    u1 = u2 + 1;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned value1 greater\n");
        return(false);
    }

    u1 = u2 - 1;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned value2 greater\n");
        return(false);
    }

    return (true);
}

/*
 * INT32
 */
static bool test3(void)
{
    int32_t v1, v2=100;
    uint32_t u1, u2=150;
    opal_data_type_t type=OPAL_INT32, utype=OPAL_UINT32;

    v1 = v2;
    if (OPAL_EQUAL != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed equality\n");
        return(false);
    }

    v1 = v2 + 1;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed value1 greater\n");
        return(false);
    }

    v1 = v2 - 1;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed value2 greater\n");
        return(false);
    }

    u1 = u2;
    if (OPAL_EQUAL != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned equality\n");
        return(false);
    }

    u1 = u2 + 1;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned value1 greater\n");
        return(false);
    }

    u1 = u2 - 1;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned value2 greater\n");
        return(false);
    }

    return (true);
}

/*
 * INT64
 */
static bool test4(void)
{
    int64_t v1, v2=100;
    uint64_t u1, u2=150;
    opal_data_type_t type=OPAL_INT64, utype=OPAL_UINT64;

    v1 = v2;
    if (OPAL_EQUAL != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed equality\n");
        return(false);
    }

    v1 = v2 + 1;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed value1 greater\n");
        return(false);
    }

    v1 = v2 - 1;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed value2 greater\n");
        return(false);
    }

    u1 = u2;
    if (OPAL_EQUAL != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned equality\n");
        return(false);
    }

    u1 = u2 + 1;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned value1 greater\n");
        return(false);
    }

    u1 = u2 - 1;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned value2 greater\n");
        return(false);
    }

    return (true);
}

/*
 * INT
 */
static bool test5(void)
{
    int v1, v2=100;
    uint u1, u2=150;
    opal_data_type_t type=OPAL_INT, utype=OPAL_UINT;

    v1 = v2;
    if (OPAL_EQUAL != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed equality\n");
        return(false);
    }

    v1 = v2 + 1;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed value1 greater\n");
        return(false);
    }

    v1 = v2 - 1;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for signed value2 greater\n");
        return(false);
    }

    u1 = u2;
    if (OPAL_EQUAL != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned equality\n");
        return(false);
    }

    u1 = u2 + 1;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned value1 greater\n");
        return(false);
    }

    u1 = u2 - 1;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&u1, &u2, utype)) {
        fprintf(test_out, "opal_dss.compare failed for unsigned value2 greater\n");
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
    char *string2="This is a longer string";

    if (OPAL_EQUAL != opal_dss.compare(string1, string1, OPAL_STRING)) {
        fprintf(test_out, "opal_dss.compare failed for equality\n");
        return(false);
    }

    if (OPAL_VALUE2_GREATER != opal_dss.compare(string1, string2, OPAL_STRING)) {
        fprintf(test_out, "opal_dss.compare failed for value2 greater\n");
        return(false);
    }

    if (OPAL_VALUE1_GREATER != opal_dss.compare(string2, string1, OPAL_STRING)) {
        fprintf(test_out, "opal_dss.compare failed for value1 greater\n");
        return(false);
    }

    return (true);
}

/*
 * BOOL
 */
static bool test7(void)
{
    bool v1, v2=true;

    v1 = v2;
    if (OPAL_EQUAL != opal_dss.compare(&v1, &v2, OPAL_BOOL)) {
        fprintf(test_out, "opal_dss.compare failed for equality\n");
        return(false);
    }

    v1 = true;
    v2 = false;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&v1, &v2, OPAL_BOOL)) {
        fprintf(test_out, "opal_dss.compare failed for value1 greater\n");
        return(false);
    }

    v1 = false;
    v2 = true;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&v1, &v2, OPAL_BOOL)) {
        fprintf(test_out, "opal_dss.compare failed for value2 greater\n");
        return(false);
    }

    return (true);
}


/*
 * SIZE
 */
static bool test8(void)
{
    size_t v1, v2=100;
    opal_data_type_t type=OPAL_SIZE;

    v1 = v2;
    if (OPAL_EQUAL != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for equality\n");
        return(false);
    }

    v1 = v2 + 1;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for value1 greater\n");
        return(false);
    }

    v1 = v2 - 1;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for value2 greater\n");
        return(false);
    }

    return (true);
}

/*
 * PID
 */
static bool test9(void)
{
    pid_t v1, v2=100;
    opal_data_type_t type=OPAL_PID;

    v1 = v2;
    if (OPAL_EQUAL != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for equality\n");
        return(false);
    }

    v1 = v2 + 1;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for value1 greater\n");
        return(false);
    }

    v1 = v2 - 1;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for value2 greater\n");
        return(false);
    }

    return (true);
}

/*
 * DATA TYPE
 */
static bool test11(void)
{
    opal_data_type_t v1, v2=100;
    opal_data_type_t type=OPAL_DATA_TYPE;

    v1 = v2;
    if (OPAL_EQUAL != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for equality\n");
        return(false);
    }

    v1 = v2 + 1;
    if (OPAL_VALUE1_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for value1 greater\n");
        return(false);
    }

    v1 = v2 - 1;
    if (OPAL_VALUE2_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for value2 greater\n");
        return(false);
    }

    return (true);
}

/**
 * OPAL_BYTE_OBJECT
 */

static bool test12(void)
{
    int i;
    opal_byte_object_t v1, v2;
    opal_data_type_t type=OPAL_BYTE_OBJECT;

    v1.size = 20;
    v1.bytes = (uint8_t*)malloc(v1.size);
    for (i=0; i<v1.size; i++) v1.bytes[i] = i;

    v2.size = v1.size;
    v2.bytes = (uint8_t*)malloc(v2.size);
    for (i=0; i<v2.size; i++) v2.bytes[i] = v1.bytes[i];
    v2.bytes[(v2.size)/2] += 1;


    if (OPAL_EQUAL != opal_dss.compare(&v1, &v1, type)) {
        fprintf(test_out, "opal_dss.compare failed for equality\n");
        return(false);
    }

    if (OPAL_VALUE1_GREATER != opal_dss.compare(&v2, &v1, type)) {
        fprintf(test_out, "opal_dss.compare failed for value1 greater\n");
        return(false);
    }

    if (OPAL_VALUE2_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for value2 greater\n");
        return(false);
    }

    return (true);
}

static bool test13(void)
{
    int dat1=100, dat2=200;
    opal_value_t v1, v2;
    opal_data_type_t type=OPAL_VALUE;

    v1.type = OPAL_INT;
    v1.data.integer = dat1;

    v2.type = OPAL_INT;
    v2.data.integer = dat2;

    if (OPAL_EQUAL != opal_dss.compare(&v1, &v1, type)) {
        fprintf(test_out, "opal_dss.compare failed for equality\n");
        return(false);
    }

    if (OPAL_VALUE1_GREATER != opal_dss.compare(&v2, &v1, type)) {
        fprintf(test_out, "opal_dss.compare failed for value1 greater\n");
        return(false);
    }

    if (OPAL_VALUE2_GREATER != opal_dss.compare(&v1, &v2, type)) {
        fprintf(test_out, "opal_dss.compare failed for value2 greater\n");
        return(false);
    }

    return (true);
}

