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

#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"


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
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/dss/dss.h"

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
static bool test10(void);        /* verify KEYVAL */
static bool test11(void);        /* verify size_t */
static bool test12(void);        /* verify pid_t */

FILE *test_out;


int main (int argc, char* argv[])
{
    int ret;

    opal_init();

    /* register handler for errnum -> string converstion */
    opal_error_register("ORTE", ORTE_ERR_BASE, ORTE_ERR_MAX, orte_err2str);

    test_out = stderr;

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_proc_info())) {
        return ret;
    }

    orte_process_info.seed = true;
    orte_process_info.my_name = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
    orte_process_info.my_name->jobid = 0;
    orte_process_info.my_name->vpid = 0;


    /* open the dss */
    if (ORTE_SUCCESS == orte_dss_open()) {
        fprintf(test_out, "DSS started\n");
    } else {
        fprintf(test_out, "DSS could not start\n");
        exit (1);
    }

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
      fprintf(test_out, "orte_dss test9 failed\n");
    }

    fprintf(test_out, "executing test10\n");
    if (test10()) {
        fprintf(test_out, "Test10 succeeded\n");
    }
    else {
      fprintf(test_out, "orte_dss test10 failed\n");
    }

    fprintf(test_out, "executing test11\n");
    if (test11()) {
        fprintf(test_out, "Test11 succeeded\n");
    }
    else {
      fprintf(test_out, "orte_dss test11 failed\n");
    }

    fprintf(test_out, "executing test12\n");
    if (test12()) {
        fprintf(test_out, "Test12 succeeded\n");
    }
    else {
      fprintf(test_out, "orte_dss test12 failed\n");
    }

    fclose(test_out);

    orte_dss_close();

    opal_finalize();

    return(0);
}

/*
 * INT8
 */
static bool test1(void)
{
    int8_t v1, v2=100;
    uint8_t u1, u2=150;
    orte_data_type_t type=ORTE_INT8, utype=ORTE_UINT8;
    orte_data_value_t dv;

    dv.type = type;
    dv.data = &v2;
    v1 = v2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (v2 != v1+1) {
        fprintf(test_out, "orte_dss.increment failed for signed value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (v2 != v1) {
        fprintf(test_out, "orte_dss.decrement failed for signed value\n");
        return(false);
    }


    dv.type = utype;
    dv.data = &u2;
    u1 = u2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (u2 != u1+1) {
        fprintf(test_out, "orte_dss.increment failed for unsigned value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (u2 != u1) {
        fprintf(test_out, "orte_dss.decrement failed for unsigned value\n");
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
    orte_data_type_t type=ORTE_INT16, utype=ORTE_UINT16;
    orte_data_value_t dv;

    dv.type = type;
    dv.data = &v2;
    v1 = v2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (v2 != v1+1) {
        fprintf(test_out, "orte_dss.increment failed for signed value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (v2 != v1) {
        fprintf(test_out, "orte_dss.decrement failed for signed value\n");
        return(false);
    }


    dv.type = utype;
    dv.data = &u2;
    u1 = u2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (u2 != u1+1) {
        fprintf(test_out, "orte_dss.increment failed for unsigned value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (u2 != u1) {
        fprintf(test_out, "orte_dss.decrement failed for unsigned value\n");
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
    orte_data_type_t type=ORTE_INT32, utype=ORTE_UINT32;
    orte_data_value_t dv;

    dv.type = type;
    dv.data = &v2;
    v1 = v2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (v2 != v1+1) {
        fprintf(test_out, "orte_dss.increment failed for signed value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (v2 != v1) {
        fprintf(test_out, "orte_dss.decrement failed for signed value\n");
        return(false);
    }


    dv.type = utype;
    dv.data = &u2;
    u1 = u2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (u2 != u1+1) {
        fprintf(test_out, "orte_dss.increment failed for unsigned value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (u2 != u1) {
        fprintf(test_out, "orte_dss.decrement failed for unsigned value\n");
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
    orte_data_type_t type=ORTE_INT64, utype=ORTE_UINT64;
    orte_data_value_t dv;

    dv.type = type;
    dv.data = &v2;
    v1 = v2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (v2 != v1+1) {
        fprintf(test_out, "orte_dss.increment failed for signed value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (v2 != v1) {
        fprintf(test_out, "orte_dss.decrement failed for signed value\n");
        return(false);
    }


    dv.type = utype;
    dv.data = &u2;
    u1 = u2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (u2 != u1+1) {
        fprintf(test_out, "orte_dss.increment failed for unsigned value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (u2 != u1) {
        fprintf(test_out, "orte_dss.decrement failed for unsigned value\n");
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
    orte_data_type_t type=ORTE_INT, utype=ORTE_UINT;
    orte_data_value_t dv;

    dv.type = type;
    dv.data = &v2;
    v1 = v2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (v2 != v1+1) {
        fprintf(test_out, "orte_dss.increment failed for signed value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (v2 != v1) {
        fprintf(test_out, "orte_dss.decrement failed for signed value\n");
        return(false);
    }


    dv.type = utype;
    dv.data = &u2;
    u1 = u2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (u2 != u1+1) {
        fprintf(test_out, "orte_dss.increment failed for unsigned value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (u2 != u1) {
        fprintf(test_out, "orte_dss.decrement failed for unsigned value\n");
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
    orte_data_value_t dv;

    dv.type = ORTE_STRING;
    dv.data = string1;

    if (ORTE_ERR_OPERATION_UNSUPPORTED != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment failed to return correct error\n");
        return(false);
    }

    return (true);
}

/*
 * BOOL
 */
static bool test7(void)
{
    bool v2=true;
    orte_data_value_t dv;

    dv.type = ORTE_BOOL;
    dv.data = &v2;

    if (ORTE_ERR_OPERATION_UNSUPPORTED != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment failed to return correct error\n");
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
    orte_data_type_t type=ORTE_SIZE;
    orte_data_value_t dv;

    dv.type = type;
    dv.data = &v2;
    v1 = v2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (v2 != v1+1) {
        fprintf(test_out, "orte_dss.increment failed for size value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (v2 != v1) {
        fprintf(test_out, "orte_dss.decrement failed for size value\n");
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
    orte_data_type_t type=ORTE_PID;
    orte_data_value_t dv;

    dv.type = type;
    dv.data = &v2;
    v1 = v2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (v2 != v1+1) {
        fprintf(test_out, "orte_dss.increment failed for pid value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (v2 != v1) {
        fprintf(test_out, "orte_dss.decrement failed for pid value\n");
        return(false);
    }

    return (true);
}

/*
 * DAEMON CMD
 */
static bool test10(void)
{
    orte_daemon_cmd_flag_t v1, v2=100;
    orte_data_type_t type=ORTE_DAEMON_CMD;
    orte_data_value_t dv;

    dv.type = type;
    dv.data = &v2;
    v1 = v2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (v2 != v1+1) {
        fprintf(test_out, "orte_dss.increment failed for daemon cmd value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (v2 != v1) {
        fprintf(test_out, "orte_dss.decrement failed for daemon cmd value\n");
        return(false);
    }

    return (true);
}

/*
 * DATA TYPE
 */
static bool test11(void)
{
    orte_data_type_t v1, v2=100;
    orte_data_type_t type=ORTE_DATA_TYPE;
    orte_data_value_t dv;

    dv.type = type;
    dv.data = &v2;
    v1 = v2;

    if (ORTE_SUCCESS != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment returned error\n");
        return(false);
    }
    if (v2 != v1+1) {
        fprintf(test_out, "orte_dss.increment failed for data type value\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.decrement(&dv)) {
        fprintf(test_out, "orte_dss.decrement returned error\n");
        return(false);
    }
    if (v2 != v1) {
        fprintf(test_out, "orte_dss.decrement failed for data type value\n");
        return(false);
    }

    return (true);
}

/**
 * ORTE_BYTE_OBJECT
 */

static bool test12(void)
{
    size_t i;
    orte_byte_object_t v2;
    orte_data_value_t dv;

    v2.size = 20;
    v2.bytes = (uint8_t*)malloc(v2.size);
    for (i=0; i<v2.size; i++) v2.bytes[i] = i;

    dv.type = ORTE_BYTE_OBJECT;
    dv.data = &v2;

    if (ORTE_ERR_OPERATION_UNSUPPORTED != orte_dss.increment(&dv)) {
        fprintf(test_out, "orte_dss.increment failed to return correct error\n");
        return(false);
    }

    return (true);
}
